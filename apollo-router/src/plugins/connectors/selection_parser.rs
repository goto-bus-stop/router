use serde_json::json;
use serde_json::Map;
use serde_json::Value as JSON;

use nom::branch::alt;
use nom::character::complete::char;
use nom::character::complete::multispace0;
use nom::character::complete::one_of;
use nom::combinator::map;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;

// Define a selection!("...") macro for convenience
macro_rules! selection {
    ($input:expr) => {
        Selection::parse($input).unwrap().1
    };
}

// Selection ::= NamedSelection+ | PathSelection

#[derive(Debug, PartialEq, Clone)]
pub(self) enum Selection {
    // Although we reuse the SubSelection type for the Selection::Named case, we
    // parse it as a sequence of NamedSelection items without the {...} curly
    // braces that SubSelection::parse expects.
    Named(SubSelection),
    Path(PathSelection),
}

impl Selection {
    fn parse<'a>(input: &'a str) -> IResult<&str, Self> {
        alt((
            map(many1(NamedSelection::parse), |selections| {
                Self::Named(SubSelection { selections })
            }),
            map(PathSelection::parse, Self::Path),
        ))(input)
    }
}

#[test]
fn test_selection() {
    assert_eq!(
        Selection::parse("hello"),
        Ok((
            "",
            Selection::Named(SubSelection {
                selections: vec![NamedSelection::Field(None, "hello".to_string(), None),]
            }),
        )),
    );

    assert_eq!(
        Selection::parse(".hello"),
        Ok((
            "",
            Selection::Path(PathSelection::from_slice(
                &[Property::Field("hello".to_string()),],
                None
            )),
        )),
    );

    assert_eq!(
        Selection::parse("hi: .hello.world"),
        Ok((
            "",
            Selection::Named(SubSelection {
                selections: vec![NamedSelection::Path(
                    Alias {
                        name: "hi".to_string(),
                    },
                    PathSelection::from_slice(
                        &[
                            Property::Field("hello".to_string()),
                            Property::Field("world".to_string()),
                        ],
                        None
                    ),
                )]
            }),
        )),
    );

    assert_eq!(
        Selection::parse("before hi: .hello.world after"),
        Ok((
            "",
            Selection::Named(SubSelection {
                selections: vec![
                    NamedSelection::Field(None, "before".to_string(), None),
                    NamedSelection::Path(
                        Alias {
                            name: "hi".to_string(),
                        },
                        PathSelection::from_slice(
                            &[
                                Property::Field("hello".to_string()),
                                Property::Field("world".to_string()),
                            ],
                            None
                        ),
                    ),
                    NamedSelection::Field(None, "after".to_string(), None),
                ]
            }),
        )),
    );

    let before_path_nested_after_result = Ok((
        "",
        Selection::Named(SubSelection {
            selections: vec![
                NamedSelection::Field(None, "before".to_string(), None),
                NamedSelection::Path(
                    Alias {
                        name: "hi".to_string(),
                    },
                    PathSelection::from_slice(
                        &[
                            Property::Field("hello".to_string()),
                            Property::Field("world".to_string()),
                        ],
                        Some(SubSelection {
                            selections: vec![
                                NamedSelection::Field(None, "nested".to_string(), None),
                                NamedSelection::Field(None, "names".to_string(), None),
                            ],
                        }),
                    ),
                ),
                NamedSelection::Field(None, "after".to_string(), None),
            ],
        }),
    ));

    assert_eq!(
        Selection::parse("before hi: .hello.world { nested names } after"),
        before_path_nested_after_result,
    );

    assert_eq!(
        Selection::parse("before hi:.hello.world{nested names}after"),
        before_path_nested_after_result,
    );

    assert_eq!(
        Selection::parse(
            "
            topLevelAlias: topLevelField {
                nonIdentifier: 'property name with spaces'
                pathSelection: .some.nested.path {
                    still: yet
                    more
                    properties
                }
                siblingGroup: { brother sister }
            }
        "
        ),
        Ok((
            "",
            Selection::Named(SubSelection {
                selections: vec![NamedSelection::Field(
                    Some(Alias {
                        name: "topLevelAlias".to_string(),
                    }),
                    "topLevelField".to_string(),
                    Some(SubSelection {
                        selections: vec![
                            NamedSelection::Quoted(
                                Alias {
                                    name: "nonIdentifier".to_string(),
                                },
                                "property name with spaces".to_string(),
                                None,
                            ),
                            NamedSelection::Path(
                                Alias {
                                    name: "pathSelection".to_string(),
                                },
                                PathSelection::from_slice(
                                    &[
                                        Property::Field("some".to_string()),
                                        Property::Field("nested".to_string()),
                                        Property::Field("path".to_string()),
                                    ],
                                    Some(SubSelection {
                                        selections: vec![
                                            NamedSelection::Field(
                                                Some(Alias {
                                                    name: "still".to_string(),
                                                }),
                                                "yet".to_string(),
                                                None,
                                            ),
                                            NamedSelection::Field(None, "more".to_string(), None,),
                                            NamedSelection::Field(
                                                None,
                                                "properties".to_string(),
                                                None,
                                            ),
                                        ],
                                    })
                                ),
                            ),
                            NamedSelection::Group(
                                Alias {
                                    name: "siblingGroup".to_string(),
                                },
                                SubSelection {
                                    selections: vec![
                                        NamedSelection::Field(None, "brother".to_string(), None,),
                                        NamedSelection::Field(None, "sister".to_string(), None,),
                                    ],
                                },
                            ),
                        ],
                    }),
                ),]
            }),
        )),
    );
}

// NamedSelection ::=
//     | Alias? Identifier SubSelection?
//     | Alias StringLiteral SubSelection?
//     | Alias PathSelection
//     | Alias SubSelection

#[derive(Debug, PartialEq, Clone)]
pub(self) enum NamedSelection {
    Field(Option<Alias>, String, Option<SubSelection>),
    Quoted(Alias, String, Option<SubSelection>),
    Path(Alias, PathSelection),
    Group(Alias, SubSelection),
}

impl NamedSelection {
    fn parse<'a>(input: &'a str) -> IResult<&str, Self> {
        alt((
            Self::parse_field,
            Self::parse_quoted,
            Self::parse_path,
            Self::parse_group,
        ))(input)
    }

    fn parse_field<'a>(input: &'a str) -> IResult<&str, Self> {
        tuple((
            opt(Alias::parse),
            parse_identifier,
            opt(SubSelection::parse),
        ))(input)
        .map(|(input, (alias, name, selection))| (input, Self::Field(alias, name, selection)))
    }

    fn parse_quoted<'a>(input: &'a str) -> IResult<&str, Self> {
        tuple((Alias::parse, parse_string_literal, opt(SubSelection::parse)))(input)
            .map(|(input, (alias, name, selection))| (input, Self::Quoted(alias, name, selection)))
    }

    fn parse_path<'a>(input: &'a str) -> IResult<&str, Self> {
        tuple((Alias::parse, PathSelection::parse))(input)
            .map(|(input, (alias, path))| (input, Self::Path(alias, path)))
    }

    fn parse_group<'a>(input: &'a str) -> IResult<&str, Self> {
        tuple((Alias::parse, SubSelection::parse))(input)
            .map(|(input, (alias, group))| (input, Self::Group(alias, group)))
    }

    fn name(&self) -> &String {
        match self {
            Self::Field(alias, name, _) => {
                if let Some(alias) = alias {
                    &alias.name
                } else {
                    name
                }
            }
            Self::Quoted(alias, _, _) => &alias.name,
            Self::Path(alias, _) => &alias.name,
            Self::Group(alias, _) => &alias.name,
        }
    }
}

#[test]
fn test_named_selection() {
    fn assert_result_and_name(input: &str, expected: NamedSelection, name: &str) {
        let actual = NamedSelection::parse(input);
        assert_eq!(actual, Ok(("", expected.clone())));
        assert_eq!(actual.unwrap().1.name(), name);
        assert_eq!(
            Selection::parse(input),
            Ok((
                "",
                Selection::Named(SubSelection {
                    selections: vec![expected],
                })
            ))
        );
    }

    assert_result_and_name(
        "hello",
        NamedSelection::Field(None, "hello".to_string(), None),
        "hello",
    );

    assert_result_and_name(
        "hello { world }",
        NamedSelection::Field(
            None,
            "hello".to_string(),
            Some(SubSelection {
                selections: vec![NamedSelection::Field(None, "world".to_string(), None)],
            }),
        ),
        "hello",
    );

    assert_result_and_name(
        "hi: hello",
        NamedSelection::Field(
            Some(Alias {
                name: "hi".to_string(),
            }),
            "hello".to_string(),
            None,
        ),
        "hi",
    );

    assert_result_and_name(
        "hi: 'hello world'",
        NamedSelection::Quoted(
            Alias {
                name: "hi".to_string(),
            },
            "hello world".to_string(),
            None,
        ),
        "hi",
    );

    assert_result_and_name(
        "hi: hello { world }",
        NamedSelection::Field(
            Some(Alias {
                name: "hi".to_string(),
            }),
            "hello".to_string(),
            Some(SubSelection {
                selections: vec![NamedSelection::Field(None, "world".to_string(), None)],
            }),
        ),
        "hi",
    );

    assert_result_and_name(
        "hey: hello { world again }",
        NamedSelection::Field(
            Some(Alias {
                name: "hey".to_string(),
            }),
            "hello".to_string(),
            Some(SubSelection {
                selections: vec![
                    NamedSelection::Field(None, "world".to_string(), None),
                    NamedSelection::Field(None, "again".to_string(), None),
                ],
            }),
        ),
        "hey",
    );

    assert_result_and_name(
        "hey: 'hello world' { again }",
        NamedSelection::Quoted(
            Alias {
                name: "hey".to_string(),
            },
            "hello world".to_string(),
            Some(SubSelection {
                selections: vec![NamedSelection::Field(None, "again".to_string(), None)],
            }),
        ),
        "hey",
    );

    assert_result_and_name(
        "leggo: 'my ego'",
        NamedSelection::Quoted(
            Alias {
                name: "leggo".to_string(),
            },
            "my ego".to_string(),
            None,
        ),
        "leggo",
    );
}

// PathSelection ::= ("." Property)+ SubSelection?

#[derive(Debug, PartialEq, Clone)]
pub(self) enum PathSelection {
    // We use a recursive structure here instead of a Vec<Property> to make
    // applying the selection to a JSON value easier.
    Path(Property, Box<PathSelection>),
    Selection(SubSelection),
    Empty,
}

impl PathSelection {
    fn parse<'a>(input: &'a str) -> IResult<&str, Self> {
        tuple((
            multispace0,
            many1(preceded(char('.'), Property::parse)),
            opt(SubSelection::parse),
        ))(input)
        .map(|(input, (_, path, selection))| (input, Self::from_slice(&path, selection)))
    }

    fn from_slice(properties: &[Property], selection: Option<SubSelection>) -> Self {
        match properties {
            [] => selection.map_or(Self::Empty, Self::Selection),
            [head, tail @ ..] => {
                Self::Path(head.clone(), Box::new(Self::from_slice(tail, selection)))
            }
        }
    }
}

#[test]
fn test_path_selection() {
    fn check_path_selection(input: &str, expected: PathSelection) {
        assert_eq!(PathSelection::parse(input), Ok(("", expected.clone())));
        assert_eq!(
            Selection::parse(input),
            Ok(("", Selection::Path(expected.clone())))
        );
    }

    check_path_selection(
        ".hello",
        PathSelection::from_slice(&[Property::Field("hello".to_string())], None),
    );

    check_path_selection(
        ".hello.world",
        PathSelection::from_slice(
            &[
                Property::Field("hello".to_string()),
                Property::Field("world".to_string()),
            ],
            None,
        ),
    );

    check_path_selection(
        ".hello.world { hello }",
        PathSelection::from_slice(
            &[
                Property::Field("hello".to_string()),
                Property::Field("world".to_string()),
            ],
            Some(SubSelection {
                selections: vec![NamedSelection::Field(None, "hello".to_string(), None)],
            }),
        ),
    );

    check_path_selection(
        ".nested.'string literal'.\"property\".name",
        PathSelection::from_slice(
            &[
                Property::Field("nested".to_string()),
                Property::Quoted("string literal".to_string()),
                Property::Quoted("property".to_string()),
                Property::Field("name".to_string()),
            ],
            None,
        ),
    );

    check_path_selection(
        ".nested.'string literal' { leggo: 'my ego' }",
        PathSelection::from_slice(
            &[
                Property::Field("nested".to_string()),
                Property::Quoted("string literal".to_string()),
            ],
            Some(SubSelection {
                selections: vec![NamedSelection::Quoted(
                    Alias {
                        name: "leggo".to_string(),
                    },
                    "my ego".to_string(),
                    None,
                )],
            }),
        ),
    );
}

// SubSelection ::= "{" NamedSelection+ "}"

#[derive(Debug, PartialEq, Clone)]
pub(self) struct SubSelection {
    selections: Vec<NamedSelection>,
}

impl SubSelection {
    fn parse<'a>(input: &'a str) -> IResult<&str, Self> {
        tuple((
            multispace0,
            char('{'),
            many1(NamedSelection::parse),
            char('}'),
            multispace0,
        ))(input)
        .map(|(input, (_, _, selections, _, _))| (input, Self { selections }))
    }
}

#[test]
fn test_subselection() {
    assert_eq!(
        SubSelection::parse("{hello}"),
        Ok((
            "",
            SubSelection {
                selections: vec![NamedSelection::Field(None, "hello".to_string(), None),],
            },
        )),
    );

    assert_eq!(
        SubSelection::parse("{ hello }"),
        Ok((
            "",
            SubSelection {
                selections: vec![NamedSelection::Field(None, "hello".to_string(), None),],
            },
        )),
    );

    assert_eq!(
        SubSelection::parse("  { padded  } "),
        Ok((
            "",
            SubSelection {
                selections: vec![NamedSelection::Field(None, "padded".to_string(), None),],
            },
        )),
    );

    assert_eq!(
        SubSelection::parse("{ hello world }"),
        Ok((
            "",
            SubSelection {
                selections: vec![
                    NamedSelection::Field(None, "hello".to_string(), None),
                    NamedSelection::Field(None, "world".to_string(), None),
                ],
            },
        )),
    );

    assert_eq!(
        SubSelection::parse("{ hello { world } }"),
        Ok((
            "",
            SubSelection {
                selections: vec![NamedSelection::Field(
                    None,
                    "hello".to_string(),
                    Some(SubSelection {
                        selections: vec![NamedSelection::Field(None, "world".to_string(), None),],
                    })
                ),],
            },
        )),
    );
}

// Alias ::= Identifier ":"

#[derive(Debug, PartialEq, Clone)]
pub(self) struct Alias {
    name: String,
}

impl Alias {
    fn parse<'a>(input: &'a str) -> IResult<&'a str, Self> {
        tuple((parse_identifier, char(':'), multispace0))(input)
            .map(|(input, (name, _, _))| (input, Self { name }))
    }
}

#[test]
fn test_alias() {
    assert_eq!(
        Alias::parse("hello:"),
        Ok((
            "",
            Alias {
                name: "hello".to_string(),
            },
        )),
    );

    assert_eq!(
        Alias::parse("hello :"),
        Ok((
            "",
            Alias {
                name: "hello".to_string(),
            },
        )),
    );

    assert_eq!(
        Alias::parse("hello : "),
        Ok((
            "",
            Alias {
                name: "hello".to_string(),
            },
        )),
    );

    assert_eq!(
        Alias::parse("  hello :"),
        Ok((
            "",
            Alias {
                name: "hello".to_string(),
            },
        )),
    );

    assert_eq!(
        Alias::parse("hello: "),
        Ok((
            "",
            Alias {
                name: "hello".to_string(),
            },
        )),
    );
}

// Property ::= Identifier | StringLiteral

#[derive(Debug, PartialEq, Clone)]
pub(self) enum Property {
    Field(String),
    Quoted(String),
    Index(usize),
}

impl Property {
    fn parse<'a>(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(parse_identifier, Self::Field),
            map(parse_string_literal, Self::Quoted),
        ))(input)
    }
}

#[test]
fn test_property() {
    assert_eq!(
        Property::parse("hello"),
        Ok(("", Property::Field("hello".to_string()))),
    );

    assert_eq!(
        Property::parse("'hello'"),
        Ok(("", Property::Quoted("hello".to_string()))),
    );
}

// Identifier ::= [a-zA-Z_][0-9a-zA-Z_]*

fn parse_identifier<'a>(input: &'a str) -> IResult<&'a str, String> {
    tuple((
        multispace0,
        recognize(pair(
            one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"),
            many0(one_of(
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789",
            )),
        )),
        multispace0,
    ))(input)
    .map(|(input, (_, name, _))| (input, name.to_string()))
}

#[test]
fn test_identifier() {
    assert_eq!(parse_identifier("hello"), Ok(("", "hello".to_string())),);

    assert_eq!(
        parse_identifier("hello_world"),
        Ok(("", "hello_world".to_string())),
    );

    assert_eq!(
        parse_identifier("hello_world_123"),
        Ok(("", "hello_world_123".to_string())),
    );

    assert_eq!(parse_identifier(" hello "), Ok(("", "hello".to_string())),);
}

// StringLiteral ::=
//     | "'" ("\'" | [^'])* "'"
//     | '"' ('\"' | [^"])* '"'

fn parse_string_literal<'a>(input: &'a str) -> IResult<&'a str, String> {
    let input = multispace0(input).map(|(input, _)| input)?;
    let mut input_char_indices = input.char_indices();

    match input_char_indices.next() {
        Some((0, quote @ '\'')) | Some((0, quote @ '"')) => {
            let mut escape_next = false;
            let mut chars: Vec<char> = vec![];
            let mut remainder: Option<&str> = None;

            for (i, c) in input_char_indices {
                if escape_next {
                    match c {
                        'n' => chars.push('\n'),
                        _ => chars.push(c),
                    }
                    escape_next = false;
                    continue;
                }
                if c == '\\' {
                    escape_next = true;
                    continue;
                }
                if c == quote {
                    remainder = Some(multispace0(&input[i + 1..])?.0);
                    break;
                }
                chars.push(c);
            }

            if let Some(remainder) = remainder {
                Ok((remainder, chars.iter().collect::<String>()))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Eof,
                )))
            }
        }

        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::IsNot,
        ))),
    }
}

#[test]
fn test_string_literal() {
    assert_eq!(
        parse_string_literal("'hello world'"),
        Ok(("", "hello world".to_string())),
    );
    assert_eq!(
        parse_string_literal("\"hello world\""),
        Ok(("", "hello world".to_string())),
    );
    assert_eq!(
        parse_string_literal("'hello \"world\"'"),
        Ok(("", "hello \"world\"".to_string())),
    );
    assert_eq!(
        parse_string_literal("\"hello \\\"world\\\"\""),
        Ok(("", "hello \"world\"".to_string())),
    );
    assert_eq!(
        parse_string_literal("'hello \\'world\\''"),
        Ok(("", "hello 'world'".to_string())),
    );
}

/// ApplyTo is a trait for applying a Selection to a JSON value, collecting
/// any/all errors encountered in the process.

trait ApplyTo {
    // Applying a selection to a JSON value produces a new JSON value, along
    // with any/all errors encountered in the process. The value is represented
    // as an Option to allow for undefined/missing values (which JSON does not
    // explicitly support), which are distinct from null values (which it does
    // support).
    fn apply_to(&self, data: &JSON) -> (Option<JSON>, Vec<ApplyToError>) {
        let mut input_path = vec![];
        self.apply_to_path(data, &mut input_path)
    }

    // This is the trait method that should be implemented and called
    // recursively by the various Selection types.
    fn apply_to_path(
        &self,
        data: &JSON,
        input_path: &mut Vec<Property>,
    ) -> (Option<JSON>, Vec<ApplyToError>);

    // When array is encountered, the Self selection will be applied to each
    // element of the array, producing a new array.
    fn apply_to_array(
        &self,
        data: &JSON,
        input_path: &mut Vec<Property>,
    ) -> (Option<JSON>, Vec<ApplyToError>) {
        let data_array = data.as_array().unwrap();
        let mut output = Vec::with_capacity(data_array.len());
        let mut errors = vec![];

        for (i, element) in data_array.iter().enumerate() {
            input_path.push(Property::Index(i));
            let (value, mut element_errors) = self.apply_to_path(element, input_path);
            input_path.pop();
            // When building an Object, we can simply omit missing properties
            // and report an error, but when building an Array, we need to
            // insert null values to preserve the original array indices/length.
            output.push(value.unwrap_or(JSON::Null));
            errors.append(&mut element_errors);
        }

        (Some(JSON::Array(output)), errors)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct ApplyToError(JSON);

impl ApplyToError {
    fn new(message: &str, path: &Vec<Property>) -> Self {
        Self(json!({
            "message": message,
            "path": path.iter().map(|property| match property {
                Property::Field(name) => json!(name),
                Property::Quoted(name) => json!(name),
                Property::Index(index) => json!(index),
            }).collect::<Vec<JSON>>(),
        }))
    }

    fn from_json(json: &JSON) -> Self {
        if let JSON::Object(error) = json {
            if let Some(JSON::String(_)) = error.get("message") {
                if let Some(JSON::Array(path)) = error.get("path") {
                    if path.iter().all(|element| match element {
                        JSON::String(_) | JSON::Number(_) => true,
                        _ => false,
                    }) {
                        return Self(json.clone());
                    }
                }
            }
        }
        panic!("invalid ApplyToError JSON: {:?}", json);
    }
}

impl ApplyTo for Selection {
    fn apply_to_path(
        &self,
        data: &JSON,
        input_path: &mut Vec<Property>,
    ) -> (Option<JSON>, Vec<ApplyToError>) {
        if data.is_array() {
            return self.apply_to_array(data, input_path);
        }

        if !data.is_object() {
            return (None, vec![ApplyToError::new("not an object", input_path)]);
        }

        match self {
            // Because we represent a Selection::Named as a SubSelection, we can
            // fully delegate apply_to_path to SubSelection::apply_to_path. Even
            // if we represented Self::Named as a Vec<NamedSelection>, we could
            // still delegate to SubSelection::apply_to_path, but we would need
            // to create a temporary SubSelection to wrap the selections Vec.
            Self::Named(named_selections) => named_selections.apply_to_path(data, input_path),
            Self::Path(path_selection) => path_selection.apply_to_path(data, input_path),
        }
    }
}

impl ApplyTo for NamedSelection {
    fn apply_to_path(
        &self,
        data: &JSON,
        input_path: &mut Vec<Property>,
    ) -> (Option<JSON>, Vec<ApplyToError>) {
        if data.is_array() {
            return self.apply_to_array(data, input_path);
        }

        if !data.is_object() {
            return (None, vec![ApplyToError::new("not an object", input_path)]);
        }

        let mut output = Map::new();
        let mut errors = vec![];

        match self {
            Self::Field(alias, name, selection) => {
                input_path.push(Property::Field(name.clone()));
                if let Some(child) = data.get(name) {
                    let output_name = alias.as_ref().map_or(name, |alias| &alias.name);
                    if let Some(selection) = selection {
                        let (value, mut selection_errors) =
                            selection.apply_to_path(child, input_path);
                        if let Some(value) = value {
                            output.insert(output_name.clone(), value);
                        }
                        errors.append(&mut selection_errors);
                    } else {
                        output.insert(output_name.clone(), child.clone());
                    }
                } else {
                    errors.push(ApplyToError::new(
                        format!("{:?} not found", name).as_str(),
                        input_path,
                    ));
                }
                input_path.pop();
                (Some(JSON::Object(output)), errors)
            }
            Self::Quoted(alias, name, selection) => {
                input_path.push(Property::Quoted(name.clone()));
                if let Some(child) = data.get(name) {
                    let output_name = &alias.name;
                    if let Some(selection) = selection {
                        let (value, mut selection_errors) =
                            selection.apply_to_path(child, input_path);
                        if let Some(value) = value {
                            output.insert(output_name.clone(), value);
                        }
                        errors.append(&mut selection_errors);
                    } else {
                        output.insert(output_name.clone(), child.clone());
                    }
                } else {
                    errors.push(ApplyToError::new(
                        format!("{:?} not found", name).as_str(),
                        input_path,
                    ));
                }
                input_path.pop();
                (Some(JSON::Object(output)), errors)
            }
            Self::Path(alias, path_selection) => {
                let (value, path_selection_errors) = path_selection.apply_to_path(data, input_path);
                if let Some(value) = value {
                    output.insert(alias.name.clone(), value);
                }
                (Some(JSON::Object(output)), path_selection_errors)
            }
            Self::Group(alias, sub_selection) => {
                let (value, sub_selection_errors) = sub_selection.apply_to_path(data, input_path);
                if let Some(value) = value {
                    output.insert(alias.name.clone(), value);
                }
                (Some(JSON::Object(output)), sub_selection_errors)
            }
        }
    }
}

impl ApplyTo for PathSelection {
    fn apply_to_path(
        &self,
        data: &JSON,
        input_path: &mut Vec<Property>,
    ) -> (Option<JSON>, Vec<ApplyToError>) {
        if data.is_array() {
            return self.apply_to_array(data, input_path);
        }

        match self {
            Self::Path(head, tail) => {
                if !data.is_object() {
                    return (None, vec![ApplyToError::new("not an object", input_path)]);
                }

                input_path.push(head.clone());
                if let Some(child) = match head {
                    Property::Field(name) => data.get(name),
                    Property::Quoted(name) => data.get(name),
                    Property::Index(index) => data.get(index),
                } {
                    let result = tail.apply_to_path(child, input_path);
                    input_path.pop();
                    result
                } else {
                    let message = match head {
                        Property::Field(name) => format!("{:?} not found", name),
                        Property::Quoted(name) => format!("{:?} not found", name),
                        Property::Index(index) => format!("{:?} not found", index),
                    };
                    let error = ApplyToError::new(message.as_str(), input_path);
                    input_path.pop();
                    (None, vec![error])
                }
            }
            Self::Selection(selection) => {
                // If data is not an object here, this recursive apply_to_path
                // call will handle the error.
                selection.apply_to_path(data, input_path)
            }
            Self::Empty => {
                // If data is not an object here, we want to preserve its value
                // without an error.
                (Some(data.clone()), vec![])
            }
        }
    }
}

impl ApplyTo for SubSelection {
    fn apply_to_path(
        &self,
        data: &JSON,
        input_path: &mut Vec<Property>,
    ) -> (Option<JSON>, Vec<ApplyToError>) {
        if data.is_array() {
            return self.apply_to_array(data, input_path);
        }

        if !data.is_object() {
            return (None, vec![ApplyToError::new("not an object", input_path)]);
        }

        let mut output = Map::new();
        let mut errors = vec![];

        for named_selection in &self.selections {
            let (value, mut named_selection_errors) =
                named_selection.apply_to_path(data, input_path);
            // If value is an object, extend output with its keys and their values.
            if let Some(JSON::Object(key_and_value)) = value {
                output.extend(key_and_value);
            }
            errors.append(&mut named_selection_errors);
        }

        (Some(JSON::Object(output)), errors)
    }
}

#[test]
fn test_apply_to_selection() {
    let data = json!({
        "hello": "world",
        "nested": {
            "hello": "world",
            "world": "hello",
        },
        "array": [
            { "hello": "world 0" },
            { "hello": "world 1" },
            { "hello": "world 2" },
        ],
    });

    let check_ok = |selection: Selection, expected_json: JSON| {
        let (actual_json, errors) = selection.apply_to(&data);
        assert_eq!(actual_json, Some(expected_json));
        assert_eq!(errors, vec![]);
    };

    check_ok(selection!("hello"), json!({"hello": "world"}));

    check_ok(
        selection!("nested"),
        json!({
            "nested": {
                "hello": "world",
                "world": "hello",
            },
        }),
    );

    check_ok(selection!(".nested.hello"), json!("world"));

    check_ok(selection!(".nested.world"), json!("hello"));

    check_ok(
        selection!("nested hello"),
        json!({
            "hello": "world",
            "nested": {
                "hello": "world",
                "world": "hello",
            },
        }),
    );

    check_ok(
        selection!("array { hello }"),
        json!({
            "array": [
                { "hello": "world 0" },
                { "hello": "world 1" },
                { "hello": "world 2" },
            ],
        }),
    );

    check_ok(
        selection!("greetings: array { hello }"),
        json!({
            "greetings": [
                { "hello": "world 0" },
                { "hello": "world 1" },
                { "hello": "world 2" },
            ],
        }),
    );

    check_ok(
        selection!(".array { hello }"),
        json!([
            { "hello": "world 0" },
            { "hello": "world 1" },
            { "hello": "world 2" },
        ]),
    );

    check_ok(
        selection!("worlds: .array.hello"),
        json!({
            "worlds": [
                "world 0",
                "world 1",
                "world 2",
            ],
        }),
    );

    check_ok(
        selection!(".array.hello"),
        json!(["world 0", "world 1", "world 2",]),
    );

    check_ok(
        selection!("nested grouped: { hello worlds: .array.hello }"),
        json!({
            "nested": {
                "hello": "world",
                "world": "hello",
            },
            "grouped": {
                "hello": "world",
                "worlds": [
                    "world 0",
                    "world 1",
                    "world 2",
                ],
            },
        }),
    );
}

#[test]
fn test_apply_to_errors() {
    let data = json!({
        "hello": "world",
        "nested": {
            "hello": 123,
            "world": true,
        },
        "array": [
            { "hello": 1, "goodbye": "farewell" },
            { "hello": "two" },
            { "hello": 3.0, "smello": "yellow" },
        ],
    });

    assert_eq!(
        selection!("hello").apply_to(&data),
        (Some(json!({"hello": "world"})), vec![],)
    );

    assert_eq!(
        selection!("yellow").apply_to(&data),
        (
            Some(json!({})),
            vec![ApplyToError::from_json(&json!({
                "message": "\"yellow\" not found",
                "path": ["yellow"],
            })),],
        )
    );

    assert_eq!(
        selection!(".nested.hello").apply_to(&data),
        (Some(json!(123)), vec![],)
    );

    assert_eq!(
        selection!(".nested.'yellow'").apply_to(&data),
        (
            None,
            vec![ApplyToError::from_json(&json!({
                "message": "\"yellow\" not found",
                "path": ["nested", "yellow"],
            })),],
        )
    );

    assert_eq!(
        selection!(".nested { hola yellow world }").apply_to(&data),
        (
            Some(json!({
                "world": true,
            })),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "\"hola\" not found",
                    "path": ["nested", "hola"],
                })),
                ApplyToError::from_json(&json!({
                    "message": "\"yellow\" not found",
                    "path": ["nested", "yellow"],
                })),
            ],
        )
    );

    assert_eq!(
        selection!("partial: .array { hello goodbye }").apply_to(&data),
        (
            Some(json!({
                "partial": [
                    { "hello": 1, "goodbye": "farewell" },
                    { "hello": "two" },
                    { "hello": 3.0 },
                ],
            })),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "\"goodbye\" not found",
                    "path": ["array", 1, "goodbye"],
                })),
                ApplyToError::from_json(&json!({
                    "message": "\"goodbye\" not found",
                    "path": ["array", 2, "goodbye"],
                })),
            ],
        )
    );

    assert_eq!(
        selection!("good: .array.hello bad: .array.smello").apply_to(&data),
        (
            Some(json!({
                "good": [
                    1,
                    "two",
                    3.0,
                ],
                "bad": [
                    null,
                    null,
                    "yellow",
                ],
            })),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "\"smello\" not found",
                    "path": ["array", 0, "smello"],
                })),
                ApplyToError::from_json(&json!({
                    "message": "\"smello\" not found",
                    "path": ["array", 1, "smello"],
                })),
            ],
        )
    );

    assert_eq!(
        selection!("array { hello smello }").apply_to(&data),
        (
            Some(json!({
                "array": [
                    { "hello": 1 },
                    { "hello": "two" },
                    { "hello": 3.0, "smello": "yellow" },
                ],
            })),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "\"smello\" not found",
                    "path": ["array", 0, "smello"],
                })),
                ApplyToError::from_json(&json!({
                    "message": "\"smello\" not found",
                    "path": ["array", 1, "smello"],
                })),
            ],
        )
    );

    assert_eq!(
        selection!(".nested { grouped: { hello smelly world } }").apply_to(&data),
        (
            Some(json!({
                "grouped": {
                    "hello": 123,
                    "world": true,
                },
            })),
            vec![ApplyToError::from_json(&json!({
                "message": "\"smelly\" not found",
                "path": ["nested", "smelly"],
            })),],
        )
    );

    assert_eq!(
        selection!("alias: .nested { grouped: { hello smelly world } }").apply_to(&data),
        (
            Some(json!({
                "alias": {
                    "grouped": {
                        "hello": 123,
                        "world": true,
                    },
                },
            })),
            vec![ApplyToError::from_json(&json!({
                "message": "\"smelly\" not found",
                "path": ["nested", "smelly"],
            })),],
        )
    );
}

#[test]
fn test_apply_to_nested_arrays() {
    let data = json!({
        "arrayOfArrays": [
            [
                { "x": 0, "y": 0 },
            ],
            [
                { "x": 1, "y": 0 },
                { "x": 1, "y": 1 },
                { "x": 1, "y": 2 },
            ],
            [
                { "x": 2, "y": 0 },
                { "x": 2, "y": 1 },
            ],
            [],
            [
                null,
                { "x": 4, "y": 1 },
                { "x": 4, "why": 2 },
                null,
                { "x": 4, "y": 4 },
            ]
        ],
    });

    assert_eq!(
        selection!(".arrayOfArrays.x").apply_to(&data),
        (
            Some(json!([[0], [1, 1, 1], [2, 2], [], [null, 4, 4, null, 4],])),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 0],
                })),
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 3],
                })),
            ],
        ),
    );

    assert_eq!(
        selection!(".arrayOfArrays.y").apply_to(&data),
        (
            Some(json!([
                [0],
                [0, 1, 2],
                [0, 1],
                [],
                [null, 1, null, null, 4],
            ])),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 0],
                })),
                ApplyToError::from_json(&json!({
                    "message": "\"y\" not found",
                    "path": ["arrayOfArrays", 4, 2, "y"],
                })),
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 3],
                })),
            ],
        ),
    );

    assert_eq!(
        selection!("alias: arrayOfArrays { x y }").apply_to(&data),
        (
            Some(json!({
                "alias": [
                    [
                        { "x": 0, "y": 0 },
                    ],
                    [
                        { "x": 1, "y": 0 },
                        { "x": 1, "y": 1 },
                        { "x": 1, "y": 2 },
                    ],
                    [
                        { "x": 2, "y": 0 },
                        { "x": 2, "y": 1 },
                    ],
                    [],
                    [
                        null,
                        { "x": 4, "y": 1 },
                        { "x": 4 },
                        null,
                        { "x": 4, "y": 4 },
                    ]
                ],
            })),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 0],
                })),
                ApplyToError::from_json(&json!({
                    "message": "\"y\" not found",
                    "path": ["arrayOfArrays", 4, 2, "y"],
                })),
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 3],
                })),
            ],
        ),
    );

    assert_eq!(
        selection!("ys: .arrayOfArrays.y xs: .arrayOfArrays.x").apply_to(&data),
        (
            Some(json!({
                "ys": [
                    [0],
                    [0, 1, 2],
                    [0, 1],
                    [],
                    [null, 1, null, null, 4],
                ],
                "xs": [
                    [0],
                    [1, 1, 1],
                    [2, 2],
                    [],
                    [null, 4, 4, null, 4],
                ],
            })),
            vec![
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 0],
                })),
                ApplyToError::from_json(&json!({
                    "message": "\"y\" not found",
                    "path": ["arrayOfArrays", 4, 2, "y"],
                })),
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 3],
                })),
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 0],
                })),
                ApplyToError::from_json(&json!({
                    "message": "not an object",
                    "path": ["arrayOfArrays", 4, 3],
                })),
            ],
        ),
    );
}
