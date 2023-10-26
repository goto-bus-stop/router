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

// Selection ::= NamedSelection+ | PathSelection

#[derive(Debug, PartialEq, Clone)]
pub(self) enum Selection {
    Named(Vec<NamedSelection>),
    Path(PathSelection),
}

impl Selection {
    fn parse<'a>(input: &'a str) -> IResult<&str, Self> {
        alt((
            map(many1(NamedSelection::parse), Self::Named),
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
            Selection::Named(vec![NamedSelection::Field(None, "hello".to_string(), None),]),
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
            Selection::Named(vec![NamedSelection::Path(
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
            )]),
        )),
    );

    assert_eq!(
        Selection::parse("before hi: .hello.world after"),
        Ok((
            "",
            Selection::Named(vec![
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
            ]),
        )),
    );

    let before_path_nested_after_result = Ok((
        "",
        Selection::Named(vec![
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
        ]),
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
            Selection::Named(vec![NamedSelection::Field(
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
                                        NamedSelection::Field(None, "properties".to_string(), None,),
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
            ),]),
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
            Ok(("", Selection::Named(vec![expected]))),
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
