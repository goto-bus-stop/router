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
pub(self) enum Selection<'a> {
    Named(Vec<NamedSelection<'a>>),
    Path(PathSelection<'a>),
}

impl<'a> Selection<'a> {
    fn parse(input: &'a str) -> IResult<&str, Self> {
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
            Selection::Named(vec![NamedSelection::Field(
                None,
                Identifier { name: "hello" },
                None
            ),]),
        )),
    );

    assert_eq!(
        Selection::parse(".hello"),
        Ok((
            "",
            Selection::Path(PathSelection::from_slice(
                &[Property::Field(Identifier { name: "hello" }),],
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
                    name: Identifier { name: "hi" }
                },
                PathSelection::from_slice(
                    &[
                        Property::Field(Identifier { name: "hello" }),
                        Property::Field(Identifier { name: "world" }),
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
                NamedSelection::Field(None, Identifier { name: "before" }, None),
                NamedSelection::Path(
                    Alias {
                        name: Identifier { name: "hi" }
                    },
                    PathSelection::from_slice(
                        &[
                            Property::Field(Identifier { name: "hello" }),
                            Property::Field(Identifier { name: "world" }),
                        ],
                        None
                    ),
                ),
                NamedSelection::Field(None, Identifier { name: "after" }, None),
            ]),
        )),
    );

    let before_path_nested_after_result = Ok((
        "",
        Selection::Named(vec![
            NamedSelection::Field(None, Identifier { name: "before" }, None),
            NamedSelection::Path(
                Alias {
                    name: Identifier { name: "hi" },
                },
                PathSelection::from_slice(
                    &[
                        Property::Field(Identifier { name: "hello" }),
                        Property::Field(Identifier { name: "world" }),
                    ],
                    Some(SubSelection {
                        selections: vec![
                            NamedSelection::Field(None, Identifier { name: "nested" }, None),
                            NamedSelection::Field(None, Identifier { name: "names" }, None),
                        ],
                    }),
                ),
            ),
            NamedSelection::Field(None, Identifier { name: "after" }, None),
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
                    name: Identifier {
                        name: "topLevelAlias"
                    }
                }),
                Identifier {
                    name: "topLevelField"
                },
                Some(SubSelection {
                    selections: vec![
                        NamedSelection::Quoted(
                            Alias {
                                name: Identifier {
                                    name: "nonIdentifier"
                                }
                            },
                            "property name with spaces".to_string(),
                            None,
                        ),
                        NamedSelection::Path(
                            Alias {
                                name: Identifier {
                                    name: "pathSelection"
                                }
                            },
                            PathSelection::from_slice(
                                &[
                                    Property::Field(Identifier { name: "some" }),
                                    Property::Field(Identifier { name: "nested" }),
                                    Property::Field(Identifier { name: "path" }),
                                ],
                                Some(SubSelection {
                                    selections: vec![
                                        NamedSelection::Field(
                                            Some(Alias {
                                                name: Identifier { name: "still" }
                                            }),
                                            Identifier { name: "yet" },
                                            None,
                                        ),
                                        NamedSelection::Field(
                                            None,
                                            Identifier { name: "more" },
                                            None,
                                        ),
                                        NamedSelection::Field(
                                            None,
                                            Identifier { name: "properties" },
                                            None,
                                        ),
                                    ],
                                })
                            ),
                        ),
                        NamedSelection::Group(
                            Alias {
                                name: Identifier {
                                    name: "siblingGroup"
                                }
                            },
                            SubSelection {
                                selections: vec![
                                    NamedSelection::Field(
                                        None,
                                        Identifier { name: "brother" },
                                        None,
                                    ),
                                    NamedSelection::Field(
                                        None,
                                        Identifier { name: "sister" },
                                        None,
                                    ),
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
pub(self) enum NamedSelection<'a> {
    Field(Option<Alias<'a>>, Identifier<'a>, Option<SubSelection<'a>>),
    Quoted(Alias<'a>, String, Option<SubSelection<'a>>),
    Path(Alias<'a>, PathSelection<'a>),
    Group(Alias<'a>, SubSelection<'a>),
}

impl<'a> NamedSelection<'a> {
    fn parse(input: &'a str) -> IResult<&str, Self> {
        alt((
            Self::parse_field,
            Self::parse_quoted,
            Self::parse_path,
            Self::parse_group,
        ))(input)
    }

    fn parse_field(input: &'a str) -> IResult<&str, Self> {
        tuple((
            opt(Alias::parse),
            Identifier::parse,
            opt(SubSelection::parse),
        ))(input)
        .map(|(input, (alias, name, selection))| (input, Self::Field(alias, name, selection)))
    }

    fn parse_quoted(input: &'a str) -> IResult<&str, Self> {
        tuple((Alias::parse, parse_string_literal, opt(SubSelection::parse)))(input)
            .map(|(input, (alias, name, selection))| (input, Self::Quoted(alias, name, selection)))
    }

    fn parse_path(input: &'a str) -> IResult<&str, Self> {
        tuple((Alias::parse, PathSelection::parse))(input)
            .map(|(input, (alias, path))| (input, Self::Path(alias, path)))
    }

    fn parse_group(input: &'a str) -> IResult<&str, Self> {
        tuple((Alias::parse, SubSelection::parse))(input)
            .map(|(input, (alias, group))| (input, Self::Group(alias, group)))
    }

    fn name(&self) -> &str {
        match self {
            Self::Field(alias, name, _) => {
                if let Some(alias) = alias {
                    alias.name.name
                } else {
                    name.name
                }
            }
            Self::Quoted(alias, _, _) => alias.name.name,
            Self::Path(alias, _) => alias.name.name,
            Self::Group(alias, _) => alias.name.name,
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
        NamedSelection::Field(None, Identifier { name: "hello" }, None),
        "hello",
    );

    assert_result_and_name(
        "hello { world }",
        NamedSelection::Field(
            None,
            Identifier { name: "hello" },
            Some(SubSelection {
                selections: vec![NamedSelection::Field(
                    None,
                    Identifier { name: "world" },
                    None,
                )],
            }),
        ),
        "hello",
    );

    assert_result_and_name(
        "hi: hello",
        NamedSelection::Field(
            Some(Alias {
                name: Identifier { name: "hi" },
            }),
            Identifier { name: "hello" },
            None,
        ),
        "hi",
    );

    assert_result_and_name(
        "hi: 'hello world'",
        NamedSelection::Quoted(
            Alias {
                name: Identifier { name: "hi" },
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
                name: Identifier { name: "hi" },
            }),
            Identifier { name: "hello" },
            Some(SubSelection {
                selections: vec![NamedSelection::Field(
                    None,
                    Identifier { name: "world" },
                    None,
                )],
            }),
        ),
        "hi",
    );

    assert_result_and_name(
        "hey: hello { world again }",
        NamedSelection::Field(
            Some(Alias {
                name: Identifier { name: "hey" },
            }),
            Identifier { name: "hello" },
            Some(SubSelection {
                selections: vec![
                    NamedSelection::Field(None, Identifier { name: "world" }, None),
                    NamedSelection::Field(None, Identifier { name: "again" }, None),
                ],
            }),
        ),
        "hey",
    );

    assert_result_and_name(
        "hey: 'hello world' { again }",
        NamedSelection::Quoted(
            Alias {
                name: Identifier { name: "hey" },
            },
            "hello world".to_string(),
            Some(SubSelection {
                selections: vec![NamedSelection::Field(
                    None,
                    Identifier { name: "again" },
                    None,
                )],
            }),
        ),
        "hey",
    );

    assert_result_and_name(
        "leggo: 'my ego'",
        NamedSelection::Quoted(
            Alias {
                name: Identifier { name: "leggo" },
            },
            "my ego".to_string(),
            None,
        ),
        "leggo",
    );
}

// PathSelection ::= ("." Property)+ SubSelection?

#[derive(Debug, PartialEq, Clone)]
pub(self) enum PathSelection<'a> {
    // We use a recursive structure here instead of a Vec<Property> to make
    // applying the selection to a JSON value easier.
    Path(Property<'a>, Box<PathSelection<'a>>),
    Selection(SubSelection<'a>),
    Empty,
}

impl<'a> PathSelection<'a> {
    fn parse(input: &'a str) -> IResult<&str, Self> {
        tuple((
            multispace0,
            many1(preceded(char('.'), Property::parse)),
            opt(SubSelection::parse),
        ))(input)
        .map(|(input, (_, path, selection))| (input, Self::from_slice(&path, selection)))
    }

    fn from_slice(properties: &[Property<'a>], selection: Option<SubSelection<'a>>) -> Self {
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
        PathSelection::from_slice(&[Property::Field(Identifier { name: "hello" })], None),
    );

    check_path_selection(
        ".hello.world",
        PathSelection::from_slice(
            &[
                Property::Field(Identifier { name: "hello" }),
                Property::Field(Identifier { name: "world" }),
            ],
            None,
        ),
    );

    check_path_selection(
        ".hello.world { hello }",
        PathSelection::from_slice(
            &[
                Property::Field(Identifier { name: "hello" }),
                Property::Field(Identifier { name: "world" }),
            ],
            Some(SubSelection {
                selections: vec![NamedSelection::Field(
                    None,
                    Identifier { name: "hello" },
                    None,
                )],
            }),
        ),
    );

    check_path_selection(
        ".nested.'string literal'.\"property\".name",
        PathSelection::from_slice(
            &[
                Property::Field(Identifier { name: "nested" }),
                Property::Quoted("string literal".to_string()),
                Property::Quoted("property".to_string()),
                Property::Field(Identifier { name: "name" }),
            ],
            None,
        ),
    );

    check_path_selection(
        ".nested.'string literal' { leggo: 'my ego' }",
        PathSelection::from_slice(
            &[
                Property::Field(Identifier { name: "nested" }),
                Property::Quoted("string literal".to_string()),
            ],
            Some(SubSelection {
                selections: vec![NamedSelection::Quoted(
                    Alias {
                        name: Identifier { name: "leggo" },
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
pub(self) struct SubSelection<'a> {
    selections: Vec<NamedSelection<'a>>,
}

impl<'a> SubSelection<'a> {
    fn parse(input: &'a str) -> IResult<&str, Self> {
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
                selections: vec![NamedSelection::Field(
                    None,
                    Identifier { name: "hello" },
                    None
                ),],
            },
        )),
    );

    assert_eq!(
        SubSelection::parse("{ hello }"),
        Ok((
            "",
            SubSelection {
                selections: vec![NamedSelection::Field(
                    None,
                    Identifier { name: "hello" },
                    None
                ),],
            },
        )),
    );

    assert_eq!(
        SubSelection::parse("  { padded  } "),
        Ok((
            "",
            SubSelection {
                selections: vec![NamedSelection::Field(
                    None,
                    Identifier { name: "padded" },
                    None
                ),],
            },
        )),
    );

    assert_eq!(
        SubSelection::parse("{ hello world }"),
        Ok((
            "",
            SubSelection {
                selections: vec![
                    NamedSelection::Field(None, Identifier { name: "hello" }, None),
                    NamedSelection::Field(None, Identifier { name: "world" }, None),
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
                    Identifier { name: "hello" },
                    Some(SubSelection {
                        selections: vec![NamedSelection::Field(
                            None,
                            Identifier { name: "world" },
                            None
                        ),],
                    })
                ),],
            },
        )),
    );
}

// Alias ::= Identifier ":"

#[derive(Debug, PartialEq, Clone)]
pub(self) struct Alias<'a> {
    name: Identifier<'a>,
}

impl<'a> Alias<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        tuple((Identifier::parse, char(':'), multispace0))(input)
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
                name: Identifier { name: "hello" },
            },
        )),
    );

    assert_eq!(
        Alias::parse("hello :"),
        Ok((
            "",
            Alias {
                name: Identifier { name: "hello" },
            },
        )),
    );

    assert_eq!(
        Alias::parse("hello : "),
        Ok((
            "",
            Alias {
                name: Identifier { name: "hello" },
            },
        )),
    );

    assert_eq!(
        Alias::parse("  hello :"),
        Ok((
            "",
            Alias {
                name: Identifier { name: "hello" },
            },
        )),
    );

    assert_eq!(
        Alias::parse("hello: "),
        Ok((
            "",
            Alias {
                name: Identifier { name: "hello" },
            },
        )),
    );
}

// Property ::= Identifier | StringLiteral

#[derive(Debug, PartialEq, Clone)]
pub(self) enum Property<'a> {
    Field(Identifier<'a>),
    Quoted(String),
}

impl<'a> Property<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            map(Identifier::parse, Self::Field),
            map(parse_string_literal, Self::Quoted),
        ))(input)
    }
}

#[test]
fn test_property() {
    assert_eq!(
        Property::parse("hello"),
        Ok(("", Property::Field(Identifier { name: "hello" }),)),
    );

    assert_eq!(
        Property::parse("'hello'"),
        Ok(("", Property::Quoted("hello".to_string()),)),
    );
}

// Identifier ::= [a-zA-Z_][0-9a-zA-Z_]*

#[derive(Debug, PartialEq, Clone)]
pub(self) struct Identifier<'a> {
    name: &'a str,
}

impl<'a> Identifier<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
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
        .map(|(input, (_, name, _))| (input, Self { name }))
    }

    fn name(&self) -> &str {
        self.name
    }
}

#[test]
fn test_identifier() {
    assert_eq!(
        Identifier::parse("hello"),
        Ok(("", Identifier { name: "hello" })),
    );

    assert_eq!(
        Identifier::parse("hello_world"),
        Ok((
            "",
            Identifier {
                name: "hello_world"
            }
        )),
    );

    assert_eq!(
        Identifier::parse("hello_world_123"),
        Ok((
            "",
            Identifier {
                name: "hello_world_123"
            }
        )),
    );

    assert_eq!(
        Identifier::parse(" hello "),
        Ok(("", Identifier { name: "hello" })),
    );
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
