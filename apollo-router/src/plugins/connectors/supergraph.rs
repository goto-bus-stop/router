use std::collections::HashMap;

use apollo_compiler::name;
use apollo_compiler::Schema;
use itertools::Itertools;
use tower::BoxError;

use super::connector::Connector;
use super::join_spec_helpers::copy_definitions;
use super::join_spec_helpers::join_graph_enum;

/// Generates a new supergraph schema with one subgraph per connector. Copies
/// types and fields from the original schema and adds directives to associate
/// them with the appropriate connector.
pub(crate) fn generate_connector_supergraph(
    schema: &Schema,
    connectors: &HashMap<String, Connector>,
) -> Result<Schema, BoxError> {
    let mut new_schema = Schema::new();
    copy_definitions(schema, &mut new_schema);

    let mut changes = Vec::new();
    // sorted for stable SDL generation
    for connector in connectors.values().sorted_by_key(|c| c.name()) {
        changes.extend(connector.changes(schema)?);
    }

    for change in changes {
        change.apply_to(schema, &mut new_schema)?;
    }

    let connector_graph_names = connectors
        .values()
        // sorted for stable SDL generation
        .sorted_by_key(|c| c.name())
        .map(|c| c.name())
        .collect::<Vec<_>>();
    new_schema.types.insert(
        name!("join__Graph"),
        join_graph_enum(connector_graph_names.as_slice()),
    );

    Ok(new_schema)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use apollo_compiler::Schema;
    use insta::assert_snapshot;
    use itertools::Itertools;

    use super::generate_connector_supergraph;
    use crate::plugins::connectors::connector::Connector;
    use crate::spec::Schema as RouterSchema;
    use crate::Configuration;

    const SCHEMA: &str = include_str!("./test_supergraph.graphql");

    #[test]
    fn it_works() -> anyhow::Result<()> {
        let schema = Schema::parse_and_validate(SCHEMA, "outer.graphql").unwrap();

        let connectors = Arc::from(Connector::from_schema(&schema).unwrap());
        let inner = generate_connector_supergraph(&schema, &connectors).unwrap();

        // new supergraph can be parsed into subgraphs
        let result = RouterSchema::parse(
            inner.serialize().to_string().as_str(),
            &Configuration::fake_builder().build().unwrap(),
        )?;

        assert_eq!(
            result
                .subgraph_definition_and_names
                .values()
                .sorted()
                .cloned()
                .collect::<Vec<_>>(),
            vec![
                "CONNECTOR_HELLO_0".to_string(),
                "CONNECTOR_HELLO_WORLD_0".to_string(),
                "CONNECTOR_MUTATION_MUTATION_1".to_string(),
                "CONNECTOR_QUERY_HELLO_2".to_string(),
                "CONNECTOR_QUERY_WITHARGUMENTS_3".to_string(),
            ]
        );

        assert_snapshot!(inner.serialize().to_string());
        Ok(())
    }
}
