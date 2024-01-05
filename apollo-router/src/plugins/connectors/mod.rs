mod connector;
pub(crate) use connector::connector_subgraph_names;
pub(crate) use connector::Connector;
mod directives;
mod join_spec_helpers;
mod selection_parser;
pub(crate) mod subgraph_connector;
mod supergraph;
pub(crate) use supergraph::generate_connector_supergraph;
mod http_json_transport;
mod request_inputs;
mod request_response;
mod url_path_parser;

#[cfg(test)]
mod tests;

pub(crate) type Connectors = (
    std::sync::Arc<apollo_compiler::validation::Valid<apollo_compiler::Schema>>,
    std::collections::HashMap<String, Connector>,
);

pub(crate) fn connectors_from_schema(
    schema: &apollo_compiler::Schema,
) -> Result<Option<Connectors>, tower::BoxError> {
    let connectors = Connector::from_schema(schema)?;

    if connectors.is_empty() {
        Ok(None)
    } else {
        let supergraph = generate_connector_supergraph(schema, &connectors)?;

        Ok(Some((std::sync::Arc::new(supergraph), connectors)))
    }
}
