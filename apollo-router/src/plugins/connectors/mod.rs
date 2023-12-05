#[allow(dead_code)]
mod connector;
pub(crate) use connector::connector_subgraph_names;
pub(crate) use connector::Connector;
#[allow(dead_code)]
mod directives;
#[allow(dead_code)]
mod join_spec_helpers;
mod selection_parser;
pub(crate) mod subgraph_connector;
#[allow(dead_code)]
mod supergraph;
pub(crate) use supergraph::generate_connector_supergraph;
mod url_path_parser;
