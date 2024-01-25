extern crate core;

mod common;

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use apollo_router::graphql;
use serde_json::json;
use tracing::Level;
use tracing::Metadata;
use tracing::Subscriber;
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::Registry;

use crate::common::IntegrationTest;

struct TestLogSubscriber {
    registry: Registry,
    event_metadata: Arc<Mutex<Vec<&'static Metadata<'static>>>>,
}

impl Subscriber for TestLogSubscriber {
    fn enabled(&self, _metadata: &Metadata<'_>) -> bool {
        true
    }

    fn new_span(&self, span: &tracing_core::span::Attributes<'_>) -> tracing_core::span::Id {
        self.registry.new_span(span)
    }

    fn record(&self, span: &tracing_core::span::Id, values: &tracing_core::span::Record<'_>) {
        self.registry.record(span, values)
    }

    fn record_follows_from(&self, span: &tracing_core::span::Id, follows: &tracing_core::span::Id) {
        self.registry.record_follows_from(span, follows)
    }

    fn event(&self, event: &tracing::Event<'_>) {
        if event.metadata().target().starts_with("apollo_router")
            && event.metadata().level() == &Level::INFO
        {
            self.event_metadata.lock().unwrap().push(event.metadata());
        }
    }

    fn enter(&self, span: &tracing_core::span::Id) {
        self.registry.enter(span)
    }

    fn exit(&self, span: &tracing_core::span::Id) {
        self.registry.exit(span)
    }
}

impl<'a> LookupSpan<'a> for TestLogSubscriber {
    type Data = tracing_subscriber::registry::Data<'a>;

    fn span_data(&'a self, id: &tracing::Id) -> Option<Self::Data> {
        self.registry.span_data(id)
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn display_headers_and_body_works_for_subgraph_and_source_api() {
    if std::env::var("TEST_APOLLO_KEY").is_ok() && std::env::var("TEST_APOLLO_GRAPH_REF").is_ok() {
        let rhai_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("fixtures");

        let config = json! {{
            "rhai": {
                "scripts": rhai_path,
                "main":"log_payload.rhai"
            }
        }};
        let yaml = serde_yaml::to_string(&config).unwrap();

        let mut router = IntegrationTest::builder()
            .config(yaml.as_str())
            .build()
            .await;

        router
            .start_with_schema_path(PathBuf::from_iter([
                "..",
                "examples",
                "connectors",
                "supergraph.graphql",
            ]))
            .await;

        tokio::time::sleep(std::time::Duration::from_secs(1)).await;
        router.assert_started().await;

        let result = router
            .execute_query(
                &json!({"query":"query Query { serverNetworkInfo { ip org } }","variables":{}}),
            )
            .await
            .1;

        insta::assert_json_snapshot!(result.json::<graphql::Response>().await.unwrap());

        router
            .assert_log_contains("Request headers sent to REST endpoint")
            .await;
        router
            .assert_log_contains("Request body sent to REST endpoint")
            .await;
        router
            .assert_log_contains("Response headers received from REST endpoint")
            .await;
        router
            .assert_log_contains("Response body received from REST endpoint")
            .await;
        router.graceful_shutdown().await;
    }
}
