# Connectors example

## Prerequisites

Docker
Rust

APOLLO_KEY and APOLLO_GRAPH_REF environment variables.

## Quickstart

In terminal, go to the root of this project (the router directory) and run:

```bash
$ cargo run -- --dev -s ./examples/connectors/supergraph.graphql
```

You can then open the sandbox explorer by visiting http://localhost:4000 in your browser!

If you want to have a look at opentelemetry traces, run the router with the provided configuration:

```bash
$ cargo run -- -c ./examples/connectors/router.yaml -s ./examples/connectors/supergraph.graphql
```

You can then open the sandbox explorer by visiting http://localhost:4000 in your browser!

## Local setup

in this folder run:

```bash
$ docker-compose up
```

Use the provided yaml configuration:

```bash
$ cargo run -- --dev -c ./examples/connectors/router_docker_apis.yaml -s ./examples/connectors/supergraph.graphql
```

Jaeger can be found at http://localhost:16686/
