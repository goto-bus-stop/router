# Connectors example

## Prerequisites

Docker
Rust

## Quickstart

In terminal, go to the root of this project (the router directory) and run:

```bash
$ cargo run -- --dev -s ./examples/connectors/supergraph.graphql
```

You can then open the sandbox explorer by visiting http://localhost:4000 in your browser!

## Local setup

Editing `supergraph.graphql` will not be mandatory anymore once https://github.com/apollographql/connectors/issues/58 is complete (which will be done during private preview)

Edit the supergraph.graphql schema to use local sourceAPI urls, as per the comments:

```graphql
@join__directive(
    graphs: [NETWORK]
    name: "sourceAPI"
    args: { name: "ipinfo", http: { baseURL: "http://localhost:4002" } }
  )
  @join__directive(
    graphs: [NETWORK]
    name: "sourceAPI"
    args: {
      name: "weather"
      http: { baseURL: "http://localhost:4003" }
    }
  )
```

in this folder run:

```bash
$ docker-compose up
```

In an other terminal, go to the root of this project (the router directory) and run:

```bash
$ cargo run -- --dev -s ./examples/connectors/supergraph.graphql
```

You can then open the sandbox explorer by visiting http://localhost:4000 in your browser!

If you want to have a look at opentelemetry traces, run the router with the provided configuration:

```bash
$ cargo run -- -c ./examples/connectors/router.yaml -s ./examples/connectors/supergraph.graphql
```

Jaeger can be found at http://localhost:16686/
