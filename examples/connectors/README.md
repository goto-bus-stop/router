# Connectors example

## Prerequisites

Docker
Rust

## Setup

in this folder run:

```bash
$ docker-compose up
```

In an other terminal, go to the root of this project (the router directory) and run:

```bash
$ cargo run -- --dev -s ./examples/connectors/supergraph-docker.graphql
```

You can then open the sandbox explorer by visiting http://localhost:4000 in your browser!
