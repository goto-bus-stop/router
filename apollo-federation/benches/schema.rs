use apollo_federation::schema::ValidFederationSchema;
use criterion::*;

fn bench_schema_setup(c: &mut Criterion) {
    let schema = apollo_compiler::Schema::parse_and_validate(
        include_str!("../../fuzz/supergraph-fed2.graphql"),
        "supergraph-fed2.graphql",
    )
    .unwrap();

    c.bench_function("schema_setup", move |b| {
        b.iter(|| {
            for _ in 0..1_000 {
                black_box(ValidFederationSchema::new(schema.clone()).unwrap());
            }
        });
    });
}

criterion_group!(schema_benches, bench_schema_setup);
criterion_main!(schema_benches);
