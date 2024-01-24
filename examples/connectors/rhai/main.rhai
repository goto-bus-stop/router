fn supergraph_service(service) {
  service.map_request(log_subgraph_payloads);
};

fn log_subgraph_payloads(request) {
  request.context["apollo_telemetry::logging::display_body"] = true;
  request.context["apollo_telemetry::logging::display_headers"] = true;
  return request;
};
