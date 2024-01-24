fn supergraph_service(service) {
    service.map_request(log_payload);
};
  


fn log_payload(request) {
    request.context["apollo_telemetry::logging::display_body"] = true;
    request.context["apollo_telemetry::logging::display_headers"] = true;
    return request;
};
