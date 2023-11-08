//! Authorization plugin

// To remove once the commented code is out
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::extra_unused_lifetimes)]

use std::collections::HashMap;
use std::ops::ControlFlow;
use std::ops::Deref;
use std::sync::Arc;

use apollo_compiler::schema::Directive;
use apollo_compiler::schema::ExtendedType;
use apollo_compiler::schema::Value;
use apollo_compiler::Node;
use http::uri::*;
use http::Uri;
use regex::Regex;
use tower::ServiceBuilder;
use tower::ServiceExt;

use crate::layers::ServiceBuilderExt;
use crate::services::subgraph;
// use crate::spec::hir_type_name;
use crate::spec::Query;
use crate::spec::Schema;
use crate::spec::Selection;

use super::directives::SourceApi;

pub(crate) const HTTP_RESOURCE_DIRECTIVE_NAME: &str = "http_resource";
pub(crate) const HTTP_LIST_RESOURCE_DIRECTIVE_NAME: &str = "http_list_resource";
pub(crate) const HTTP_FIELD_DIRECTIVE_NAME: &str = "http_field";

#[derive(Clone)]
pub(crate) struct SubgraphConnector {
    // subgraph name -> callparams
    metadata: HashMap<String, CallParams>,
    // TODO: Arc plz
    // type name -> (field name -> directive)
    field_directives_for_type: Arc<HashMap<String, HashMap<String, Node<Directive>>>>,
}

impl SubgraphConnector {
    pub(crate) fn for_schema(schema: Arc<Schema>) -> Self {
        let mut field_directives_for_type: HashMap<String, HashMap<String, Node<Directive>>> =
            Default::default();
        let mut metadata: HashMap<String, CallParams> = Default::default();

        for (typename, ty) in &schema.definitions.types {
            // TODO: Check against http_list_resource as well

            if let Some(api_name) = ty
                .directives()
                .get(HTTP_RESOURCE_DIRECTIVE_NAME)
                .and_then(|http_directive| http_directive.argument_by_name("api"))
            {
                if let ExtendedType::Object(o) = &ty {
                    let mut field_directives: HashMap<String, Node<Directive>> = Default::default();

                    for (fieldname, field) in &o.fields {
                        if let Some(directive) = field.directives.get(HTTP_FIELD_DIRECTIVE_NAME) {
                            field_directives.insert(fieldname.to_string(), directive.clone());
                        }
                    }

                    field_directives_for_type.insert(typename.to_string(), field_directives);
                }

                if let Value::Enum(value) = api_name.deref() {
                    if let Some(subgraph_name) = schema.subgraph_name(value.as_str()) {
                        metadata.insert(
                            subgraph_name.clone(),
                            CallParams::from_api_and_definition(
                                schema.subgraph_url(subgraph_name).unwrap().clone(),
                                &ty,
                            ),
                        );
                    }
                }
            }
        }

        Self {
            metadata,
            field_directives_for_type: Arc::new(field_directives_for_type),
        }
    }

    pub(crate) fn subgraph_service(
        &self,
        subgraph_name: &str,
        service: subgraph::BoxService,
    ) -> subgraph::BoxService {
        if let Some(_call_parameters) = self.metadata.get(subgraph_name) {
            todo!();
            //     let call_parameters: CallParams = call_parameters.clone();
            //     let service_name = subgraph_name.to_string();
            //     let field_directives_for_types = self.field_directives_for_type.clone();
            //     ServiceBuilder::new()
            //     .checkpoint_async(move |request: subgraph::Request| {
            //             println!("subgraph request \n{}", serde_json::to_string_pretty(&request.subgraph_request.body()).unwrap());
            //             let service_name = service_name.clone();

            //             let query = request.subgraph_request.body().query.clone().unwrap();

            //             let relevant_schemas = request
            //                 .context
            //                 .private_entries
            //                 .lock()
            //                 .get::<Arc<HashMap<String, Arc<Schema>>>>()
            //                 .cloned();

            //             let query = Query::parse(
            //                 query,
            //                 relevant_schemas.unwrap().get(&service_name).unwrap(),
            //                 &Default::default(),
            //             )
            //             .unwrap();

            //             let root_field = query
            //                 .operations
            //                 .first()
            //                 .unwrap()
            //                 .selection_set
            //                 .first()
            //                 .unwrap();

            //             let mut fragment_root = None;
            //             let mut root_is_list = false;
            //             let (root_field_name, root_field_type) = if let Selection::Field {
            //                 name,
            //                 field_type,
            //                 selection_set,
            //                 ..
            //             } = root_field
            //             {
            //                 let field_type = selection_set.as_ref().and_then(|selection_set| {
            //                     selection_set
            //                         .iter()
            //                         .any(|selection| {
            //                             if let Selection::Field {
            //                                 name,
            //                                 ..
            //                             } = selection
            //                             {
            //                                 name.as_str() == "__typename"
            //                             } else {
            //                                 false
            //                             }
            //                         })
            //                         .then(|| {
            //                             match &field_type.0 {
            //                                 Type::List{..} => {
            //                                     root_is_list = true;
            //                                 }
            //                                 Type::NonNull {
            //                                     ty,..
            //                                 } => {
            //                                     root_is_list = ty.is_list()
            //                                 }
            //                                 _ => {}
            //                             }
            //                             hir_type_name(&field_type.0).to_string()
            //                         })
            //                 });

            //                 if "_entities" == name.as_str() {
            //                     fragment_root = selection_set.as_ref().and_then(|selection_set| {
            //                         selection_set.first().and_then(|selection| {
            //                             if let Selection::InlineFragment { selection_set, .. } =
            //                                 selection
            //                             {
            //                                 selection_set.first().and_then(|selection| {
            //                                     if let Selection::Field {
            //                                         name, field_type, ..
            //                                     } = selection
            //                                     {
            //                                         match &field_type.0 {
            //                                             Type::List{..} => {
            //                                                 root_is_list = true;
            //                                             }
            //                                             Type::NonNull {
            //                                                 ty,..
            //                                             } => {
            //                                                 root_is_list = ty.is_list()
            //                                             }
            //                                             _ => {}
            //                                         }
            //                                         Some((
            //                                             name.as_str().to_string(),
            //                                             hir_type_name(&field_type.0).to_string(),
            //                                         ))
            //                                     } else {
            //                                         None
            //                                     }
            //                                 })
            //                             } else {
            //                                 None
            //                             }
            //                         })
            //                     });
            //                 }
            //                 (name.as_str().to_string(), field_type)
            //             } else {
            //                 // TODO: maybe don't panic
            //                 panic!("no root field?! Oo");
            //             };

            //             let mut requests_to_send = if root_is_list {
            //                 let call_parameters = call_parameters.list.clone().expect("we should have a list endpoint here...");
            //                 call_parameters.into_requests(&request.subgraph_request, root_field_type.as_ref().and_then(|f| field_directives_for_types.get(f).cloned()).unwrap_or_default())
            //             } else {
            //                 let call_parameters = &call_parameters.object;
            //                call_parameters.into_requests(&request.subgraph_request, root_field_type.as_ref().and_then(|f| field_directives_for_types.get(f).cloned()).unwrap_or_default())
            //             };

            //             // TODO: traverse query roots!

            //             async move {
            //                 let (parts, body) = if requests_to_send.len() > 1 {

            //                     let stuff = futures::future::join_all(requests_to_send.into_iter().map(|(request, type_name)| make_request(request, root_field_type.clone(), type_name))).await;

            //                     // let (all_parts, all_bodies, type_name): (Vec<http::response::Parts>, Vec<serde_json::Value>, Option<String>) = stuff.unzip();

            //                     let mut value = if root_is_list {
            //                         serde_json_bytes::Value::Array(Vec::with_capacity(stuff.len()))
            //                     } else {
            //                         serde_json_bytes::Value::Null
            //                     };

            //                     // todo: figure this out
            //                     let mut parts = None;

            //                     for (p, mut body, t) in stuff {
            //                         parts = Some(p);

            //                         if body.is_array() {
            //                             for item in body.as_array_mut().unwrap() {
            //                                 item["__typename"] = t.clone().into();
            //                             }
            //                         } else {
            //                             body["__typename"] = t.clone().into();
            //                         }
            //                         use crate::json_ext::ValueExt;
            //                         value.deep_merge(body);
            //                     }

            //                     let value = if root_field_name == "_entities" {
            //                             serde_json::json! {{ root_field_name: [
            //                                 { fragment_root.unwrap().0 : value }
            //                             ] }}
            //                         } else {
            //                             serde_json::json! {{ root_field_name: value }}
            //                         };

            //                     (parts.unwrap(), value)
            //                 } else {
            //                     let (request_to_send, type_name) = requests_to_send.pop().expect("there should be at least 1 request to send");
            //                    let (parts, value, _) = make_request(request_to_send , root_field_type.clone(), type_name).await;

            //                     let value = if root_field_name == "_entities" {
            //                         serde_json::json! {{ root_field_name: [{ fragment_root.unwrap().0 : value }] }}
            //                     } else {
            //                         serde_json::json! {{ root_field_name: value }}
            //                     };

            //                     (parts, value)
            //                 };

            //                 let graphql_response =
            //                     crate::graphql::Response::builder().data(body).build();

            //                 let resp = http::Response::from_parts(parts, graphql_response);
            //                 Ok(ControlFlow::Break(
            //                     crate::services::SubgraphResponse::new_from_response(
            //                         resp,
            //                         request.context,
            //                     ),
            //                 ))
            //             }
            //         })
            //         .buffered()
            //         .service(service)
            //         .boxed()
        } else {
            service
        }
    }
}

// async fn make_request(
//     request: hyper::Request<hyper::Body>,
//     root_field_type: Option<String>,
//     type_name: Option<String>,
// ) -> (
//     http::response::Parts,
//     serde_json_bytes::Value,
//     Option<String>,
// ) {
//     // TODO: Reuse client plz >.<
//     let client = hyper::Client::new();
//     let response = client.request(request).await.unwrap();
//     let (parts, body) = response.into_parts();
//     let mut body: serde_json_bytes::Value =
//         serde_json::from_slice(&hyper::body::to_bytes(body).await.unwrap()).unwrap_or_default();

//     if body.is_array() {
//         body.as_array_mut().unwrap().iter_mut().for_each(|item| {
//             if let Some(type_name) = root_field_type.clone() {
//                 item["__typename"] = type_name.clone().into();
//             }
//         });
//     } else {
//         if !body.is_null() {
//             if let Some(type_name) = root_field_type {
//                 body["__typename"] = type_name.into();
//             }
//         }
//     }

//     (parts, body, type_name)
// }

#[derive(Debug, Clone)]
struct CallParams {
    object: ObjectCallParams,
    list: Option<ListCallParams>,
}

#[derive(Debug, Clone)]
struct ObjectCallParams {
    method: http::Method,
    uri: http::Uri,
}

#[derive(Debug, Clone)]
struct ListCallParams {
    method: http::Method,
    uri: http::Uri,
}

impl ObjectCallParams {
    fn from_api_and_definition(api_url: Uri, ty: &ExtendedType) -> Self {
        let http_resource = ty.directives().get(HTTP_RESOURCE_DIRECTIVE_NAME).unwrap();

        let path = http_resource
            .argument_by_name("GET")
            .unwrap()
            .as_str()
            .unwrap();
        let method = http::Method::GET;

        let mut parts = Parts::default();
        parts.scheme = api_url.scheme().cloned();
        parts.authority = api_url.authority().cloned();
        parts.path_and_query = Some(path.parse().unwrap());
        let uri = Uri::from_parts(parts).unwrap();

        Self { method, uri }
    }

    // fn into_requests(
    //     &self,
    //     request: &http::Request<crate::graphql::Request>,
    //     field_directives_for_type: HashMap<String, Directive>,
    // ) -> Vec<(hyper::Request<hyper::Body>, Option<String>)> {
    //     into_requests(
    //         self.uri.clone().to_string(),
    //         self.method.clone(),
    //         request,
    //         field_directives_for_type,
    //     )
    // }
}

impl ListCallParams {
    fn from_api_and_definition(api_url: Uri, definition: &ExtendedType) -> Option<Self> {
        definition
            .directives()
            .get(HTTP_LIST_RESOURCE_DIRECTIVE_NAME)
            .map(|http_resource| {
                let path = http_resource
                    .argument_by_name("GET")
                    .unwrap()
                    .as_str()
                    .unwrap();
                let method = http::Method::GET;

                let mut parts = Parts::default();
                parts.scheme = api_url.scheme().cloned();
                parts.authority = api_url.authority().cloned();
                parts.path_and_query = Some(path.parse().unwrap());
                let uri = Uri::from_parts(parts).unwrap();

                Self { method, uri }
            })
    }

    // fn into_requests(
    //     &self,
    //     request: &http::Request<crate::graphql::Request>,
    //     field_directives_for_type: HashMap<String, Directive>,
    // ) -> Vec<(hyper::Request<hyper::Body>, Option<String>)> {
    //     into_requests(
    //         self.uri.clone().to_string(),
    //         self.method.clone(),
    //         request,
    //         field_directives_for_type,
    //     )
    // }
}

impl CallParams {
    fn from_api_and_definition(api_url: Uri, definition: &ExtendedType) -> Self {
        Self {
            object: ObjectCallParams::from_api_and_definition(api_url.clone(), definition),
            list: ListCallParams::from_api_and_definition(api_url, definition),
        }
    }
}

// fn remove_extra_variables(uri: String) -> String {
//     let re = Regex::new(r"\{.*?\}").unwrap();
//     let to_return = re.replace_all(uri.as_str(), "").to_string();
//     // remove extra slashes
//     to_return.trim_end_matches("/").to_string()
// }

// fn into_requests(
//     mut uri: String,
//     method: http::Method,
//     request: &http::Request<crate::graphql::Request>,
//     field_directives_for_type: HashMap<String, Directive>,
// ) -> Vec<(hyper::Request<hyper::Body>, Option<String>)> {
//     let mut representations = request
//         .body()
//         .variables
//         .get("representations")
//         .map(|rep| {
//             deal_with_representations(rep.clone(), uri.clone(), field_directives_for_type.clone())
//         })
//         .unwrap_or_default();

//     for (name, value) in request.body().variables.iter() {
//         if name.as_str() == "representations" {
//             continue;
//         }
//         use serde_json_bytes::Value;
//         let to_replace = match value {
//             Value::Array(a) => {
//                 dbg!("i didnt expect an array here...");
//                 dbg!(&a);
//                 continue;
//             }
//             Value::Object(o) => {
//                 dbg!(name, o);
//                 "not yet implemented".to_string()
//             }
//             Value::Null => "".to_string(),
//             Value::Bool(v) => v.to_string(),
//             Value::Number(v) => v.to_string(),
//             Value::String(v) => v.as_str().to_string(),
//         };

//         uri = uri.replace(&format!("{{{}}}", name.as_str()), to_replace.as_str());

//         representations = representations
//             .into_iter()
//             .map(|(u, type_name)| {
//                 (
//                     u.replace(&format!("{{{}}}", name.as_str()), to_replace.as_str()),
//                     type_name,
//                 )
//             })
//             .collect();
//     }

//     uri = remove_extra_variables(uri);

//     representations = representations
//         .into_iter()
//         .map(|(u, t)| (remove_extra_variables(u), t))
//         .collect();

//     if representations.is_empty() {
//         vec![(
//             hyper::Request::builder()
//                 .method(method)
//                 .uri(uri)
//                 .body(Default::default())
//                 .unwrap(),
//             None,
//         )]
//     } else {
//         representations
//             .into_iter()
//             .map(|(r, t)| {
//                 (
//                     hyper::Request::builder()
//                         .method(method.clone())
//                         .uri(r)
//                         .body(Default::default())
//                         .unwrap(),
//                     Some(t),
//                 )
//             })
//             .collect()
//     }
// }

// // TODO: I'm doing input type magic, none of this is accurate.
// fn deal_with_representations(
//     representations: serde_json_bytes::Value,
//     uri: String,
//     field_directives_for_type: HashMap<String, Directive>,
// ) -> Vec<(String, String)> {
//     use serde_json_bytes::Value;

//     let representations = match representations {
//         serde_json_bytes::Value::Array(representations) => representations,
//         other => {
//             dbg!(&other);
//             panic!("representations should be an array");
//         }
//     };

//     let mut uris = Vec::with_capacity(representations.len());

//     for object in representations {
//         let mut u = uri.clone();
//         let type_name = if let Some(&Value::String(ref s)) = object.get("__typename") {
//             s.as_str()
//         } else {
//             ""
//         };

//         let empty_map = serde_json_bytes::Map::new();

//         for (key, value) in object
//             .as_object()
//             .unwrap_or_else(|| &empty_map)
//             .iter()
//             .filter(|o| o.0.as_str() != "__typename")
//         {
//             // TODO: this is terribly wrong
//             let name = format!(
//                 "{}{}",
//                 type_name.to_lowercase(),
//                 if type_name.is_empty() {
//                     key.as_str().to_lowercase()
//                 } else {
//                     ucfirst(key.as_str())
//                 }
//             );
//             let to_replace = match value {
//                 Value::Array(a) => {
//                     dbg!(&name, a);
//                     "not yet implemented".to_string()
//                 }
//                 Value::Object(o) => {
//                     dbg!(&name, o);
//                     "not yet implemented".to_string()
//                 }
//                 Value::Null => "".to_string(),
//                 Value::Bool(v) => v.to_string(),
//                 Value::Number(v) => v.to_string(),
//                 Value::String(v) => v.as_str().to_string(),
//             };
//             u = u.replace(&format!("{{{}}}", name.as_str()), to_replace.as_str());
//         }

//         uris.push((u, type_name.to_string()));
//     }
//     uris
// }

// fn ucfirst(s: &str) -> String {
//     let mut c = s.chars();
//     match c.next() {
//         None => String::new(),
//         Some(f) => format!(
//             "{}{}",
//             f.to_uppercase().collect::<String>(),
//             c.as_str().to_lowercase()
//         ),
//     }
// }

fn source_apis_from_schema(schema: &Schema) -> SourceApi {
    todo!();
}

#[cfg(test)]
mod tests {
    const SCHEMA: &str = r#"
  schema
    @core(feature: "https://specs.apollo.dev/core/v0.1")
    @core(feature: "https://specs.apollo.dev/join/v0.1") {
    query: Query
  }
  
  directive @core(feature: String!) repeatable on SCHEMA
  
  directive @join__field(
    graph: join__Graph
    requires: join__FieldSet
    provides: join__FieldSet
  ) on FIELD_DEFINITION
  
  directive @join__type(
    graph: join__Graph!
    key: join__FieldSet
  ) repeatable on OBJECT | INTERFACE
  
  directive @join__owner(graph: join__Graph!) on OBJECT | INTERFACE
  
  directive @join__graph(name: String!, url: String!) on ENUM_VALUE
  
  scalar join__FieldSet
  
  enum join__Graph {
    CONTACTS @join__graph(name: "contacts", url: "http://localhost:4002")
    NOTES @join__graph(name: "notes", url: "http://localhost:4002")
  }
  
  enum SOURCE_API {
    CONTACTS @source_api(base_url: "http://localhost:4002")
    NOTES @source_api(base_url: "http://localhost:4002")
    LEGACY_CONTACTS @source_api(base_url: "http://localhost:4002")
  }
  
  directive @source_api(
    # The base hostname/prefix to use, like "https://my-rest-api.net/api/v2"
    base_url: String!
  
    # Whether to use this API as the default when unspecified
    default: Boolean
  
    # List of HTTP header names from the subgraph GraphQL HTTP request
    # to be forwarded with any REST HTTP requests to this API
    headers: [HTTPHeaderMapping!]
  
    # Many more API configuration options can be added here in the future!
  ) on ENUM_VALUE
  
  directive @http_field(
    # Type-safe, as before
    api: SOURCE_API
  
    # For JSON field renaming and nested value extraction
    json_path: [String]
  
    # A string representation of argument names to include in the JSON body
    # of the HTTP request, such as "title text" for createNote (below).
    body: String
  
    # HTTP methods
    GET: String
    POST: String
    PUT: String
    PATCH: String
    DELETE: String
  ) repeatable on FIELD_DEFINITION
  
  directive @http_resource(
    # Allows any SOURCE_API enum value, but only those values.
    api: SOURCE_API
  
    # The URL path template used to retrieve this resource, such as
    # GET: "/contacts/{contactId}/notes/{noteId}"
    GET: String!
  ) repeatable on OBJECT
  
  # this directive behaves the same as @http_resource,
  # except it applies to queries which return type is a list
  directive @http_list_resource(
    # Allows any SOURCE_API enum value, but only those values.
    api: SOURCE_API
  
    # The URL path template used to retrieve this list of resources, such as
    # GET: "/contacts"
    GET: String!
  ) repeatable on OBJECT
  
  type Query {
    contact(id: ID!): Contact @join__field(graph: CONTACTS)
    contacts: [Contact]!
      @join__field(graph: CONTACTS)
      @http_field(GET: "/contacts")
  }
  
  type Contact
    @join__owner(graph: CONTACTS)
    @join__type(graph: CONTACTS, key: "id")
    @join__type(graph: NOTES, key: "id")
    @http_resource(api: CONTACTS, GET: "/contacts/{contactId}") {
    id: ID! @join__field(graph: CONTACTS)
    name: String
    email: String
    notes: [Note]
      @join__field(graph: NOTES)
      @http_field(GET: "/contacts/{contactId}/notes")
    note(noteId: ID!): Note @join__field(graph: NOTES)
  }
  
  type Note
    @join__owner(graph: NOTES)
    @join__type(graph: NOTES, key: "id")
    @http_resource(api: NOTES, GET: "/contacts/{contactId}/notes/{noteId}") {
    contact: Contact @join__field(graph: CONTACTS)
    id: ID! @join__field(graph: NOTES)
    title: String
    text: String
    contactName: String
      @http_field(
        api: LEGACY_CONTACTS
        GET: "/notes?id={noteId}"
        json_path: ["contact", "name"]
      )
  }
"#;

    #[test]
    fn test_source_api() {}
}
