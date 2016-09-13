run_look <- function(connection, look_id, format="json", limit=NULL) {
  require(httr)
  require(jsonlite)
  request_path <- paste0("api/3.0/looks/", look_id, "/run/", format)
  check_token(connection$hostname, connection$port)
  request_response <- api_get(connection$hostname,
                              connection$port,
                              path=request_path,
                              add_headers("Authorization"=connection$token,
                                          "Content-Type"="application/json"),
                              query=list(limit=limit))
  data <- jsonlite::fromJSON(content(request_response,
                                     as="text",
                                     type=format))
  return(data)
}

get_look <- function(connection, look_id) {
  require(httr)
  require(jsonlite)
  request_path <- paste0("api/3.0/looks/", look_id)

  request_response <- api_get(connection$hostname,
                              connection$port,
                              path=request_path,
                              add_headers("Authorization"=connection$token,
                                          "Content-Type"="application/json"))

  data <- jsonlite::fromJSON(content(request_response,
                                     as="text",
                                     type="json"))
  return(data)
}
