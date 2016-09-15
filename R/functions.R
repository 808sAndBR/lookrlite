example_path <- "https://example.looker.com/api/3.0"

check_alive <- function(hash, hostname, port=19999) {
  # Input:    hash, or hostname, [port]
  # Purpose:  Check to make sure the API host is responsive.

  require(httr)
  request_response <- GET(modify_url(example_path,
                                     hostname=hostname,
                                     port=port,
                                     path="alive"))
  check_response_status(request_response)
}

check_token <- function(hash, hostname, port=19999) {
  # Inputs:   hash, or hostname, [port]
  # Purpose:  Check to see if the token for a connection is expired.
  #           Renew using api_login if the token is expired.

  if (exists_connection(hostname, port))
    connection <- get_connection(hostname, port, hash)
  else
    api_login(hostname, port)

  if (is.null(connection["token"]) || Sys.time() > connection["expires_at"]) {
    message("Need a new token")
    api_login(connection["hostname"],
              connection["port"],
              connection["client_id"],
              connection["client_secret"])
  }
}

api_login <- function(hostname, port=19999, client_id=NULL, client_secret=NULL) {
  # Inputs:   hostname, [port], [client_id], [client_secret]
  # Purpose:  Get an authentication token for the given hostname and port,
  #           will ask user for a client_id and client_secret if not
  #           provided programmatically.
  # Returns:  The response returned from the host.

  require(httr)
  if (!check_alive(hostname=hostname, port=port))
    stop("No response from ", hostname, ":", port, "/alive")
  hash <- hash_connection(hostname, port)
  if (exists_connection(hash, hostname, port)) {
    connection <- get_connection(hash=hash)
    client_id <- connection["client_id"]
    client_secret <- connection["client_secret"]
  } else {
    if (missing(client_id))
      client_id <- readline("Enter client ID: ")
    if (missing(client_secret))
      client_secret <- readline("Enter client secret: ")
  }
  message("Getting new token...")
  auth_body <- paste0("client_id=", client_id, "&client_secret=", client_secret)
  request_response <- POST(modify_url(example_path,
                                      hostname=hostname,
                                      port=port,
                                      path="login"),
                           body = auth_body)
  token <- jsonlite::fromJSON(content(request_response, as="text"))
  if (exists_connection(hash, hostname, port)) {
    update_connection(hash, hostname, port, client_id, client_secret, token)
  } else {
    set_connection(hash, hostname, port, client_id, client_secret, token)
  }
  if (!is.null(token$access_token) && nchar(token$access_token) == 40)
    message("Login success")
  check_response_status(request_response)
}

api_logout <- function(hash, hostname, port=19999) {
  # Required: hash, or hostname, [port]
  # Purpose:  Revoke the token associated with a connection and delete
  #           the connection object associated with it.

  require(httr)
  connection <- get_connection(hostname, port)
  request_response <- api_delete(hostname=hostname,
                                 port=port,
                                 path="logout",
                                 add_headers(Authentication=connection$token))
  if (check_response_status(request_response)) {
    message("Logout success")
    return(request_response)
  }
}

get_connection <- function(hash, hostname, port=19999, envir=globalenv()) {
  # Required: hash, or hostname, [port]
  possible_connections <- lapply(list(list_connections()),
                                 get, envir=globalenv())
  if (hasArg(hash)) {
    hashes <- lapply(possible_connections,
                     { function(x) x["hash"] }, simplify=TRUE)
    connection <- possible_connections[hashes == hash]
    if (length(connection) == 1) return(connection)
  } else {
    hostnames <- lapply(possible_connections,
                        { function(x) x["hostname"] }, simplify=TRUE)
    ports <- lapply(possible_connections,
                    { function(x) x["port"] }, simplify=TRUE)
    connection <- possible_connections[hostnames == hostname, ports == port]
    if (length(connection) == 1) return(connection)
  }
}

set_connection <- function(hash, hostname, port=19999, client_id, client_secret, token) {
  # Required: hash, or hostname, [port], client_id, client_secret, token
  # Purpose:  Create a connection object in the global environment

  possible_connections <- list_connections()
  this_connection <- length(possible_connections) + 1
  assign(paste("conn", this_connection, sep="_"),
         list(hash=hash,
              hostname=hostname,
              port=port,
              client_id=client_id,
              client_secret=client_secret,
              token=paste("token", token["access_token"]),
              expires_at=Sys.time()+as.numeric(token["expires_in"])),
         envir=globalenv())
}

update_connection <- function(hash, hostname, port=19999, client_id, client_secret, token) {
  # Required: hash, or hostname, [port]
  # Data:     [client_id], [client_secret], [token]
  # Purpose:  Update client_id, client_secret, and token of the connection object
  #           specified by the hash or hostname and port.

  connection <- get_connection(hostname, port, hash, envir)
  if (hasArg(client_id)) connection["client_id"] <- client_id
  if (hasArg(client_secret)) connection["client_secret"] <- client_secret
  if (hasArg(token)) {
    connection["token"] <- token["access_token"]
    connection["expires_in"] <- Sys.time()+token["expires_in"]
  }
}

hash_connection <- function(hostname, port=19999) {
  # Inputs:   hostname, [port]
  # Purpose:  Produce hash of the connection name and port.

  require(httr)
  require(digest)
  digest(paste(hostname, port, sep=":"), algo="md5")
}

exists_connection <- function(hash, hostname, client_id, port=19999) {
  # Inputs:   hash, or hostname, [port]
  # Purpose:  Check if a given connection exists in the global environment.

  possible_connections <- list_connections()
  if (is.na(possible_connections) || length(possible_connections) == 0)
    return(FALSE)
  if (hasArg(hash)) {
    hashes <- sapply(possible_connections, { function(x) x["hash"] })
    return(any(hashes == hash, na.rm=T))
  } else {
    hostnames <- sapply(possible_connections, { function(x) x["hostname"] })
    ports <- lapply(possible_connections, { function(x) x["port"] })
    return(any(hostnames == hostname && ports == port, na.rm=T))
  }
}

list_connections <- function() {
  # Purpose:  Return a list of connection objects in the global environment
  # Returns:  Character vector with the object names of all connections.

  grep("conn[_0-9]", ls(envir=globalenv()), value=TRUE)
}

api_post <- function(hash, hostname, port=19999, path, ...) {
  # Inputs:   hash, or hostname, [port], and path [...]
  # Purpose:  Issue an HTTP POST request to the specified connection at
  #           the specified path. Include headers and data in ...
  # Returns:  The entire response: metadata and content

  require(httr)
  check_token(hash, hostname, port)
  request_response <- POST(modify_url(example_path,
                                      hostname=hostname,
                                      port=port,
                                      path=path),
                           ...)
  if (check_response_status(request_response))
    return(request_response)
}

api_get <- function(hostname, port=19999, path, ...) {
  # Inputs:   hash, or hostname, [port], and path [...]
  # Purpose:  Issue an HTTP GET request to the specified connection at
  #           the specified path. Include headers and data in ...
  # Returns:  The entire response: metadata and content

  require(httr)
  check_token(hostname, port)
  request_response <- GET(modify_url(example_path,
                                     hostname=hostname,
                                     port=port,
                                     path=path),
                          ...)
  if (check_response_status(request_response))
    return(request_response)
}

api_patch <- function(hostname, port=19999, path, ...) {
  # Inputs:   hash, or hostname, [port], and path [...]
  # Purpose:  Issue an HTTP PATCH request to the specified connection at
  #           the specified path. Include headers and data in ...
  # Returns:  The entire response: metadata and content

  require(httr)
  check_token(hostname, port)
  request_response <- PATCH(modify_url(example_path,
                                       hostname=hostname,
                                       port=port,
                                       path=path),
                            ...)
  if (check_response_status(request_response))
    return(request_response)
}

api_delete <- function(hash, hostname, port=19999, path, ...) {
  # Inputs:   hash, or hostname, [port], and path [...]
  # Purpose:  Issue an HTTP DELETE request to the specified connection at
  #           the specified path. Include headers and data in ...
  # Returns:  The entire response: metadata and content

  require(httr)
  check_token(hostname, port)
  request_response <- DELETE(modify_url(example_path,
                                        hostname=hostname,
                                        port=port,
                                        path=path),
                             ...)
  if (check_response_status(request_response))
    return(request_response)
}

check_response_status <- function(request_response) {
  # Inputs:   request_response
  # Purpose:  Check the request response status code and output a
  #           use-friendly message.
  # Returns:  TRUE OR FALSE depending on the success or failure of the request.

  message_for_status(request_response)
  return(!http_error(request_response))
}
