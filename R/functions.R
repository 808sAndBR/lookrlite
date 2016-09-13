check_alive <- function(hostname, port=19999) {
  require(httr)
  request_response <- GET(modify_url("https://example.looker.com",
                                     hostname=hostname,
                                     port=port,
                                     path="alive"))
  return(check_response_status(request_response))
}

api_login <- function(hostname, port=19999, client_id=NULL, client_secret=NULL) {
  require(httr)

  if (!check_alive(hostname=hostname, port=port))
    stop("No response from ", hostname, ":", port, "/alive")
  message("Getting new token...")
  if (missing(client_id))
    client_id <- readline("Enter client ID: ")
  if (missing(client_secret))
    client_secret <- readline("Enter client secret: ")

  auth_body <- paste0("client_id=", client_id, "&client_secret=", client_secret)
  request_response <- api_post(hostname=hostname,
                               port=port,
                               path="login",
                               body = auth_body)

  token <- jsonlite::fromJSON(content(request_response, as="text"))

  set_connection(hostname, port, client_id, client_secret, token)

  if (!is.null(token$access_token) && nchar(token$access_token) == 40) {
    message("Login success")
    return(TRUE)
  }
}

api_logout <- function(hostname, port=19999) {
  require(httr)

}

set_connection <- function(hostname, port=19999, client_id, client_secret, token) {
  assign(paste(hostname, port, "conn", sep="_"),
         list(hostname=hostname,
              port=port,
              client_id=client_id,
              client_secret=client_secret,
              token=token$access_token,
              expires_at=Sys.time()+token$expires_in),
         envir=globalenv())
}

get_connection <- function(hostname, port=19999) {
  get(paste(hostname, port, "conn", sep="_"), envir=globalenv())
}

check_token <- function(hostname, port=19999) {
  # Make sure the connection object exists in the global environment
  # and see if its token has expired, generate token

  if (exists(paste0(hostname, port, "_conn"), envir=globalenv()))
    connection <- get_connection(hostname=hostname, port=port)
  else
    api_login(hostname=hostname, port=port)

  if (is.null(connection$token) || Sys.time() > connection$expires_at)
    api_login(connection$hostname,
              connection$port,
              connection$client_id,
              connection$client_secret)

  if (nchar(token$access_token) == 40) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

api_post <- function(hostname, port=19999, path, ...) {
  require(httr)
  request_response <- POST(modify_url("https://example.looker.com",
                                               hostname=hostname,
                                               port=port,
                                               path=path),
                                    ...)
  if (check_response_status(request_response))
    return(request_response)
}

api_get <- function(hostname, port=19999, path) {
  require(httr)
  request_response <- GET(modify_url("https://example.looker.com",
                                              hostname=hostname,
                                              port=port,
                                              path=path),
                                   ...)
  if (check_response_status(request_response))
    return(request_response)
}

api_patch <- function(hostname, port=19999, path, ...) {
  require(httr)
  request_response <- PATCH(modify_url("https://example.looker.com",
                                                hostname=hostname,
                                                port=port,
                                                path=path),
                                     ...)
  if (check_response_status(request_response))
    return(request_response)
}

api_delete <- function(hostname, port=19999, path, ...) {
  require(httr)
  request_response <- DELETE(modify_url("https://example.looker.com",
                                                 hostname=hostname,
                                                 port=port,
                                                 path=path),
                                      ...)
  if (check_response_status(request_response))
    return(request_response)
}

check_response_status <- function(request_response) {
  status_code <- request_response$status_code
  if (status_code == 200) {
    message("Success: OK")
    return(TRUE)
  } else if (status_code == 204) {
    message("Success: No Content")
    return(TRUE)
  } else if (status_code == 400) {
    message("Failure: Bad Request")
    return(FALSE)
  } else if (response$status_code == 404) {
    message("Failure: Not Found")
    return(FALSE)
  } else {
    message("Unexpected response: ", response$status_code)
    return(FALSE)
  }
}
