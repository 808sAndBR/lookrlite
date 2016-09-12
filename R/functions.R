check_alive <- function(hostname, port=19999) {
  require(httr)
  response <- GET(modify_url("https://example.looker.com",
                                   hostname=hostname,
                                   port=port,
                                   path="alive"))
  return(check_response_status(response))
}

api_login <- function(hostname, port=19999, client_id, client_secret) {
  require(httr)

  if (!check_alive(hostname=hostname, port=port))
    stop("No response from ", hostname, ":", port, "/alive")
  message("Getting new token...")
  if (missing(client_id))
    client_id <- readline("Enter client ID: ")
  if (missing(client_secret))
    client_secret <- readline("Enter client secret: ")

  auth_body <- paste0("client_id=", client_id, "&client_secret=", client_secret)
  auth_response <- api_post(hostname=hostname,
                            port=port,
                            path="login",
                            body = auth_body)

  token <- jsonlite::fromJSON(content(auth_response, as="text"))

  assign(paste0(hostname, port, "_conn"),
         list(hostname=hostname,
              port=port,
              client_id=client_id,
              client_secret=client_secret,
              token=token$access_token,
              expires_at=Sys.time()+token$expires_in),
         envir=globalenv())
  if (nchar(token$access_token) == 40) message("Success"); return(TRUE)
}

api_logout <- function(hostname, port=19999) {
  require(httr)

}

get_connection <- function(hostname, port=19999) {
  get(paste0(hostname, port, "_conn"), envir=globalenv())
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
  if (check_token(hostname=hostname, port=port)) {
    response <- POST(modify_url("https://example.looker.com",
                                hostname=hostname,
                                port=port,
                                path=path),
                     ...)
    return(check_responses_status(response))
  } else {
    message("You must login first")
    return(FALSE)
  }
}

api_get <- function(hostname, port=19999, path) {
  require(httr)
  if (check_token(hostname=hostname, port=port)) {
    response <- GET(modify_url("https://example.looker.com",
                               hostname=hostname,
                               port=port,
                               path=path),
                    ...)
    check_responses_status(response)
  } else {
    message("You must login first")
    return(FALSE)
  }
}

api_patch <- function(hostname, port=19999, path, ...) {
  require(httr)
  if (check_token(hostname=hostname, port=port)) {
    response <- PATCH(modify_url("https://example.looker.com",
                                 hostname=hostname,
                                 port=port,
                                 path=path),
                      ...)
    return(check_responses_status(response))
  } else {
    message("You must login first")
    return(FALSE)
  }
}

api_delete <- function(hostname, port=19999, path, ...) {
  require(httr)
  if (check_token(hostname=hostname, port=port)) {
    response <- DELETE(modify_url("https://example.looker.com",
                                  hostname=hostname,
                                  port=port,
                                  path=path),
                      ...)
    return(check_responses_status(response))
  } else {
    message("You must login first")
    return(FALSE)
  }
}

check_response_status <- function(response) {
  if (response$status_code == 200) {
    message("Success: OK")
    return(TRUE)
  } else
  if (response$status_code == 204) {
    message("Success: No Content")
    return(TRUE)
  } else
  if (response$status_code == 400) {
    message("Failure: Bad Request")
    return(FALSE)
  } else
  if (response$status_code == 404) {
    message("Failure: Not Found")
    return(FALSE)
  }
}
