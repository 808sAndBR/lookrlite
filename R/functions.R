examplePath <- "https://example.looker.com/api/3.0"

CheckAlive <- function(hostname,
											 port = 19999) {
	requestResponse <- GET(modify_url(examplePath,
																		 hostname = hostname,
																		 port 		= port,
																		 path     = "alive"))
	return(CheckResponse(requestResponse))
}

ApiLogin <- function(hostname,
										 clientId,
										 clientSecret,
										 port = 19999) {
	
	# Check the instance at hostname:port/alive to make sure it can response
	if (!CheckAlive(hostname = hostname, 
									port = port)) {
		stop("No response from ", hostname, ":", port, "/alive")
	}
	message("Getting new token...")
	if (missing(clientId)) {
		clientId <- readline("Enter client ID: ")
	}
	if (missing(clientSecret)) {
		clientSecret <- readline("Enter client secret: ")
	}
	
	# Build a POST request, send to hostname:port/login
	authBody <- paste0("client_id=", clientId, "&client_secret=", clientSecret)
	requestResponse <- POST(modify_url(examplePath,
																		 hostname = hostname,
																		 port     = port,
																		 path     = "login"),
													body = authBody)
	
	# Parse token from JSON response
	token <- jsonlite::fromJSON(content(requestResponse, as="text"))
	
	# Set connection in R session as a list in the global environment
	SetConnection(hostname     = hostname,
								port         = port,
								clientId     = clientId,
								clientSecret = clientSecret,
								token        = token)
	
	# Check the access token to make sure it looks as expected
	if (!is.null(token["access_token"]) && nchar(token["access_token"]) == 40) {
		message("Login success")
		return(TRUE)
	}
}

ApiLogout <- function(hostname,
											port = 19999) {
	# Retrieve connection details from global environment
	connection <- GetConnection(hostname, port)
	
	# Build a DELETE request, send to hostname:port/logout to revoke token
	requestResponse <- ApiDelete(hostname = hostname,
															 port = port,
															 path = "logout",
															 add_headers(Authentication = connection["token"]))
	
	# Check response for success before returning
	# TODO (maxcorbin): Try to send a simple API request using old token
	# 									to make sure the token was revoked successfully
	if (CheckResponse(requestResponse)) {
		message("Logout success")
		return(requestResponse)
	}
}

SetConnection <- function(hostname,
													port = 19999,
													clientId,
													clientSecret,
													token) {
	
	# Assign the connection details to a list in the global environment
	assign(paste(hostname, port, "conn", sep="_"),
				 list(hostname = hostname,
				 		 port = port,
				 		 clientId = clientId,
				 		 clientSecret = clientSecret,
				 		 token = paste("token", token["access_token"]),
				 		 expiresAt = Sys.time() + as.numeric(token["expires_in"])),
				 envir=globalenv())
}

ExistsConnection <- function(hostname, port = 19999) {
	exists(paste(hostname, port, "conn", sep = "_"), envir = globalenv())
}

GetConnection <- function(hostname, port = 19999) {
	get(paste(hostname, port, "conn", sep = "_"), envir = globalenv())
}

CheckToken <- function(hostname, port = 19999) {
	# Check to ensure connection object is in global environment
	# before trying to retrieve it
	# TODO (maxcorbin): Make this automatic as part of GetConnection
	if (ExistsConnection(hostname = hostname, port = port)) {
		connection <- GetConnection(hostname = hostname, port = port)
	} else if (interactive()) {
		# If no connection exists && interactive() is true, establish connection
		ApiLogin(hostname = hostname, port = port)
	} else {
		stop("No valid connection found")
	}
	
	# Check the token for null-ness and whether it has been expired
	if (is.null(connection$token) || Sys.time() > connection["expiresAt"]) {
		# Get a new token if it's expired or null
		ApiLogin(hostname = connection["hostname"],
						 port = connection["port"],
						 clientId = connection["clientId"],
						 clientSecret = connection["clientSecret"])
	}
}

ApiPost <- function(hostname,
										path,
										port = 19999,
										...) {
	
	# Make sure the token is not expired
	CheckToken(hostname = hostname, port = port)
	
	# Build a POST request, send to hostname:port/<path>,
	# include additional arguments to POST in <...>
	requestResponse <- POST(modify_url(examplePath,
																		 hostname = hostname,
																		 port = port,
																		 path = path),
													...)
	
	# Check response for success before returning
	if (CheckResponse(requestResponse)) {
		return(requestResponse)
	}
}

ApiGet <- function(hostname, port = 19999, path, ...) {
	
	# Make sure the token is not expired
	CheckToken(hostname = hostname, port = port)
	
	# Build a GET request, send to hostname:port/<path>,
	# include additional arguments to GET in <...>
	requestResponse <- GET(modify_url(examplePath,
																		hostname = hostname,
																		port = port,
																		path = path),
												 ...)
	
	# Check response for success before returning
	if (CheckResponse(requestResponse)) {
		return(requestResponse)
	}
}

ApiPatch <- function(hostname, port = 19999, path, ...) {
	
	# Make sure the token is not expired
	CheckToken(hostname = hostname, port = port)
	
	# Build a PATCH request, send to hostname:port/<path>,
	# include additional arguments to PATCH in <...>
	requestResponse <- PATCH(modify_url(examplePath,
																			hostname = hostname,
																			port = port,
																			path = path),
													 ...)
	
	# Check response for success before returning
	if (CheckResponse(requestResponse)) {
		return(requestResponse)
	}
}

ApiDelete <- function(hostname,
											path,
											port = 19999,
											...) {
	# Make sure the token is not expired
	CheckToken(hostname = hostname, port = port)
	
	# Build a DELETE request, send to hostname:port/<path>,
	# include additional arguments to DELETE in <...>
	requestResponse <- DELETE(modify_url(examplePath,
																			 hostname = hostname,
																			 port = port,
																			 path = path),
														...)
	
	# Check response for success before returning
	if (CheckResponse(requestResponse)) {
		return(requestResponse)
	}
}

CheckResponse <- function(requestResponse) {
	status_code <- requestResponse$status_code
	if (status_code == 200) {
		message("Success: OK")
		return(TRUE)
	} else if (status_code == 204) {
		message("Success: No Content")
		return(TRUE)
	} else if (status_code == 400) {
		stop("Failure: Bad Request")
		return(FALSE)
	} else if (status_code == 404) {
		stop("Failure: Not Found")
		return(FALSE)
	} else {
		stop("Unexpected response: ", requestResponse$status_code)
		return(FALSE)
	}
}
