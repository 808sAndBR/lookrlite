require(httr)
context("API Login")

host <- list(name="learn.looker.com", port=19999, hash=NULL)
client <- list(id="gnKZH3cVswj6X5pdqp2X", secret="Xhmf3knFf7chmCTwGX7JmXqR")
host["hash"] <- hash_connection(hostname=host["name"], port=host["port"])

test_that("can login with hostname", {
  request_response <- api_login(hostname=host["name"],
                                port=host["port"],
                                client_id=client["id"],
                                client_secret=client["secret"])
  expect_that(check_response_status, is_true())
})
test_that("can login with hostname and port", {
  request_response <- api_login(hostname=host["name"],
                                port=host["port"],
                                client_id=client["id"],
                                client_secret=client["secret"])
})
test_that("can login with connection hash", {
  request_response <- api_login(hash=host["hash"],
                                client_id=client["id"],
                                client_secret=client["secret"])
})
test_that("")
