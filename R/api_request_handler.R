api_get <- function(path) {
    url <- httr::modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
    cat("Request URL:", url, "\n")
    resp <- httr::GET(url, httr::authenticate(username, pwd, type = "ntlm"))

    if (httr::http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }

    parsed <- jsonlite::fromJSON(httr::content(resp, "text"))

    structure(
      list(
        content = parsed,
        path = path,
        response = resp
      ),
      class = "api_get"
    )
}

print.api_get <- function(x, ...) {
  cat("<TFS get", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

api_post <- function(path, query) {
    url <- httr::modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
    cat("Request URL:", url, "\n")
    resp <- httr::POST(url, httr::authenticate(username, pwd, type = "ntlm"), body = list(query = query), encode = "json")
    if (httr::http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"))

    structure(
      list(
        content = parsed,
        path = path,
        response = resp
      ),
      class = "api_post"
    )
}

print.api_post <- function(x, ...) {
  cat("<TFS post", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

