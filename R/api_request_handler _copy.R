api_get <- function(path) {
    url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
    cat("Request URL:", url, "\n")
    resp <- GET(url, authenticate(username, pwd, type = "ntlm"))

    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }

    parsed <- jsonlite::fromJSON(content(resp, "text"))

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
    url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
    cat("Request URL:", url, "\n")
    resp <- POST(url, authenticate(username, pwd, type = "ntlm"), body = list(query = query), encode = "json")
    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }
    parsed <- jsonlite::fromJSON(content(resp, "text"))

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

