api_get <- function(path) {
    url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
    cat("Request URL:", url, "\n")
    resp <- GET(url, authenticate("TEN\\U6033371", "Monday.1234567", type = "ntlm"))

    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }

    jsonlite::fromJSON(content(resp, "text"))
}

api_post <- function(path, query) {
    url <- modify_url("http://elite-tfsapp.elitecorp.com:8080", path = path)
    cat("Request URL:", url, "\n")
    resp <- POST(url, authenticate("TEN\\U6033371", "Monday.1234567", type = "ntlm"), body = list(query = query), encode = "json")
    if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }
    jsonlite::fromJSON(content(resp, "text"))
}


