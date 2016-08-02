#' @title API Requests
#' @description EFS HTTP Requests
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param action A character string specifying the path to the API endpoint.
#' @param query A named list of query string parameters.
#' @param body A list of body (JSON) arguments.
#' @param region A character string containing the AWS region. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. If missing, defaults to value stored in environment variable \dQuote{AWS_ACCESS_KEY_ID}.
#' @param secret A character string containing an AWS Secret Access Key.  If missing, defaults to value stored in environment variable \dQuote{AWS_SECRET_ACCESS_KEY}.
#' @param version A character string specifying an API version. Default is \dQuote{2015-04-13}.
#' @param ... Additional arguments passed to \code{\link[httr]{POST}}.
#' @return A list
#' @importFrom aws.signature signature_v4_auth
#' @importFrom httr add_headers headers content warn_for_status http_status http_error GET POST PUT DELETE
#' @export
efsHTTP <- function(verb = "GET",
                    action = "/",
                    query = list(), 
                    body = list(),
                    region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
                    key = Sys.getenv("AWS_ACCESS_KEY_ID"), 
                    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"), 
                    version = "2015-02-01",
                    ...) {
    query$Version <- version
    url <- paste0("https://elasticfilesystem.",region,".amazonaws.com")
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    S <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "elasticfilesystem",
           verb = verb,
           action = action,
           query_args = query,
           canonical_headers = list(host = paste0("elasticfilesystem.",region,".amazonaws.com"),
                                    `X-Amz-Date` = d_timestamp,
                                    `X-Amz-Target` = paste0("elasticfilesystem_", gsub("-", "", version), action)),
           request_body = "",
           key = key, secret = secret)
    H <- add_headers(`X-Amz-Date` = d_timestamp, 
                     `X-Amz-Target` = paste0("elasticfilesystem_", gsub("-", "", version), action),
                     Authorization = S$SignatureHeader)
    if (verb == "GET") {
        if (length(query)) {
            r <- GET(url, H, body = body, encode = "json", query = query, ...)
        } else {
            r <- GET(url, H, body = body, encode = "json", ...)
        }
    } else if (verb == "POST") {
        if (length(query)) {
            r <- POST(url, H, body = body, encode = "json", query = query, ...)
        } else {
            r <- POST(url, H, body = body, encode = "json", ...)
        }
    } else if (verb == "POST") {
        if (length(query)) {
            r <- PUT(url, H, body = body, encode = "json", query = query, ...)
        } else {
            r <- PUT(url, H, body = body, encode = "json", ...)
        }
    } else if (verb == "DELETE") {
        if (length(query)) {
            r <- DELETE(url, H, encode = "json", query = query, ...)
        } else {
            r <- DELETE(url, H, encode = "json", ...)
        }
    }
    out <- structure(content(r, "text"))
    return(out)
}
