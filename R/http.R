#' @title API Requests
#' @description EFS HTTP Requests
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param action A character string specifying the path to the API endpoint.
#' @param query A named list of query string parameters.
#' @param body A list of body (JSON) arguments.
#' @param region A character string containing the AWS region. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string containing an AWS Secret Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token A character string containing an AWS Session Token. See \code{\link[aws.signature]{locate_credentials}}.
#' @param ... Additional arguments passed to \code{\link[httr]{POST}}.
#' @return A list
#' @importFrom aws.signature signature_v4_auth
#' @importFrom httr add_headers headers content warn_for_status http_status http_error GET POST PUT DELETE
#' @importFrom xml2 read_xml as_list
#' @importFrom jsonlite fromJSON
#' @export
efsHTTP <- 
function(verb = "GET",
         action = "/",
         query = NULL,
         body = NULL,
         region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"), 
         key = NULL, 
         secret = NULL,
         session_token = NULL, 
         ...
) {
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    url <- paste0("https://elasticfilesystem.",region,".amazonaws.com", action)
    S <- signature_v4_auth(
           datetime = d_timestamp,
           region = region,
           service = "elasticfilesystem",
           verb = verb,
           action = action,
           query_args = query,
           canonical_headers = list(host = paste0("elasticfilesystem.",region,".amazonaws.com"),
                                    `x-amz-date` = d_timestamp),
           request_body = if (length(body)) jsonlite::toJSON(body, auto_unbox = TRUE) else "",
           key = key, 
           secret = secret,
           session_token = session_token)
    headers <- list(`x-amz-date` = d_timestamp, 
                    Authorization = S$SignatureHeader)
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
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
        if (!http_error(r)) {
            return(TRUE)
        }
    }
    cont <- content(r, "text", encoding = "UTF-8")
    if (http_error(r)) {
        warn_for_status(r)
        h <- headers(r)
        out <- try(structure(jsonlite::fromJSON(cont), headers = h, class = "aws_error"))
        if (inherits(out, "try-error")) {
            out <- xml2::as_list(xml2::read_xml(cont))
        }
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
    } else {
        out <- try(jsonlite::fromJSON(cont, simplifyDataFrame = FALSE))
        if (inherits(out, "try-error")) {
            out <- xml2::as_list(xml2::read_xml(cont))
        }
        
    }
    return(out)
}
