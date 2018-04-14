#' @title API Requests
#' @description EFS HTTP Requests
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param action A character string specifying the path to the API endpoint.
#' @param query A named list of query string parameters.
#' @param headers A list of headers to pass to the HTTP request.
#' @param body A list of body (JSON) arguments.
#' @param verbose A logical indicating whether to be verbose. Default is given by \code{options("verbose")}.
#' @param region A character string containing the AWS region. If missing, defaults to \dQuote{us-east-1}.
#' @param key A character string containing an AWS Access Key ID. See \code{\link[aws.signature]{locate_credentials}}.
#' @param secret A character string containing an AWS Secret Access Key. See \code{\link[aws.signature]{locate_credentials}}.
#' @param session_token A character string containing an AWS Session Token. See \code{\link[aws.signature]{locate_credentials}}.
#' @param \dots Additional arguments passed to \code{\link[httr]{POST}}.
#' @return A list
#' @importFrom aws.signature signature_v4_auth
#' @importFrom httr add_headers headers content warn_for_status http_status http_error GET POST PUT DELETE
#' @importFrom xml2 read_xml as_list
#' @importFrom jsonlite fromJSON
#' @export
efsHTTP <- 
function(
  verb = "GET",
  action = "/",
  query = NULL,
  headers = list(),
  body = NULL,
  verbose = getOption("verbose", FALSE),
  region = Sys.getenv("AWS_DEFAULT_REGION","us-east-1"),
  key = NULL, 
  secret = NULL,
  session_token = NULL,
  ...
) {
    # locate and validate credentials
    credentials <- locate_credentials(key = key, secret = secret, session_token = session_token, region = region, verbose = verbose)
    key <- credentials[["key"]]
    secret <- credentials[["secret"]]
    session_token <- credentials[["session_token"]]
    region <- credentials[["region"]]
    
    # generate request signature
    d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    url <- paste0("https://elasticfilesystem.",region,".amazonaws.com", action)
    Sig <- signature_v4_auth(
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
           session_token = session_token,
           verbose = verbose)
    # setup request headers
    headers[["x-amz-date"]] <- d_timestamp
    headers[["Authorization"]] <- Sig[["SignatureHeader"]]
    if (!is.null(session_token) && session_token != "") {
        headers[["x-amz-security-token"]] <- session_token
    }
    H <- do.call(add_headers, headers)
    
    # execute request
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
        attr(out, "request_canonical") <- Sig$CanonicalRequest
        attr(out, "request_string_to_sign") <- Sig$StringToSign
        attr(out, "request_signature") <- Sig$SignatureHeader
    } else {
        out <- try(jsonlite::fromJSON(cont, simplifyDataFrame = FALSE))
        if (inherits(out, "try-error")) {
            out <- xml2::as_list(xml2::read_xml(cont))
        }
        
    }
    return(out)
}
