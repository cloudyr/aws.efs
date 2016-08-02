#' @title Create EFS File System
#' @description Create an EFS File System
#' @param mode A character string specifying the \dQuote{Performance Mode} for the file system. \dQuote{generalPurpose} (the default) is appropriate in most situations.
#' @param token An optional character string specifying a token to ensure idempotency. Must be between 1 and 64 characters in length.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_CreateFileSystem.html}{API Documentation}
#' @seealso \code{\link{delete_efs}}, \code{\link{create_mount}}
#' @export
create_efs <- function(mode = c("generalPurpose", "maxIO"), token = NULL, ...) {
    b <- list()
    b[["PerformanceMode"]] <- match.arg(mode)
    if (!is.null(token)) {
        stopifnot(nchar(token) %in% 1:64)
        b[["token"]] <- token
    }
    efsHTTP(verb = "POST", path = "/2015-02-01/file-systems", body = b, ...)
}
