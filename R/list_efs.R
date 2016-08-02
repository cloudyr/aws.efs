#' @title List EFS File System(s)
#' @description List EFS File System(s)
#' @param id Optionally, a character string specifying an EFS File System ID, perhaps as returned by \code{\link{create_efs}} to use to restrict results.
#' @param n Optionally, an integer specifying the number of results to return.
#' @param marker Optionally, a character string specifying a pagination marker returned by a previous result.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_DescribeFileSystems.html}{API Documentation}
#' @seealso \code{\link{create_efs}}, \code{\link{delete_efs}}
#' @export
efs_list <- function(id = NULL, n = NULL, marker = NULL, ...) {
    query <- list()
    if (!is.null(id)) {
        query[["FileSystemId"]] <- id
    }
    if (!is.null(n)) {
        query[["MaxItems"]] <- as.integer(n)
    }
    if (!is.null(n)) {
        query[["Marker"]] <- marker
    }
    efsHTTP(verb = "GET", path = "/2015-02-01/file-systems", query = query, ...)
}
