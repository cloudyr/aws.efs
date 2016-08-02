#' @title List EFS Mount Target(s)
#' @description List EFS Mount Target(s)
#' @param id Optionally, a character string specifying an EFS File System ID, perhaps as returned by \code{\link{create_efs}} to use to restrict results.
#' @param mount Optionally, a character string specifying an EFS Mount Target ID, perhaps as returned by \code{\link{create_mount}} to use to restrict results.
#' @param n Optionally, an integer specifying the number of results to return.
#' @param marker Optionally, a character string specifying a pagination marker returned by a previous result.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_DescribeFileSystems.html}{API Documentation}
#' @seealso \code{\link{create_mount}}, \code{\link{delete_mount}}, \code{\link{efs_list}}
#' @export
mount_list <- function(id = NULL, mount = NULL, n = NULL, marker = NULL, ...) {
    query <- list()
    if (!is.null(id)) {
        query[["FileSystemId"]] <- id
    }
    if (!is.null(id)) {
        query[["MountTargetId"]] <- mount
    }
    if (!is.null(n)) {
        query[["MaxItems"]] <- as.integer(n)
    }
    if (!is.null(n)) {
        query[["Marker"]] <- marker
    }
    efsHTTP(verb = "GET", path = "/2015-02-01/mount-targets", query = query, ...)
}
