#' @title Delete EFS File System
#' @description Delete an EFS File System
#' @param id A character string specifying an EFS File System ID, perhaps as returned by \code{\link{create_efs}} or \code{\link{efs_list}}.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @details You cannot delete a File System that is in use, meaning has any mount targets. The mount targets must be deleted first for this operation to succeed, perhaps using \code{\link{delete_mount}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_DeleteFileSystem.html}{API Documentation}
#' @seealso \code{\link{create_efs}}, \code{\link{delete_mount}}
#' @export
delete_efs <- function(id, ...) {
    efsHTTP(verb = "DELETE", path = paste0("/2015-02-01/file-systems/", id), ...)
}
