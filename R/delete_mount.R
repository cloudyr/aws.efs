#' @title Delete EFS Mount Target
#' @description Delete an EFS Mount Target
#' @param mount A character string specifying an EFS Mount Target ID, perhaps as returned by \code{\link{create_mount}} or \code{\link{mount_list}}.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_DeleteMountTarget.html}{API Documentation}
#' @seealso \code{\link{create_efs}}, \code{\link{delete_mount}}, \code{\link{mount_list}}, \code{\link{get_mount_sgroups}}
#' @export
delete_mount <- function(mount, ...) {
    efsHTTP(verb = "DELETE", action = paste0("/2015-02-01/mount-targets/", mount), ...)
}
