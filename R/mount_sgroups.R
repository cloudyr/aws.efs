#' @rdname sgroups
#' @rdname List/Replace Mount Target Security Groups
#' @title List or replace Mount Target security groups
#' @description \code{get_mount_sgroups} lists security groups for an EFS Mount Target. \code{replace_mount_sgroups} replaces them.
#' @param mount A character string specifying an EFS Mount Target ID, perhaps as returned by \code{\link{create_mount}} or \code{\link{mount_list}}.
#' @param sgroup A character vector specifying one or more Security Group IDs, or (a list of) objects of class \dQuote{ec2_security_group}. No more than 5 security groups can be specified.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_DescribeMountTargetSecurityGroups.html}{API Documentation: DescribeMountTargetSecurityGroups}
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_ModifyMountTargetSecurityGroups.html}{API Documentation: ModifyMountTargetSecurityGroups}
#' @seealso \code{\link{create_mount}}, \code{\link{delete_mount}}, \code{\link{mount_list}}
#' @export
get_mount_sgroups <- function(mount, ...) {
    efsHTTP(verb = "GET", path = paste0("/2015-02-01/mount-targets/", mount, "/security-groups"), ...)
}

#' @rdname sgroups
#' @export
replace_mount_sgroups <- function(mount, sgroup, ...) {
    if (is.list(sgroup)) {
        sgroup <- sapply(sgroup, get_sgid)
    }
    b <- list(SecurityGroups = sgroup)
    efsHTTP(verb = "PUT", path = paste0("/2015-02-01/mount-targets/", mount, "/security-groups"), body = b, ...)
}
