#' @title Create EFS Mount Target
#' @description Create an EFS Mount Target
#' @details This function creates a mount target for an EFS File System so that it can mounted to one or more EC2 instances. All instances within an AWS availability zone share a mount target. Mount targets are subnet-specific, so one mount target should be created for each subnet.
#' @param id A character string specifying an EFS File System ID, perhaps as returned by \code{\link{create_efs}} or \code{\link{efs_list}}.
#' @param ip An object of class \dQuote{ec2_ip} or an allocationId (for a VPC IP).
#' @param sgroup A character vector specifying one or more Security Group IDs, or (a list of) objects of class \dQuote{ec2_security_group}. No more than 5 security groups can be specified. These values can be modified later using \code{\link{replace_mount_sgroups}}.
#' @param subnet A character string containing the name of a subnet, or an object of class \dQuote{ec2_subnet}.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_CreateMountTarget.html}{API Documentation}
#' @seealso \code{\link{create_efs}}, \code{\link{delete_mount}}, \code{\link{get_mount_sgroups}}
#' @importFrom utils head
#' @export
create_mount <- function(id, ip, sgroup, subnet, ...) {
    if (is.list(sgroup)) {
        sgroup <- sapply(sgroup, get_sgid)
    }
    id <- get_file_system_id(id)
    b <- list(FileSystemId = id,
              IpAddress = ip,
              SecurityGroups = head(sgroup, 5),
              SubnetId = get_subnetid(subnet)
              )
    efsHTTP(verb = "POST", action = "/2015-02-01/mount-targets", body = b, ...)
}
