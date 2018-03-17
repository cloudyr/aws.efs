get_sgid <- function(x) {
    # copied from aws.ec2 package
    if (inherits(x, "ec2_security_group")) {
        return(x$groupId[[1]])
    } else if (is.character(x)) {
        return(x)
    }     
}

get_subnetid <- function(x) {
    # copied from aws.ec2 package
    if (is.character(x)) {
        return(x)
    } else if (inherits(x, "ec2_subnet")) {
        return(x$subnetId[[1]])
    }
}


get_file_system_id <- function(x) {
    UseMethod("get_file_system_id")
}

get_file_system_id.default <- function(x) {
    x
}

get_file_system_id.character <- function(x) {
    x
}

get_file_system_id.aws_file_system <- function(x) {
    x$FileSystemId
}

