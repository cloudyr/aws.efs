#' @rdname tags
#' @title Get/Add/Delete EFS Tags
#' @description Get, add, or delete tags on an EFS file system
#' @param id A character string specifying an EFS File System ID, perhaps as returned by \code{\link{create_efs}} or \code{\link{efs_list}}.
#' @param tags For \code{add_tags}, a named character vector specifying tag keys (as names) and values to add to the EFS. For \code{delete_tags}, a character vector of tag keys to delete.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references
#'   \href{http://docs.aws.amazon.com/efs/latest/ug/API_CreateTags.html}{API Documentation: CreateTags}
#'   \href{http://docs.aws.amazon.com/efs/latest/ug/API_DescribeTags.html}{API Documentation: DescribeTags}
#'   \href{http://docs.aws.amazon.com/efs/latest/ug/API_DeleteTags.html}{API Documentation: DeleteTags}
#' @seealso \code{\link{create_efs}}
#' @export
add_efs_tags <- function(id, tags, ...) {
    b <- list()
    b[["tags"]] <- list()
    for (i in seq_along(tags)) {
        b[["tags"]][[i]] <- list(Key = names(tags)[i], Value = tags[i])
    }
    id <- get_file_system_id(id)
    efsHTTP(verb = "POST", action = paste0("/2015-02-01/tags/", id), body = b, ...)
}

#' @rdname tags
#' @export
delete_efs_tags <- function(id, tags, ...) {
    b <- list()
    b[["TagKeys"]] <- tags
    id <- get_file_system_id(id)
    efsHTTP(verb = "DELETE", action = paste0("/2015-02-01/tags/", id), body = b, ...)
}

#' @rdname tags
#' @export
get_efs_tags <- function(id, ...) {
    id <- get_file_system_id(id)
    out <- efsHTTP(verb = "GET", action = paste0("/2015-02-01/tags/", id), ...)
    structure(out$Tags, Marker = out$Marker, NextMarker = out$NextMarker)
}
