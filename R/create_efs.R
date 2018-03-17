#' @title Create EFS File System
#' @description Create an EFS File System
#' @param token An optional character string specifying a token to ensure idempotency. Must be between 1 and 64 characters in length.
#' @param mode A character string specifying the \dQuote{Performance Mode} for the file system. \dQuote{generalPurpose} (the default) is appropriate in most situations.
#' @param encrypted Optionally, a logical indicating whether to create an encrypted file system.
#' @param kms_key Optionally, an AWS KMS key ID.
#' @param \dots Additional arguments passed to \code{\link{efsHTTP}}.
#' @return A list.
#' @references \href{http://docs.aws.amazon.com/efs/latest/ug/API_CreateFileSystem.html}{API Documentation}
#' @seealso \code{\link{delete_efs}}, \code{\link{create_mount}}
#' @export
create_file_system <- 
function(
  token = NULL,
  mode = c("generalPurpose", "maxIO"),
  encrypted = NULL,
  kms_key = NULL,
  ...
) {
    b <- list()
    b[["PerformanceMode"]] <- match.arg(mode)
    stopifnot(nchar(token) %in% 1:64)
    b[["CreationToken"]] <- token
    if (!is.null(kms_key)) {
        b[["KmsKeyId"]] <- kms_key
    }
    if (!is.null(encrypted)) {
        b[["Encrypted"]] <- encrypted
    }
    out <- efsHTTP(verb = "POST", action = "/2015-02-01/file-systems", body = b, ...)
    structure(out, class = "aws_file_system")
}
