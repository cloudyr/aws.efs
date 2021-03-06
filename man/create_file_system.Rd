% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_efs.R
\name{create_file_system}
\alias{create_file_system}
\title{Create EFS File System}
\usage{
create_file_system(token = NULL, mode = c("generalPurpose", "maxIO"),
  encrypted = NULL, kms_key = NULL, ...)
}
\arguments{
\item{token}{An optional character string specifying a token to ensure idempotency. Must be between 1 and 64 characters in length.}

\item{mode}{A character string specifying the \dQuote{Performance Mode} for the file system. \dQuote{generalPurpose} (the default) is appropriate in most situations.}

\item{encrypted}{Optionally, a logical indicating whether to create an encrypted file system.}

\item{kms_key}{Optionally, an AWS KMS key ID.}

\item{\dots}{Additional arguments passed to \code{\link{efsHTTP}}.}
}
\value{
A list.
}
\description{
Create an EFS File System
}
\references{
\href{http://docs.aws.amazon.com/efs/latest/ug/API_CreateFileSystem.html}{API Documentation}
}
\seealso{
\code{\link{delete_efs}}, \code{\link{create_mount}}
}
