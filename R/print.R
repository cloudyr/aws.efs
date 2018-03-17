print.aws_file_system <-
function(
  x,
  ...
) {
    cat(sprintf("File System:        %s (%s)%s\n", if (is.null(x$Name)) "[unnamed]" else x$Name, 
                                            x$FileSystemId,
                                            if (x$Encrypted) " (Encrypted)" else ""))
    cat(sprintf("CreationToken:      %s\n", x$CreationToken))
    cat(sprintf("Creation Time:      %s\n", as.POSIXct(x$CreationTime, origin = "1970-01-01")))
    cat(sprintf("PerformanceMode:    %s\n", x$PerformanceMode))
    cat(sprintf("# of Mount Targets: %s\n", x$NumberOfMountTargets))
    cat(sprintf("Size (bytes):       %s\n", x$SizeInBytes$Value))
    cat(sprintf("State:              %s\n", x$LifeCycleState))
    invisible(x)
}
