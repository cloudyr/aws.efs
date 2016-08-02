library("testthat")
library("aws.efs")

if (Sys.getenv("AWS_ACCESS_KEY_ID") != "") {
    #test_check("aws.efs", filter = "authenticated")
}

#test_check("aws.efs", filter = "public")
