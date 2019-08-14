library(testthat)
library(stringr)
library(readr)
library(gen3sdk)
library(jsonlite)
library(httr)

#endpoint <- read_file("../../../../sandbox/endpoint.txt")
endpoint <- read_file("path/to/library/endpoint.txt")
#auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="../../../../sandbox/credentials.json")
auth <- Gen3AuthHelper(endpoint=general, refresh_file="path/to/library/credentials.json")
general <- "https://nci-crdc-demo.datacommons.io"
#guid <- read_file("../../../../sandbox/file_guid.txt")
guid <- "<GUID>"

test_that("Parameter type check", {
    expect_error(Gen3File(endpoint=5))
    expect_error(Gen3File(auth_provider=3.14))
})

test_that("Missing Initialization Parameter", {
    expect_error(Gen3File())
    expect_error(Gen3File(endpoint=endpoint))
    expect_error(Gen3File(auth_provider=auth))
})

test_that("Missing GUID Parameter", {
    file <- Gen3File(endpoint=endpoint, auth_provider=auth)
    expect_error(file$get_presigned_url())
})

test_that("Fail to authenticate", {
    file <- Gen3File(endpoint=endpoint, auth_provider=auth)
    expect_error(auth$get_presigned_url("60637"))
})

test_that("Valid authenication", {
    file <- Gen3File(endpoint=endpoint, auth_provider=auth)
    ret_val <- file$get_presigned_url(guid=guid)
    expect_equal(ret_val$status, 200)
})