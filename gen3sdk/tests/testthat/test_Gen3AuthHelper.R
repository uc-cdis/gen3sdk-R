library(testthat)
library(stringr)
library(readr)
library(gen3sdk)
library(jsonlite)
library(httr)

#endpoint <- read_file("../../../../../sandbox/endpoint.txt")
endpoint <- read_file("path/to/library/endpoint.txt")
general <- "https://nci-crdc-demo.datacommons.io"

test_that("Parameter type check", {
    expect_error(Gen3AuthHelper(endpoint=5))
    expect_error(Gen3AuthHelper(refresh_file=3.14))
})

test_that("Parameter initialization", {
    auth <- Gen3AuthHelper(endpoint="dummy", refresh_file="string")
    expect_equal(auth$endpoint, "dummy")
    expect_equal(auth$refresh_file, "string")
})

test_that("Missing Parameter", {
    expect_error(Gen3AuthHelper()$get_access_token())
    expect_error(Gen3AuthHelper(endpoint=endpoint)$get_access_token())
    expect_error(Gen3AuthHelper(refresh_file="dummy.json")$get_access_token())
})

test_that("Loading bad JSON credentials file", {
    auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="nonexist.json")
    expect_error(auth$get_access_token())
})

test_that("Fail to authenticate", {
    #auth <- Gen3AuthHelper(endpoint=general, refresh_file="../../../../../sandbox/credentials.json")
    auth <- Gen3AuthHelper(endpoint=general, refresh_file="path/to/library/credentials.json")
    expect_error(auth$get_access_token())
})

test_that("Valid authenication", {
    #auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="../../../../../sandbox/credentials.json")
    auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="path/to/library/credentials.json")
    ret_val <- auth$get_access_token()
    expect_equal(ret_val$status, 200)
})

test_that("Auth value", {
    #auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="../../../../../sandbox/credentials.json")
    auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="path/to/library/credentials.json")
    ret_val <- auth$get_access_token()
    auth_val <- auth$get_auth_value(ret_val)
    expect_true(is.character(auth_val))
})
