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
    auth <- Gen3AuthHelper(endpoint=general, refresh_file="path/to/library/credentials.json")
    expect_error(auth$get_access_token())
})

test_that("Valid authenication", {
    auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="path/to/library/credentials.json")
    refresh_data <- fromJSON(auth$refresh_file)
    refresh_token <- toJSON(refresh_data, auto_unbox = TRUE)
    auth_url = paste(auth$endpoint, "/user/credentials/cdis/access_token", sep="")
    access_token_json <- POST(auth_url, body=refresh_token, encode = 'json')
    expect_equal(access_token_json$status, 200)
})
