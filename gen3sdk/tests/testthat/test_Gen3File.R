library(testthat)
library(readr)
library(jsonlite)
library(httr)
library(httptest)
library(gen3sdk)


GEN3_ENDPOINT="~/.gen3/endpoint.txt"
GEN3_AUTH_TOKEN="~/.gen3/auth_token.txt"
GEN3_FILE_GUID="~/.gen3/file_guid.txt"

endpoint <- read_file(GEN3_ENDPOINT)
auth_token <- read_file(GEN3_AUTH_TOKEN)
guid <- read_file(GEN3_FILE_GUID)
protocol <- "http"


test_that("Parameter type check", {
    expect_error(Gen3File(endpoint=5))
    expect_error(Gen3File(auth_provider=3.14))
})

test_that("Missing initialization parameter", {
    expect_error(Gen3File())
    expect_error(Gen3File(endpoint=endpoint))
})

with_mock_api({
    test_that("Mock test: get presigned url", {
        api_url <- paste(endpoint, "/user/data/download/", guid, sep="")
        expect_GET(
            GET(api_url, add_headers(Authorization = auth_token), query = list(protocol=protocol)),
            paste(api_url, "?protocol=", protocol, sep="")
        )
    })
})
