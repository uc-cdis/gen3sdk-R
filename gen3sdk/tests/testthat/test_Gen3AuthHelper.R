library(testthat)
library(readr)
library(jsonlite)
library(httr)
library(httptest)
library(gen3sdk)


GEN3_ENDPOINT = "~/.gen3/endpoint.txt"
GEN3_CREDENTIALS = "~/.gen3/credentials.json"

endpoint <- read_file(GEN3_ENDPOINT)


test_that("Parameter type check", {
    expect_error(Gen3AuthHelper(endpoint = 5))
    expect_error(Gen3AuthHelper(refresh_file = 3.14))
})

test_that("Parameter initialization", {
    auth <- Gen3AuthHelper(endpoint = "dummy", refresh_file = "string")
    expect_equal(auth$endpoint, "dummy")
    expect_equal(auth$refresh_file, "string")
})

test_that("Missing parameter", {
    expect_error(Gen3AuthHelper()$get_access_token())
    expect_error(Gen3AuthHelper(endpoint = endpoint)$get_access_token())
    expect_error(Gen3AuthHelper(refresh_file = "dummy.json")$get_access_token())
})

with_mock_api({
    test_that("Mock test: get access token", {
        refresh_data <- fromJSON(GEN3_CREDENTIALS)
        refresh_token <- toJSON(refresh_data, auto_unbox = TRUE)
        auth_url = paste(endpoint, "/user/credentials/cdis/access_token", sep = "")
        expect_POST(
            POST(auth_url, body = refresh_token, encode = 'json'),
            auth_url
        )
    })
})
