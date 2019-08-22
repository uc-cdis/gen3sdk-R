library(testthat)
library(readr)
library(jsonlite)
library(httr)
library(httptest)
library(gen3sdk)


GEN3_ENDPOINT="~/.gen3/endpoint.txt"
GEN3_AUTH_TOKEN="~/.gen3/auth_token.txt"
GEN3_BODY_JSON="~/.gen3/body.json"
GEN3_UUID="~/.gen3/uuid.txt"
GEN3_QUERY_TXT="~/.gen3/query.txt"

endpoint <- read_file(GEN3_ENDPOINT)
auth_token <- read_file(GEN3_AUTH_TOKEN)
json_parameter <- fromJSON(GEN3_BODY_JSON)
uuid <- read_file(GEN3_UUID)
query_txt <- read_file(GEN3_QUERY_TXT)
program <- "lorem"
project <- "ipsum"
node_type <- "_all"
fileformat <- "json"


test_that("Parameter type check", {
    expect_error(Gen3Submission(endpoint=5))
    expect_error(Gen3Submission(auth_provider=3.14))
})

test_that("Missing initialization parameter", {
    expect_error(Gen3Submission())
    expect_error(Gen3Submission(endpoint=endpoint))
})

with_mock_api({
    test_that("Mock test: create program", {
        api_url <- paste(endpoint, "/api/v0/submission/", sep="")
        json_body <- toJSON(json_parameter, auto_unbox = TRUE)
        expect_POST(
            POST(api_url, add_headers(Authorization = auth_token), body = json_body, encode = 'json'),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: create project", {
        api_url <- paste(endpoint, "/api/v0/submission/", program, sep="")
        json_body <- toJSON(json_parameter, auto_unbox = TRUE)
        expect_PUT(
            PUT(api_url, add_headers(Authorization = auth_token), body = json_body, encode = 'json'),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: delete project", {
        api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, sep="")
        expect_DELETE(
            DELETE(api_url, add_headers(Authorization = auth_token)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: delete program", {
        api_url <- paste(endpoint, "/api/v0/submission/", program, sep="")
        expect_DELETE(
            DELETE(api_url, add_headers(Authorization = auth_token)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get dictionary node", {
        api_url <- paste(endpoint, "/api/v0/submission/_dictionary/", node_type, sep="")
        expect_GET(
            GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get graphql schema", {
        api_url <- paste(endpoint, "/api/v0/submission/getschema", sep="")
        expect_GET(
            GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: submit record", {
        api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, sep="")
        json_body <- toJSON(json_parameter, auto_unbox = TRUE)
        expect_PUT(
            PUT(api_url, add_headers(Authorization = auth_token), body = json_body, encode = 'json'),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: delete record", {
        api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/entities/", uuid, sep="")
        expect_DELETE(
            DELETE(api_url, add_headers(Authorization = auth_token)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: export record", {
        api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/export", sep="")
        expect_GET(
            GET(api_url, add_headers(Authorization = auth_token), query = list(ids = uuid, format = fileformat)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: export node", {
        api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/export", sep="")
        expect_GET(
            GET(api_url, add_headers(Authorization = auth_token), query = list(node_label = node_type, format = fileformat)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: query", {
        variables <- ""
        api_url <- paste(endpoint, "/api/v0/submission/graphql", sep="")
        if(variables=="") {
            query <- paste('{"query": "', query_txt, '"}', sep='')
        } else {
            query <- paste('{"query": "', query_txt, '"variables": "', variables, '"}', sep="")
        }
        query_body <- fromJSON(query)
        json_body <- toJSON(query_body, auto_unbox = TRUE)
        expect_POST(
            POST(api_url, add_headers(Authorization = auth_token), body = json_body, encode = 'json'),
            api_url
        )
    })
})
