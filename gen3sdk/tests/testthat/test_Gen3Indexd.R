library(testthat)
library(readr)
library(jsonlite)
library(httr)
library(httptest)
library(gen3sdk)


GEN3_ENDPOINT="~/.gen3/endpoint.txt"
GEN3_AUTH_TOKEN="~/.gen3/auth_token.txt"
GEN3_FILE_GUID="~/.gen3/file_guid.txt"
GEN3_HASH="~/.gen3/hash.txt"
GEN3_IDS="~/.gen3/ids.txt"
GEN3_DIDS="~/.gen3/dids.txt"
GEN3_BODY_JSON="~/.gen3/body.json"
GEN3_REV="~/.gen3/rev.txt"

endpoint <- read_file(GEN3_ENDPOINT)
auth_token <- read_file(GEN3_AUTH_TOKEN)
body <- fromJSON(GEN3_BODY_JSON)
guid <- read_file(GEN3_FILE_GUID)
hash <- read_file(GEN3_HASH)
ids <- read_file(GEN3_IDS)
dids <- read_file(GEN3_DIDS)
rev <- read_file(GEN3_REV)
size <- 0


test_that("Parameter type check", {
    expect_error(Gen3Indexd(endpoint=5))
    expect_error(Gen3Indexd(auth_provider=3.14))
})

test_that("Missing initialization parameter", {
    expect_error(Gen3Indexd())
    expect_error(Gen3Indexd(endpoint=endpoint))
})

with_mock_api({
    test_that("Mock test: get system status", {
        api_url <- paste(endpoint, "/index/_status", sep="")
        expect_GET(
            output <- GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get system status", {
        api_url <- paste(endpoint, "/index/_version", sep="")
        expect_GET(
            output <- GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get system stats", {
        api_url <- paste(endpoint, "/index/_stats", sep="")
        expect_GET(
            output <- GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get global guid", {
        api_url <- paste(endpoint, "/index/", guid, sep="")
        expect_GET(
            output <- GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get global urls", {
        api_url <- paste(endpoint, "/index/urls", sep="")
        expect_GET(
            GET(api_url, query = list(size=size, hash=hash, ids=ids)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: post index", {
        body$authz <- list(body$authz)
        json_body <- toJSON(body, auto_unbox = TRUE)
        api_url <- paste(endpoint, "/index/index", sep="")
        expect_POST(
            POST(api_url, add_headers(Authorization = auth_token), content_type('application/json'), body = json_body, encode = 'json'),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get index", {
        urls_meta=NULL
        meta=NULL
        uploader=NULL
        urls=NULL
        acl=NULL
        authz=NULL
        negate_params=NULL
        api_url <- paste(endpoint, "/index/index", sep="")
        expect_GET(
            GET(api_url, query = list(urls_metadata=urls_meta, metadata=meta, size=size, hash=hash, 
                uploader=uploader, ids=ids, urls=urls, acl=acl, authz=authz, negate_params=negate_params)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get index guid", {
        api_url <- paste(endpoint, "/index/index/", guid, sep="")
        expect_GET(
            output <- GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: post index guid", {
        body$authz <- list(body$authz)
        json_body <- toJSON(body, auto_unbox = TRUE)
        api_url <- paste(endpoint, "/index/index/", guid, sep="")
        expect_POST(
            POST(api_url, content_type("application/json"), add_headers(Authorization = auth_token), body = json_body),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: put index guid", {
        body$authz <- list(body$authz)
        body$urls <- list(body$urls)
        json_body <- toJSON(body, auto_unbox = TRUE)
        api_url <- paste(endpoint, "/index/index/", guid, sep="")
        expect_PUT(
            PUT(api_url, add_headers(Authorization = auth_token), content_type("application/json"), query = list(rev = rev), body = json_body),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: delete index guid", {
        api_url <- paste(endpoint, "/index/index/", guid, sep="")
        expect_DELETE(
            DELETE(api_url, add_headers(Authorization = auth_token), query = list(rev = rev)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: post bulk documents", {
        api_url <- paste(endpoint, "/index/bulk/documents", sep="")
        expect_POST(
            POST(api_url, body = dids, encode = 'json'),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get index guid latest", {
        has_version=TRUE
        api_url <- paste(endpoint, "/index/", guid, "/latest", sep="")
        expect_GET(
            GET(api_url, query = list(has_version=has_version)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get index guid latest", {
        has_version=TRUE
        api_url <- paste(endpoint, "/index/", guid, "/latest", sep="")
        expect_GET(
            GET(api_url, query = list(has_version=has_version)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get index guid versions", {
        api_url <- paste(endpoint, "/index/", guid, "/versions", sep="")
        expect_GET(
            GET(api_url),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get query urls", {
        exclude=""
        include=""
        versioned=FALSE
        limit=100
        offset=0
        api_url <- paste(endpoint, "/_query_urls/q", sep="")
        expect_GET(
            GET(api_url, query = list(exclude=exclude, include=include, versioned=versioned, limit=limit, offset=offset)),
            api_url
        )
    })
})

with_mock_api({
    test_that("Mock test: get query urls metatdata", {
        key="key"
        value="value"
        url=""
        versioned=FALSE
        limit=100
        offset=0
        api_url <- paste(endpoint, "/_query_urls/metdata/q", sep="")
        expect_GET(
            GET(api_url, query = list(key=key, value=value, url=url, versioned=versioned, limit=limit, offset=offset)),
            api_url
        )
    })
})

