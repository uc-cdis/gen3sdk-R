library(testthat)
library(stringr)
library(readr)
library(gen3sdk)
library(jsonlite)
library(httr)

# auth <- Gen3AuthHelper(endpoint=endpoint, refresh_file="../../../../sandbox/credentials.json")
# endpoint <- read_file("../../../../sandbox/endpoint.txt")
# hash <- read_file("../../../../sandbox/hash.txt")
# post_index <- fromJSON("../../../../sandbox/index_post_index.txt")
# post_index_guid_body <- fromJSON("../../../../sandbox/post_index_guid_body.txt")
# put_index_guid_body <- fromJSON("../../../../sandbox/put_index_guid_body.txt")
# guid <- read_file("../../../../sandbox/indexd_guid.txt")
hash <- read_file("path/to/library/hash.txt")
post_index <- fromJSON("path/to/library/index_post_index.txt")
post_index_guid_body <- fromJSON("path/to/library/post_index_guid_body.txt")
put_index_guid_body <- fromJSON("path/to/library/put_index_guid_body.txt")
endpoint <- read_file("path/to/library/endpoint.txt")
auth <- Gen3AuthHelper(endpoint=general, refresh_file="path/to/library/credentials.json")
general <- "https://nci-crdc-demo.datacommons.io"
guid <- "<GUID>"

ind <- Gen3Indexd(endpoint=endpoint, auth_provider=auth)

test_that("Parameter type check", {
    expect_error(Gen3Indexd(endpoint=5))
    expect_error(Gen3Indexd(auth_provider=3.14))
})

test_that("Missing Initialization Parameter", {
    expect_error(Gen3Indexd())
    expect_error(Gen3Indexd(endpoint=endpoint))
    expect_error(Gen3Indexd(auth_provider=auth))
})

test_that("System Status", {
    ret_val <- ind$get_system_status()
    expect_equal(ret_val$status, 200)
})

test_that("System Version", {
    ret_val <- ind$get_system_version()
    expect_equal(ret_val$status, 200)
})

test_that("System Stats", {
    ret_val <- ind$get_system_stats()
    expect_equal(ret_val$status, 200)
})

test_that("Get global GUID", {
    ret_val <- ind$get_global_guid(guid)
    expect_equal(ret_val$status, 200)
})

test_that("Get global urls: no parameters", {
    ret_val <- ind$get_global_urls()
    expect_equal(ret_val$status, 400)
})

test_that("Get global urls: size", {
    ret_val <- ind$get_global_urls(size=2)
    expect_equal(ret_val$status, 200)
})

test_that("Get global urls: hash", {
    ret_val <- ind$get_global_urls(hash=hash)
    expect_equal(ret_val$status, 200)
})

test_that("Get global urls: ids", {
    ret_val <- ind$get_global_urls(ids=guid)
    expect_equal(ret_val$status, 200)
})

test_that("Get global urls: size and ids", {
    ret_val <- ind$get_global_urls(size=1, ids=guid)
    expect_equal(ret_val$status, 200)
})

test_that("Post index", {
    ret_val <- ind$post_index(post_index)
    expect_equal(ret_val$status, 200)
})

test_that("Get index: no params", {
    ret_val <- ind$get_index()
    expect_equal(ret_val$status, 200)
})

test_that("Get index: size", {
    ret_val <- ind$get_index(size=1)
    expect_equal(ret_val$status, 200)
})

test_that("Get index: hash", {
    ret_val <- ind$get_index(hash=hash)
    expect_equal(ret_val$status, 200)
})

test_that("Get index: hash", {
    ret_val <- ind$get_index(size=1, ids=guid)
    expect_equal(ret_val$status, 200)
})

test_that("Get index GUID", {
    ret_val <- ind$get_index_guid(guid)
    expect_equal(ret_val$status, 200)
})

test_that("Post index GUID", {
    output <- ind$post_index(post_index)
    new_output <- content(output)
    post_guid <- new_output$did
    ret_val <- ind$post_index_guid(post_guid, post_index_guid_body)
    expect_equal(ret_val$status, 200)
})

test_that("Put index GUID", {
    output <- ind$post_index(post_index)
    new_output <- content(output)
    post_guid <- new_output$did
    post_rev <- new_output$rev
    ret_val <- ind$put_index_guid(post_guid, post_rev, put_index_guid_body)
    expect_equal(ret_val$status, 200)
})

test_that("Delete index GUID", {
    output <- ind$post_index(post_index)
    new_output <- content(output)
    post_guid <- new_output$did
    post_rev <- new_output$rev
    ret_val <- ind$delete_index_guid(post_guid, post_rev)
    expect_equal(ret_val$status, 200)
})

test_that("Post bulk document GUID", {
    ret_val <- ind$post_bulk_documents(list(guid))
    expect_equal(ret_val$status, 200)
})

test_that("Get query urls: no params", {
    ret_val <- ind$get_query_urls()
    expect_equal(ret_val$status, 200)
})

test_that("Get query urls: no dogs", {
    ret_val <- ind$get_query_urls(exclude="dog")
    expect_equal(ret_val$status, 200)
})

test_that("Get query urls metadata: missing params", {
    expect_error(ind$get_query_urls_metadata())
})

test_that("Get query urls metadata: ", {
    key <- "key"
    value <- "value"
    ret_val <- ind$get_query_urls_metadata(key, value)
    expect_equal(ret_val$status, 200)
})