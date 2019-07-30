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
sub <- Gen3Submission(endpoint=endpoint, auth_provider=auth)

test_that("Parameter type check", {
    expect_error(Gen3Submission(endpoint=5))
    expect_error(Gen3Submission(auth_provider=3.14))
})

test_that("Missing Initialization Parameter", {
    expect_error(Gen3Submission())
    expect_error(Gen3Submission(endpoint=endpoint))
    expect_error(Gen3Submission(auth_provider=auth))
})

test_that("get_dictionary_node: no parameter", {
    ret_val <- sub$get_dictionary_node("")
    expect_equal(ret_val$status, 200)
})

test_that("get_dictionary_node: invalid parameter", {
    ret_val <- sub$get_dictionary_node("querty")
    expect_equal(ret_val$status, 404)
})

test_that("get_dictionary_node: vaild parameter", {
    ret_val <- sub$get_dictionary_node("program")
    expect_equal(ret_val$status, 200)
})

test_that("get_dictionary_all", {
    ret_val <- sub$get_dictionary_all()
    expect_equal(ret_val$status, 200)
})

test_that("get_dictionary_all", {
    ret_val <- sub$get_graphql_schema()
    expect_equal(ret_val$status, 200)
})

test_that("query: invalid authentication", {
    query <- '{ project(first:0) { code } }'
    ret_val <- sub$query(query)
    expect_equal(ret_val$status, 200)
})

test_that("query: invalid parameter", {
    query <- "querty"
    ret_val <- sub$query(query)
    expect_equal(ret_val$status, 400)
})

test_that("query: empty parameter", {
    query <- ""
    ret_val <- sub$query(query)
    expect_equal(ret_val$status, 400)
})

test_that("query: vaild parameter", {
    query <- "{ project(first:0) { code } }"
    ret_val <- sub$query(query)
    expect_equal(ret_val$status, 200)
})

test_that("create_program: valid parameter", {
    json_data <- fromJSON('{"dbgap_accession_number": "prog42", "name": "prog42", "type": "program"}')
    ret_val <- sub$create_program(json_data)
    expect_equal(ret_val$status, 200)
})

test_that("create_project: valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog43", "name": "prog43", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj43", "dbgap_accession_number": "proj43", "investigator_name": "jones", "name": "proj43", "type": "project"}')
    ret_val <- sub$create_project("prog43", json_proj)
    expect_equal(ret_val$status, 200)
})

test_that("delete_project: valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog44", "name": "prog44", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj44", "dbgap_accession_number": "proj44", "investigator_name": "jones", "name": "proj44", "type": "project"}')
    ret_val <- sub$create_project("prog44", json_proj)
    expect_equal(ret_val$status, 200)
    ret_val <- sub$delete_project("prog44", "proj44")
    expect_equal(ret_val$status, 204)
})

test_that("delete_program: valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog45", "name": "prog45", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj45", "dbgap_accession_number": "proj45", "investigator_name": "jones", "name": "proj45", "type": "project"}')
    ret_val <- sub$create_project("prog45", json_proj)
    expect_equal(ret_val$status, 200)
    ret_val <- sub$delete_project("prog45", "proj45")
    expect_equal(ret_val$status, 204)
    ret_val <- sub$delete_program("prog45")
    expect_equal(ret_val$status, 204)
})

### Test only passes with 'prog1', 'proj1' ###
test_that("submit_record: valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog1", "name": "prog1", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj1", "dbgap_accession_number": "proj1", "investigator_name": "jones", "name": "proj1", "type": "project", "availability_type": "Open"}')
    ret_val <- sub$create_project("prog1", json_proj)
    expect_equal(ret_val$status, 200)
    json_data <- fromJSON('
                            {
                                "projects":[{
                                    "code": "proj1",
                                    "dbgap_accession_number": "proj1",
                                    "name": "proj1"
                                }],
                                "submitter_id":"60637",
                                "type":"experiment"
                            }
                          '
                         )
    ret_val <- sub$submit_record("prog1", "proj1", json_data)
    expect_equal(ret_val$status, 200)
})

test_that("delete_record: valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog1", "name": "prog1", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj1", "dbgap_accession_number": "proj1", "investigator_name": "jones", "name": "proj1", "type": "project", "availability_type": "Open"}')
    ret_val <- sub$create_project("prog1", json_proj)
    expect_equal(ret_val$status, 200)
    json_data <- fromJSON('
                            {
                                "projects":[{
                                    "code": "proj1",
                                    "dbgap_accession_number": "proj1",
                                    "name": "proj1"
                                }],
                                "submitter_id":"6037",
                                "type":"experiment"
                            }
                          '
                         )
    ret_val <- sub$submit_record("prog1", "proj1", json_data)
    expect_equal(ret_val$status, 200)
    print(ret_val)
    record_table <- content(ret_val)
    print(record_table$entities[[1]]$id)
    ret_val <- sub$delete_record("prog1", "proj1", record_table$entities[[1]]$id)
    expect_equal(ret_val$status, 200)
})

test_that("export_record (json): valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog1", "name": "prog1", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj1", "dbgap_accession_number": "proj1", "investigator_name": "jones", "name": "proj1", "type": "project", "availability_type": "Open"}')
    ret_val <- sub$create_project("prog1", json_proj)
    expect_equal(ret_val$status, 200)
    json_data <- fromJSON('
                            {
                                "projects":[{
                                    "code": "proj1",
                                    "dbgap_accession_number": "proj1",
                                    "name": "proj1"
                                }],
                                "submitter_id":"60637",
                                "type":"experiment"
                            }
                          '
                         )
    ret_val <- sub$submit_record("prog1", "proj1", json_data)
    expect_equal(ret_val$status, 200)
    record_table <- content(ret_val)
    ret_val <- sub$export_record_helper("prog1", "proj1", record_table$entities[[1]]$id, "json")
    expect_equal(ret_val$status, 200)
})

test_that("export_record (tsv): valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog1", "name": "prog1", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj1", "dbgap_accession_number": "proj1", "investigator_name": "jones", "name": "proj1", "type": "project", "availability_type": "Open"}')
    ret_val <- sub$create_project("prog1", json_proj)
    expect_equal(ret_val$status, 200)
    json_data <- fromJSON('
                            {
                                "projects":[{
                                    "code": "proj1",
                                    "dbgap_accession_number": "proj1",
                                    "name": "proj1"
                                }],
                                "submitter_id":"60637",
                                "type":"experiment"
                            }
                          '
                         )
    ret_val <- sub$submit_record("prog1", "proj1", json_data)
    expect_equal(ret_val$status, 200)
    record_table <- content(ret_val)
    ret_val <- sub$export_record_helper("prog1", "proj1", record_table$entities[[1]]$id, "tsv")
    expect_equal(ret_val$status, 200)
})

test_that("export_node (json): valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog1", "name": "prog1", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj1", "dbgap_accession_number": "proj1", "investigator_name": "jones", "name": "proj1", "type": "project", "availability_type": "Open"}')
    ret_val <- sub$create_project("prog1", json_proj)
    expect_equal(ret_val$status, 200)
    ret_val <- sub$export_node_helper("prog1", "proj1", "program", "json")
    expect_equal(ret_val$status, 200)
})

test_that("export_node (json): valid parameter", {
    json_prog <- fromJSON('{"dbgap_accession_number": "prog1", "name": "prog1", "type": "program"}')
    ret_val <- sub$create_program(json_prog)
    expect_equal(ret_val$status, 200)
    json_proj <- fromJSON('{"code": "proj1", "dbgap_accession_number": "proj1", "investigator_name": "jones", "name": "proj1", "type": "project", "availability_type": "Open"}')
    ret_val <- sub$create_project("prog1", json_proj)
    expect_equal(ret_val$status, 200)
    ret_val <- sub$export_node_helper("prog1", "proj1", "program", "tsv")
    expect_equal(ret_val$status, 200)
})