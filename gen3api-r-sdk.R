library(reticulate)
library(httr)
library(jsonlite)
library(readr)

use_python("/usr/local/bin/python3")

source_python("auth_func.py")
auth_tok <- get_auth_value()

link = "https://nci-crdc-demo.datacommons.io"

Submission <- setRefClass("Submission",
    fields = list(
        endpoint= "character",
        auth_provider= "character"
    ),
    methods = list(
        export_json = function(filename, output) {
            exportJson <- toJSON(output)
            write(exportJson, file=filename)
            print(paste("Output written to file:",filename))
        },
        export_tsv = function(filename, output) {
            write.table(output, file=filename, quote=FALSE, sep='\t')
            print(paste("Output written to file:",filename))
        },
        create_program = function(json_parameter) {
            api_url <- paste(endpoint, "/api/v0/submission/", sep="")
            json_body <- toJSON(json_parameter, auto_unbox = TRUE)
            output <- POST(api_url,
                         add_headers(Authorization = auth_provider),
                         body = json_body, 
                         encode = 'json'
                        )
            return (output)
        },
        create_project = function(program, json_parameter) {
            api_url <- paste(endpoint, "/api/v0/submission/", program, sep="")
            json_body <- toJSON(json_parameter, auto_unbox = TRUE)
            output <- PUT(api_url,
                        add_headers(Authorization = auth_provider),
                        body = json_body, 
                        encode = 'json'
                       )
            return (output)
        },
        delete_project = function(program, project) {
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, sep="")
            output <- DELETE(api_url, add_headers(Authorization = auth_provider))
            return (output)
        },
        delete_program = function(program) {
            api_url <- paste(endpoint, "/api/v0/submission/", program, sep="")
            output <- DELETE(api_url, add_headers(Authorization = auth_provider))
            return (output)
        },
        get_dictionary_node = function(node_type) {
            api_url <- paste(endpoint, "/api/v0/submission/_dictionary/", node_type, sep="")
            output <- GET(api_url)
            return (output)
        },
        get_dictionary_all = function() {
            output <- get_dictionary_node("_all")
            return(output)
        },
        get_graphql_schema = function() {
            api_url <- paste(endpoint, "/api/v0/submission/getschema", sep="")
            output <- GET(api_url)
            return (output)
        },
        submit_record = function(program, project, json_parameter) {
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, sep="")
            json_body <- toJSON(json_parameter, auto_unbox = TRUE)
            output <- PUT(api_url,
                         add_headers(Authorization = auth_provider),
                         body = json_body, 
                         encode = 'json'
                        )
            return (output)
        },
        delete_record = function(program, project, uuid) {
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/entities/", uuid, sep="")
            output <- DELETE(api_url, add_headers(Authorization = auth_provider))
            return (output)
        },
        export_record = function(program, project, uuid, fileformat, filename="") {
            if(!(fileformat %in% list('json','tsv'))) {
                stop("Error: File format must be either 'json' or 'tsv'")
            }
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/export?ids=", uuid, "&format=", fileformat, sep="")
            output <- GET(api_url, add_headers(Authorization = auth_provider))
            if(fileformat == 'json') {
                json_content <- content(output, "parsed", "application/json")
                if(!(filename=="")) {
                    export_json(filename, json_content)
                }
                return (json_content)
            } else {
                text_content <- content(output, as = "text")
                tsv_content <- read_tsv(text_content)
                if(!(filename=="")) {
                    export_tsv(filename, tsv_content)
                }
                return (tsv_content)
            }
        },
        export_node = function(program, project, node_type, fileformat, filename="") {
            if(!(fileformat %in% list('json','tsv'))) {
                stop("Error: File format must be either 'json' or 'tsv'")
            }
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/export/?node_label=", node_type, "&format=", fileformat, sep="")
            output <- GET(api_url, add_headers(Authorization = auth_provider))
            if(fileformat == 'json') {
                json_content <- content(output, "parsed", "application/json")
                if(!(filename=="")) {
                    export_json(filename, json_content)
                }
                return (json_content)
            } else {
                text_content <- content(output, as = "text")
                tsv_content <- read_tsv(text_content)
                if(!(filename=="")) {
                    export_tsv(filename, tsv_content)
                }
                return (tsv_content)
            }
        },
        query = function(query_txt, variables="") {
            api_url <- paste(endpoint, "/api/v0/submission/graphql", sep="")
            if(variables=="") {
                query <- paste('{"query": "', query_txt, '"}', sep='')
            } else {
                query <- paste('{"query": "', query_txt, '"variables": "', variables, '"}', sep="")
            }
            query_body <- fromJSON(query)
            json_body <- toJSON(query_body, auto_unbox = TRUE)
            output <- POST(api_url,
                           add_headers(Authorization = auth_provider),
                           body = json_body, 
                           encode = 'json'
                          )
            json_content <- content(output)
            return (output)
        }
    )
)

sub <- Submission(endpoint=link, auth_provider=auth_tok)

#json_data <- fromJSON("create_program.json")
#response <- sub$create_program(json_data)
#print(response)
#str(content(response))

#json_data <- fromJSON("create_project.json")
#response <- sub$create_project("prog1",json_data)
#print(response)
#str(content(response))

#json_data <- fromJSON("submit_record.json")
#response <- sub$submit_record("prog1", "proj1", json_data)
#print(response)
#str(content(response))

#response <- sub$export_record("prog1", "proj1", <uuid from submit_record>, "json")
#print(response)

#response <- sub$export_record("prog1", "proj1", "1ed31d69-79dd-4c86-9a1b-309c5ed26c5f", "json", "record.json")
#print(response)

#response <- sub$export_record("prog1", "proj1", <uuid from submit_record>, "tsv")
#print(response)

#response <- sub$export_record("prog1", "proj1", "1ed31d69-79dd-4c86-9a1b-309c5ed26c5f", "tsv", "record.txt")
#print(response)

#response <- sub$delete_record("prog1", "proj1", "af2b99d4-a320-4fb3-bc5f-63984cdb26ff")
#print(response)
#str(content(response))

#response <- sub$delete_project("prog1", "proj1")
#print(response)
#str(content(response))

#response <- sub$delete_program("prog1")
#print(response)
#str(content(response))

#response <- sub$export_node("prog1", "proj1", "project", "json")
#print(response)

#response <- sub$export_node("prog1", "proj1", "project", "json", "node.json")
#print(response)

#response <- sub$export_node("prog1", "proj1", "project", "tsv")
#print(response)

#response <- sub$export_node("prog1", "proj1", "project", "tsv", "node.txt")
#print(response)

#response <- sub$get_dictionary_node("program")
#print(response)
#str(content(response))

#response <- sub$get_dictionary_all()
#print(response)
#str(content(response))

#response <- sub$get_graphql_schema()
#print(response)
#str(content(response))

#query <- "{ project(first:0) { code } }"
#response <- sub$query(query)
#print(response)
#str(content(response))


