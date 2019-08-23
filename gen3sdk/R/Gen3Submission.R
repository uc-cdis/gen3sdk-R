library(httr)
library(jsonlite)
library(readr)

Gen3Submission <- setRefClass("Gen3Submission",

# Submit/Export/Query data from a Gen3 Submission system.

# A class for interacting with the Gen3 submission services.
# Supports submitting and exporting from Sheepdog.
# Supports GraphQL queries through Peregrine.

# Args:
#   endpoint (str): The URL of the data commons.
#   auth_provider (Gen3Auth): A Gen3Auth class instance.

# Examples:
#   This generates the Gen3Submission class pointed at the sandbox commons while
#   using the credentials.json downloaded from the commons profile page.

#   >>> endpoint <- "https://nci-crdc-demo.datacommons.io"
#   ... auth <- Gen3AuthHelper(endpoint, refresh_file="credentials.json")
#   ... sub <- Gen3Submission(endpoint, auth)

    fields = list(
        endpoint = "character",
        auth_provider = "Gen3AuthHelper"
    ),

    methods = list(
        initialize = function(endpoint, auth_provider) {
            .self$endpoint <- endpoint
            .self$auth_provider <- auth_provider
        },

        export_json = function(filename, output) {
            # Writes an API response to a file.
            exportJson <- toJSON(output)
            write(exportJson, file = filename)
            print(paste("Output written to file:",filename))
        },

        export_tsv = function(filename, output) {
            # Writes an API response to a file.
            write.table(output, file = filename, quote=FALSE, sep = '\t')
            print(paste("Output written to file:", filename))
        },

        create_program = function(json_parameter) {
            # Create a program.
            # Args:
            # json (object): The json of the program to create

            # Examples:
            # This creates a program in the sandbox commons.

            # >>> sub.create_program(json_parameter)

            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", sep = "")
            json_body <- toJSON(json_parameter, auto_unbox = TRUE)
            output <- POST(api_url,
                         add_headers(Authorization = auth_token),
                         body = json_body, 
                         encode = 'json'
                        )
            return (output)
        },

        create_project = function(program, json_parameter) {
            # Create a project.
            # Args:
            #   program (str): The program to create a project on
            #   json (object): The json of the project to create

            # Examples:
            #   This creates a project on the DCF program in the sandbox commons.

            # >>> sub.create_project("DCF", json)

            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", program, sep = "")
            json_body <- toJSON(json_parameter, auto_unbox = TRUE)
            output <- PUT(api_url,
                        add_headers(Authorization = auth_token),
                        body = json_body, 
                        encode = 'json'
                       )
            return (output)
        },

        delete_project = function(program, project) {
            # Delete a project.

            # This deletes an empty project from the commons.

            # Args:
            #   program (str): The program containing the project to delete.
            #   project (str): The project to delete.

            # Examples:
            #   This deletes the "CCLE" project from the "DCF" program.

            # >>> sub.delete_project("DCF", "CCLE")

            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, sep = "")
            output <- DELETE(api_url, add_headers(Authorization = auth_token))
            return (output)
        },

        delete_program = function(program) {
            # Delete a program.

            # This deletes an empty program from the commons.

            # Args:
            #   program (str): The program to delete.

            # Examples:
            #   This deletes the "DCF" program.

            # >>> sub.delete_program("DCF")

            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", program, sep = "")
            output <- DELETE(api_url, add_headers(Authorization = auth_token))
            return (output)
        },

        get_dictionary_node = function(node_type) {
            # Returns the dictionary schema for a specific node.

            # This gets the current json dictionary schema for a specific node type in a commons.

            # Args:
            #   node_type (str): The node_type (or name of the node) to retrieve.

            # Examples:
            #   This returns the dictionary schema the "subject" node.

            # >>> sub.get_dictionary_node("subject")

            api_url <- paste(endpoint, "/api/v0/submission/_dictionary/", node_type, sep = "")
            output <- GET(api_url)
            return (output)
        },

        get_dictionary_all = function() {
            # Returns the entire dictionary object for a commons.

            # This gets a json of the current dictionary schema for a commons.

            # Examples:
            #   This returns the dictionary schema for a commons.

            #   >>> sub.get_dictionary_all()

            output <- get_dictionary_node("_all")
            return(output)
        },

        get_graphql_schema = function() {
            # Returns the GraphQL schema for a commons.

            # This runs the GraphQL introspection query against a commons and returns the results.

            # Examples:
            #   This returns the GraphQL schema.

            # >>> sub.get_graphql_schema()

            api_url <- paste(endpoint, "/api/v0/submission/getschema", sep = "")
            output <- GET(api_url)
            return (output)
        },

        submit_record = function(program, project, json_parameter) {
            # Submit record(s) to a project as json.

            # Args:
            #   program (str): The program to submit to.
            #   project (str): The project to submit to.
            #   json (object): The json defining the record(s) to submit. For multiple records, the json should be an array of records.

            # Examples:
            #   This submits records to the CCLE project in the sandbox commons.

            # >>> sub.submit_record("DCF", "CCLE", json)

            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, sep = "")
            json_body <- toJSON(json_parameter, auto_unbox = TRUE)
            output <- PUT(api_url,
                         add_headers(Authorization = auth_token),
                         body = json_body, 
                         encode = 'json'
                        )
            return (output)
        },

        delete_record = function(program, project, uuid) {
            # Delete a record from a project.
            # Args:
            #   program (str): The program to delete from.
            #   project (str): The project to delete from.
            #   uuid (str): The uuid of the record to delete

            # Examples:
            #   This deletes a record from the CCLE project in the sandbox commons.

            # >>> sub.delete_record("DCF", "CCLE", uuid)

            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/entities/", uuid, sep = "")
            output <- DELETE(api_url, add_headers(Authorization = auth_token))
            return (output)
        },

        export_record_helper = function(program, project, uuid, fileformat) {
            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/export", sep = "")
            output <- GET(api_url, add_headers(Authorization = auth_token), query = list(ids = uuid, format = fileformat))
            return (output)
        },

        export_record = function(program, project, uuid, fileformat, filename="") {
            # Export a single record into json.

            # Args:
            #   program (str): The program the record is under.
            #   project (str): The project the record is under.
            #   uuid (str): The UUID of the record to export.
            #   fileformat (str): Export data as either 'json' or 'tsv'
            #   filename (str): Name of the file to export to; if no filename is provided, prints data to screen

            # Examples:
            #   This exports a single record from the sandbox commons.

            # >>> sub.export_record("DCF", "CCLE", "d70b41b9-6f90-4714-8420-e043ab8b77b9", "json", filename="DCF-CCLE_one_record.json")

            if(!(fileformat %in% list('json','tsv'))) {
                stop("Error: File format must be either 'json' or 'tsv'")
            }
            output <- export_record_helper(program, project, uuid, fileformat)
            if(fileformat == 'json') {
                json_content <- content(output, "parsed", "application/json")
                if(filename! = "") {
                    export_json(filename, json_content)
                }
                return (json_content)
            } else {
                text_content <- content(output, as = "text")
                tsv_content <- read_tsv(text_content)
                if(filename! = "") {
                    export_tsv(filename, tsv_content)
                }
                return (tsv_content)
            }
        },

        export_node_helper = function(program, project, node_type, fileformat) {
            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/", program, "/", project, "/export", sep = "")
            output <- GET(api_url, add_headers(Authorization = auth_token), query = list(node_label = node_type, format = fileformat))
            return (output)
        },

        export_node = function(program, project, node_type, fileformat, filename = "") {
            # Export all records in a single node type of a project.

            # Args:
            #   program (str): The program to which records belong.
            #   project (str): The project to which records belong.
            #   node_type (str): The name of the node to export.
            #   fileformat (str): Export data as either 'json' or 'tsv'
            #   filename (str): Name of the file to export to; if no filename is provided, prints data to screen

            # Examples:
            #   This exports all records in the "sample" node from the CCLE project in the sandbox commons.

            # >>> sub.export_node("DCF", "CCLE", "sample", "tsv", filename="DCF-CCLE_sample_node.tsv")

            if(!(fileformat %in% list('json','tsv'))) {
                stop("Error: File format must be either 'json' or 'tsv'")
            }
            output <- export_record_helper(program, project, node_type, fileformat)
            if(fileformat == 'json') {
                json_content <- content(output, "parsed", "application/json")
                if(filename! = "") {
                    export_json(filename, json_content)
                }
                return (json_content)
            } else {
                text_content <- content(output, as = "text")
                tsv_content <- read_tsv(text_content)
                if(filename! = "") {
                    export_tsv(filename, tsv_content)
                }
                return (tsv_content)
            }
        },

        query = function(query_txt, variables = "") {
            # Execute a GraphQL query against a data commons.

            # Args:
            #   query_txt (str): Query text.
            #   variables (:obj:`object`, optional): Dictionary of variables to pass with the query.
            #   max_tries (:obj:`int`, optional): Number of times to retry if the request fails.

            # Examples:
            #   This executes a query to get the list of all the project codes for all the projects
            #   in the data commons.

            # >>> query <- "{ project(first:0) { code } }"
            # ... sub.query(query)
            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/api/v0/submission/graphql", sep = "")
            if(variables == "") {
                query <- paste('{"query": "', query_txt, '"}', sep = '')
            } else {
                query <- paste('{"query": "', query_txt, '"variables": "', variables, '"}', sep = "")
            }
            query_body <- fromJSON(query)
            json_body <- toJSON(query_body, auto_unbox = TRUE)
            output <- POST(api_url,
                           add_headers(Authorization = auth_token),
                           body = json_body, 
                           encode = 'json'
                          )
            json_content <- content(output)
            return (output)
        }
    )
)