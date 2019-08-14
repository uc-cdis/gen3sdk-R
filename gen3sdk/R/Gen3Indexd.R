library(httr)
library(jsonlite)
library(readr)

Gen3Indexd <- setRefClass("Gen3Indexd",

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
#   ... ind <- Gen3Indexd(endpoint, auth)

    fields = list(
        endpoint= "character",
        auth_provider= "Gen3AuthHelper"
    ),

    methods = list(
        initialize = function(endpoint, auth_provider) {
            .self$endpoint <- endpoint
            .self$auth_provider <- auth_provider
        },
        get_system_status = function() {
            api_url <- paste(endpoint, "/index/_status", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_system_version = function() {
            api_url <- paste(endpoint, "/index/_status", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_system_stats = function() {
            api_url <- paste(endpoint, "/index/_status", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_global_guid = function(guid) {
            api_url <- paste(endpoint, "/index/", guid, sep="")
            output <- GET(api_url)
            return (output)
        },
        get_global_urls = function(size=NULL, hash=NULL, ids=NULL) {
            api_url <- paste(endpoint, "/index/urls", sep="")
            output <- GET(api_url, query = list(size=size, hash=hash, ids=ids))
            return (output)
        },
        post_index = function(body) {
            auth_token <- auth_provider$get_auth_value(auth_provider$get_access_token())
            body$authz <- list(body$authz)
            json_body <- toJSON(body, auto_unbox = TRUE)
            api_url <- paste(endpoint, "/index/index", sep="")
            output <- POST(api_url, add_headers(Authorization = auth_token), 
                content_type('application/json'), body = json_body, encode = 'json')
                return (output)
        },
        get_index = function(urls_meta=NULL, meta=NULL, size=NULL, hash=NULL, uploader=NULL, 
                                ids=NULL, urls=NULL, acl=NULL, authz=NULL, negate_params=NULL) {
            api_url <- paste(endpoint, "/index/index", sep="")
            output <- GET(api_url, query = list(urls_metadata=urls_meta, metadata=meta, size=size, hash=hash, 
                            uploader=uploader, ids=ids, urls=urls, acl=acl, authz=authz, negate_params=negate_params))
            return (output)
        },
        get_index_guid = function(guid) {
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- GET(api_url)
            return (output)
        },
        post_index_guid = function(guid, body) {
            auth_token <- auth_provider$get_auth_value(auth_provider$get_access_token())
            body$authz <- list(body$authz)
            json_body <- toJSON(body, auto_unbox = TRUE)
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- POST(api_url, content_type("application/json"), add_headers(Authorization = auth_token), body = json_body)
            return (output)
        },
        put_index_guid = function(guid, rev, body) {
            auth_token <- auth_provider$get_auth_value(auth_provider$get_access_token())
            body$authz <- list(body$authz)
            body$urls <- list(body$urls)
            json_body <- toJSON(body, auto_unbox = TRUE)
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- PUT(api_url, add_headers(Authorization = auth_token), content_type("application/json"), query = list(rev = rev), body = json_body)
            return (output)
        },
        delete_index_guid = function(guid, rev) {
            auth_token <- auth_provider$get_auth_value(auth_provider$get_access_token())
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- DELETE(api_url, add_headers(Authorization = auth_token), query = list(rev = rev))
            return (output)
        },
        post_bulk_documents = function(dids) {
            api_url <- paste(endpoint, "/index/bulk/documents", sep="")
            output <- POST(api_url, body = dids, encode = 'json')
            return (output)
        },
        get_index_guid_latest = function(guid, has_version) {
            api_url <- paste(endpoint, "/index/", guid, "/latest", sep="")
            if (missing(has_version)) {
                output <- GET(api_url)
                return (output)
            } else {
                output <- GET(api_url, query = list(has_version=has_version))
                return (output)
            }
        },
        get_index_guid_versions = function(guid) {
            api_url <- paste(endpoint, "/index/", guid, "/versions", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_query_urls = function(exclude=NULL, include=NULL, versioned=FALSE, limit=100, offset=0) {
            api_url <- paste(endpoint, "/_query_urls/q", sep="")
            output <- GET(api_url, query = list(exclude=exclude, include=include, versioned=versioned,
                                                    limit=limit, offset=offset))
            return (output)
        },
        get_query_urls_metadata = function(key, value, url=NULL, versioned=FALSE, limit=100, offset=0) {
            api_url <- paste(endpoint, "/_query_urls/metdata/q", sep="")
            output <- GET(api_url, query = list(key=key, value=value, url=url, versioned=versioned, 
                                                    limit=limit, offset=offset))
            return (output)
        }          
    )
)