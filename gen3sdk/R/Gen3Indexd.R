library(httr)
library(jsonlite)
library(readr)

Gen3Indexd <- setRefClass("Gen3Indexd",

# A class for interacting with the Gen3 Indexd services.

# Args:
#   endpoint (str): The URL of the data commons.
#   auth_provider (Gen3Auth): A Gen3Auth class instance.

# Examples:
#   This generates the Gen3DIndexd class pointed at the sandbox commons while
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
            # Returns if IndexD is healthy or not

            # Examples:
            # >>> ind.get_system_status()

            api_url <- paste(endpoint, "/index/_status", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_system_version = function() {
            # Returns the version of IndexD

            # Examples:
            # >>> ind.get_system_version()
            api_url <- paste(endpoint, "/index/_version", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_system_stats = function() {
            # Returns basic information about the records in IndexD

            # Examples:
            # >>> ind.get_system_stats()
            api_url <- paste(endpoint, "/index/_stats", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_global_guid = function(guid) {
            # Get the metadata associated with the given id, alias, or distributed identifer
            # Args:
            #   guid (str): The guid of the record to retrieve

            # Examples:
            # This retrieves the metadata for guid

            # >>> ind.get_global_guid(guid)
            api_url <- paste(endpoint, "/index/", guid, sep="")
            output <- GET(api_url)
            return (output)
        },
        get_global_urls = function(size=NULL, hash=NULL, ids=NULL) {
            # Get a list of urls that match query params
            # Args:
            #   size (int): The object size of the record to retrieve
            #   hash (str): The hashes specified as algorithm:value of the record to retrieve
            #   ids (str): The ids, comma delimited, of the record to retrieve

            # Examples:
            # This retrieves the urls with the filters size, ids

            # >>> ind.get_global_urls(size, ids)
            api_url <- paste(endpoint, "/index/urls", sep="")
            output <- GET(api_url, query = list(size=size, hash=hash, ids=ids))
            return (output)
        },
        post_index = function(body) {
            # Add a new entry to the index
            # Args:
            #   body (object): The json-R-object of the record to create

            # Examples:
            # This adds a new entry in the sandbox index

            # >>> ind.post_index(body)
            auth_token <- auth_provider$get_auth_value()
            body$authz <- list(body$authz)
            json_body <- toJSON(body, auto_unbox = TRUE)
            api_url <- paste(endpoint, "/index/index", sep="")
            output <- POST(api_url, add_headers(Authorization = auth_token), 
                content_type('application/json'), body = json_body, encode = 'json')
                return (output)
        },
        get_index = function(urls_meta=NULL, meta=NULL, size=NULL, hash=NULL, uploader=NULL, 
                                ids=NULL, urls=NULL, acl=NULL, authz=NULL, negate_params=NULL) {
            # Get a list of all records
            # Args:
            #   urls_metatdata (str): The urls_metadata, JSON string format, of the record to retrieve
            #   metatdata (str): The metatdata, in format key:value, of the record to retrieve
            #   size (int): The object size of the record to retrieve
            #   hash (str): The hashes specified as algorithm:value of the record to retrieve
            #   uploader (str): The uploader id of the reecord to retrieve
            #   ids (str): The ids, comma delimited, of the record to retrieve
            #   urls (str): The urls, comma delimited, of the record to reetrieve
            #   acl (str): The acl, comma delimited, of the record to retrieve
            #   authz (str): The authz, comma delimited, of the record to retrieve
            #   negate_params (str): The negate params, JSON string format, of the record to retrieve
            #   start (str): The start did of the record to retrieve
            #   limit (str): The number of records to return for this page, default to 100

            # Examples:
            # This retrieves the records with the size filter

            # >>> ind.get_index(size)
            api_url <- paste(endpoint, "/index/index", sep="")
            output <- GET(api_url, query = list(urls_metadata=urls_meta, metadata=meta, size=size, hash=hash, 
                            uploader=uploader, ids=ids, urls=urls, acl=acl, authz=authz, negate_params=negate_params))
            return (output)
        },
        get_index_guid = function(guid) {
            # Get the metadata associated with the given id
            # Args:
            #   guid (str): The guid of the record to retrieve

            # Examples:
            # This retrieves the metadata for guid

            # >>> ind.get_index_guid(guid)
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- GET(api_url)
            return (output)
        },
        post_index_guid = function(guid, body) {
            # Add a new version for the document associated to the provided uuid
            # Args:
            #   guid (str): The uuid associated to the record needed to have new verison
            #   body (object): The json-R-object of the record to create

            # Examples:
            # This adds a new verion of the document anchored by baseid

            # >>> ind.post_index_guid(guid, body)
            auth_token <- auth_provider$get_auth_value()
            body$authz <- list(body$authz)
            json_body <- toJSON(body, auto_unbox = TRUE)
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- POST(api_url, content_type("application/json"), add_headers(Authorization = auth_token), body = json_body)
            return (output)
        },
        put_index_guid = function(guid, rev, body) {
            # Update an existing entry in the index
            # Args:
            #   guid (str): The uuid associated to the record needed to update
            #   rev (str): The data revision associated with the record to update
            #   body (object): The json-R-object of the index record that needs to be updated

            # Examples:
            # This updates the record

            # >>> ind.put_index_guid(guid, rev, body)
            auth_token <- auth_provider$get_auth_value()
            body$authz <- list(body$authz)
            body$urls <- list(body$urls)
            json_body <- toJSON(body, auto_unbox = TRUE)
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- PUT(api_url, add_headers(Authorization = auth_token), content_type("application/json"), query = list(rev = rev), body = json_body)
            return (output)
        },
        delete_index_guid = function(guid, rev) {
            # Deletes an entry from the index
            # Args:
            #   guid (str): The uuid associated to the record needed to delete
            #   rev (str): The data revision associated with the record to delete

            # Examples:
            # This deletes the record

            # >>> ind.delete_index_guid(guid, rev)
            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/index/index/", guid, sep="")
            output <- DELETE(api_url, add_headers(Authorization = auth_token), query = list(rev = rev))
            return (output)
        },
        post_bulk_documents = function(dids) {
            # Get a list of documents given a list of dids
            # Args:
            #   dids (str): List of dids to retrive

            # Examples:
            # Retrieves documents associated with dids

            # >>> ind.post_bulk_documents(guid, rev)
            api_url <- paste(endpoint, "/index/bulk/documents", sep="")
            output <- POST(api_url, body = dids, encode = 'json')
            return (output)
        },
        get_index_guid_latest = function(guid, has_version) {
            # Get the metadata of the latest index record version associated with the given id
            # Args:
            #   guid (str): The guid of the record to retrieve
            #   has_version (bool): Filter by the latest doc that has version value populated

            # Examples:
            # Retrieves latest metadata associated with guid

            # >>> ind.get_index_guid_latest(guid, has_version)
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
            # Get the metadata of index record versions associated with the given id
            # Args:
            #   guid (str): The guid of the record to retrieve

            # Examples:
            # Retrieves metadata associated with guid

            # >>> ind.get_index_guid_versions(guid)
            api_url <- paste(endpoint, "/index/", guid, "/versions", sep="")
            output <- GET(api_url)
            return (output)
        },
        get_query_urls = function(exclude=NULL, include=NULL, versioned=FALSE, limit=100, offset=0) {
            # Search index records by urls

            # Args:
            #   exclude (str): search for documents without a single URL that match this pattern
            #   include (str): search for documents with at least one URL that match this pattern
            #   versioned (bool): if true search with a version set, else search documents without version
            #   limit (int): maximum rows to return
            #   offset (int): pointer position to start search

            # Examples:
            # Retrieves index records by url with include filter

            # >>> ind.get_query_urls(include)
            api_url <- paste(endpoint, "/_query_urls/q", sep="")
            output <- GET(api_url, query = list(exclude=exclude, include=include, versioned=versioned,
                                                    limit=limit, offset=offset))
            return (output)
        },
        get_query_urls_metadata = function(key, value, url=NULL, versioned=FALSE, limit=100, offset=0) {
            # Search index records by urls metadata key and value

            # Args:
            #   key (str): metadata key to search by
            #   value (str): metadata value for provided key
            #   url (str): URL pattern to filter by
            #   versioned (bool): if true search with a version set, else search documents without version
            #   limit (int): maximum rows to return
            #   offset (int): pointer position to start search

            # Examples:
            # Retrieves index records by urls metadata

            # >>> ind.get_query_urls_metadata(key, value)
            api_url <- paste(endpoint, "/_query_urls/metdata/q", sep="")
            output <- GET(api_url, query = list(key=key, value=value, url=url, versioned=versioned, 
                                                    limit=limit, offset=offset))
            return (output)
        }          
    )
)