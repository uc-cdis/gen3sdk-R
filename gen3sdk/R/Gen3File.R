library(httr)
library(jsonlite)

Gen3File <- setRefClass("Gen3File",
# For interacting with Gen3 file management features.

# A class for interacting with the Gen3 file download services.
# Supports getting presigned urls right now.

# Args:
#     endpoint (str): The URL of the data commons.
#     auth_provider (Gen3Auth): A Gen3Auth class instance.

# Examples:
#     This generates the Gen3File class pointed at the sandbox commons while
#     using the credentials.json downloaded from the commons profile page.

#     >>> endpoint <- "https://nci-crdc-demo.datacommons.io"
#     ... auth <- Gen3AuthHelper(endpoint, refresh_file="credentials.json")
#     ... file <- Gen3File(endpoint, auth)

    fields = list(
        endpoint= "character",
        auth_provider= "Gen3AuthHelper"
    ),

    methods = list(
        initialize = function(endpoint, auth_provider) {
            .self$endpoint <- endpoint
            .self$auth_provider <- auth_provider
        },

        get_presigned_url = function(guid, protocol="http") {
            # Generates a presigned URL for a file.

            # Retrieves a presigned url for a file giving access to a file for a limited time.

            # Args:
            #     guid (str): The GUID for the object to retrieve.
            #     protocol (:obj:`str`, optional): The protocol to use for picking the available URL for generating the presigned URL.

            # Examples:

            #     >>> file.get_presigned_url(guid)

            auth_token <- auth_provider$get_auth_value()
            api_url <- paste(endpoint, "/user/data/download/", guid, sep="")
            output <- GET(api_url, add_headers(Authorization = auth_token), query = list(protocol=protocol))
            return (output)
        }
    )
)