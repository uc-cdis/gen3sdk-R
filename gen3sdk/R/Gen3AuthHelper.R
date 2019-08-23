library(httr)
library(jsonlite)

Gen3AuthHelper <- setRefClass("Gen3AuthHelper",

# Gen3 auth helper class for use with requests auth.

# Implements requests.auth.AuthBase in order to support JWT authentication.
# Generates access tokens from the provided refresh token file or string.
# Automatically refreshes access tokens when they expire.

# Args:
#   endpoint (str): The URL of the data commons.
#   refresh_file (str): The file containing the downloaded json web token.

#   Examples:
#       This generates the Gen3Auth class pointed at the sandbox commons while
#       using the credentials.json downloaded from the commons profile page.

#       >>> auth <- Gen3AuthHelper("https://nci-crdc-demo.datacommons.io", refresh_file="credentials.json")

    fields = list(
        endpoint = "character",
        refresh_file = "character"
    ),

    methods = list(
        initialize = function(endpoint = "", refresh_file = "") {
            .self$endpoint <- endpoint
            .self$refresh_file <- refresh_file
        },
        
        get_access_token = function() {
            if (.self$endpoint == "") {
                stop("Missing endpoint")
            }
            if (.self$refresh_file == "") {
                stop("Missing refresh file")
            }
            tryCatch( { refresh_data <- fromJSON(refresh_file) },
                        error = function(e) {stop(sprintf("Could not load your refresh token file: %s", refresh_file))}
                    )
            refresh_data <- fromJSON(refresh_file)
            refresh_token <- toJSON(refresh_data, auto_unbox = TRUE)
            auth_url <- paste(endpoint, "/user/credentials/cdis/access_token", sep = "")
            access_token_json <- POST(auth_url, body = refresh_token, encode = 'json')
            if (http_error(access_token_json)) {
                stop(sprintf("Failed to authenticate %s", auth_url))
            }
            return (access_token_json)
        },

        get_auth_value = function() {
            token <- get_access_token()
            auth_value <- paste("Bearer ", content(token), sep = "")
            return (auth_value)
        }
    )
)