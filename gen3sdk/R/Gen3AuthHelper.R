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

#       >>> auth = Gen3AuthHelper("https://nci-crdc-demo.datacommons.io", refresh_file="credentials.json")

    fields = list(
        endpoint= "character",
        refresh_file= "character"
    ),

    methods = list(
        get_access_token = function() {
            # Returns the Authorization header value for the request.
            
            refresh_data <- fromJSON(refresh_file)
            refresh_token <- toJSON(refresh_data, auto_unbox = TRUE)
            auth_url = paste(link, "/user/credentials/cdis/access_token", sep="")
            access_token_json <- POST(auth_url, body=refresh_token, encode = 'json')
            access_token <- paste("Bearer ", content(access_token_json), sep="")
            return (access_token)
        }
    )
)