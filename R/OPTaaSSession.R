USER_AGENT = paste("RClient", packageVersion("optaas.client"), sep="/")

#' OPTaaSSession
#'
#' Makes requests to OPTaaS and returns responses.
#'
#' @import httr

OPTaaSSession <- R6::R6Class(
    'OPTaaSSession',
    public = list(
        initialize = function(server_url, api_key) {
            private$server_url <- server_url
            private$headers <- add_headers('X-ApiKey' = api_key, 'User-Agent' = USER_AGENT)
        },
        post = function(endpoint, body) {
            private$get_response(POST, endpoint, body)
        },
        get = function(endpoint) {
            private$get_response(GET, endpoint)
        },
        put = function(endpoint) {
            private$get_response(PUT, endpoint)
        },
        delete = function(endpoint) {
            private$get_response(DELETE, endpoint)
        }
    ),
    private = list(
        server_url = NULL,
        headers = NULL,
        get_response = function(callable, endpoint, body = NULL) {
            response <- callable(
                url = modify_url(private$server_url, path = endpoint),
                body = body,
                encode = "json",
                private$headers
            )
            if (http_error(response)) {
                stop(paste(status_code(response), httr::content(response)))
            } else {
                httr::content(response)
            }
        }
    )
)
