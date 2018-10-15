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
        post = function(endpoint, body, query = NULL) {
            private$get_response(POST, endpoint, body, query=query)
        },
        get = function(endpoint, query = NULL) {
            private$get_response(GET, endpoint, query=query)
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
        get_response = function(callable, endpoint, body = NULL, query = NULL) {
            response <- callable(
                url = modify_url(private$server_url, path = endpoint),
                query = query,
                body = body,
                encode = "json",
                private$headers
            )
            if (http_error(response)) {
                stop(paste("Status:", status_code(response), " ",
                           "Message:", httr::content(response)))
            } else {
                httr::content(response)
            }
        }
    )
)
