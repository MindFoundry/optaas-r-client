USER_AGENT = paste("RClient", packageVersion("optaas.client"), sep="/")

#' OPTaaSSession
#'
#' Makes requests to OPTaaS and returns responses.
#'
#' @import httr

OPTaaSSession <- R6::R6Class(
    'OPTaaSSession',
    public = list(
        initialize = function(server_url, api_key, disable_version_check) {
            private$server_url <- server_url
            private$headers <- add_headers('X-ApiKey' = api_key, 'User-Agent' = USER_AGENT)
            private$disable_version_check <- disable_version_check
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
        disable_version_check = NULL,
        get_response = function(callable, endpoint, body = NULL, query = NULL) {
            response <- callable(
                url = modify_url(private$server_url, path = endpoint),
                query = query,
                body = body,
                encode = "json",
                private$headers
            )
            
            if (isFALSE(private$disable_version_check)) {
                check_version(httr::headers(response)$`X-ApiVersion`, packageVersion('optaas.client'))
            }
            
            if (http_error(response)) {
                stop(paste("Status:", status_code(response), " ",
                           "Message:", httr::content(response)))
            } else {
                httr::content(response)
            }
        }
    )
)


check_version = function(server_version, client_version) {
    if (!is.null(server_version)) {
        server_version <- strsplit(server_version, ".post")[[1]][1]
        if (server_version != client_version) {
            tryCatch({
                comparison <- compareVersion(server_version, client_version)
                if (comparison > 0) {
                    warning(gsub("SERVER_VERSION", server_version, NEWER_VERSION_MESSAGE))
                } else if (comparison < 0) {
                    warning(gsub("SERVER_VERSION", server_version, OLDER_VERSION_MESSAGE))
                }
            }, error=function(cond) {})
        }
    }
}


NEWER_VERSION_MESSAGE = "
A new version of the OPTaaS client is available. Please run:
    devtools::install_github('MindFoundry/optaas-r-client@SERVER_VERSION')
To stop these messages, use OPTaaSClient$new(url, api_key, disable_version_check=TRUE)
"


OLDER_VERSION_MESSAGE = "
Your OPTaaS client version is not in sync with your OPTaaS server. To avoid unexpected behavior, please run:
    devtools::install_github('MindFoundry/optaas-r-client@SERVER_VERSION')
To stop these messages, use OPTaaSClient$new(url, api_key, disable_version_check=TRUE)
"