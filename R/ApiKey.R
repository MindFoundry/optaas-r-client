#' ApiKey
#'
#' An API Key that can be used to interact with OPTaaS.
#' 
#' @field id The actual API Key
#' @field json The full JSON representation of the API Key in OPTaaS
#' 
#' @export

ApiKey <- R6::R6Class(
    'ApiKey',
    public = list(
        id = NULL,
        json = NULL,
        initialize = function(json, session) {
            self$id <- json$id
            self$json <- json
            private$session <- session
            private$url <- self$json$`_links`$self$href
        },
        set_role = function(role) {
            private$session$put(private$url, list(role=role))
            self$json$role <- role
        },
        set_expired = function(expired) {
            private$session$put(private$url, list(expired=expired))
            self$json$expired <- expired
        }
    ),
    private = list(
        session = NULL,
        url = NULL
    )
)
