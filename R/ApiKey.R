#' ApiKey
#'
#' An API Key that can be used to interact with OPTaaS.
#' 
#' @field id The actual API Key.
#' @field role The role assigned to this key.
#' @field expired Whether this key has expired, i.e. can no longer be used.
#' @field json The full JSON representation of this key in OPTaaS.
#' 
#' @export

ApiKey <- R6::R6Class(
    'ApiKey',
    public = list(
        id = NULL,
        role = NULL,
        expired = NULL,
        json = NULL,
        initialize = function(json, session) {
            self$id <- json$id
            self$role <- json$role
            self$expired <- json$expired
            self$json <- json
            private$session <- session
            private$url <- self$json$`_links`$self$href
        },
        set_role = function(role) {
            private$session$put(private$url, list(role=role))
            self$role <- role
            self$json$role <- role
        },
        set_expired = function(expired) {
            private$session$put(private$url, list(expired=expired))
            self$expired <- expired
            self$json$expired <- expired
        }
    ),
    private = list(
        session = NULL,
        url = NULL
    )
)
