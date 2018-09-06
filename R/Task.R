#' Task
#'
#' Allows you to access Task attributes and perform all Task-related functions.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' task$id  # The unique ID of the Task
#' task$json  # The full JSON representation of the Task in OPTaaS
#' 
#' task$delete()  # Delete the task
#' }

Task <- R6::R6Class(
    'Task',
    public = list(
        json = NULL,
        id = NULL,
        initialize = function(json, session) {
            self$json <- json
            self$id <- json$id
            private$session <- session
            private$self_url <- json$'_links'$self$href
        },
        delete = function() {
            private$session$delete(private$self_url)
        }
    ),
    private = list(
        session = NULL,
        self_url = NULL
    )
)
