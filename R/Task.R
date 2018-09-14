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
#' # To run your task:
#' # First define a scoring function whose arguments are identical to your parameter names.
#' # The function should return a score value. Then to run the task for n iterations and store the best result:
#' best_result <- task$run(scoring_function, n)
#' 
#' # Or if you prefer to do things manually:
#' task$generate_configuration()  # Generate a configuration
#' task$record_result(configuration, score)  # Record your score and return the next configuration to try
#' task$get_best_result()  # Return the result with the best score
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
            private$configurations_url <- json$'_links'$configurations$href
            private$results_url <- json$'_links'$results$href
        },
        run = function(scoring_function, number_of_iterations) {
            configuration <- self$generate_configuration()
            for (i in 1:number_of_iterations) {
                score <- do.call(scoring_function, configuration$values)
                print(paste("Iteration:", i, " ", "Score:", score))
                configuration <- self$record_result(configuration, score)
            }
            self$get_best_result()
        },
        generate_configuration = function() {
            response <- private$session$post(private$configurations_url, NULL)
            response$configurations[[1]]
        },
        record_result = function(configuration, score) {
            result_url <- configuration$'_links'$results$href
            result_body <- list(score=score)
            response <- private$session$post(result_url, result_body)
            response$nextConfiguration
        },
        get_best_result = function() {
            url <- paste(private$results_url, '?order=bestFirst&limit=1&includeConfigurations=true', sep="")
            response <- private$session$get(url)
            response$results[[1]]
        },
        delete = function() {
            private$session$delete(private$self_url)
        }
    ),
    private = list(
        session = NULL,
        self_url = NULL,
        configurations_url = NULL,
        results_url = NULL
    )
)
