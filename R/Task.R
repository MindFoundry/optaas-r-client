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
#' # The function should return a score value. 
#' # Then to run the task for n iterations and store the best result:
#' best_result <- task$run(scoring_function, n)
#' 
#' # Or if you prefer to do things manually:
#' task$generate_configuration()  # Generate a configuration
#' task$record_result(configuration, score)  # Record your score and return a new configuration
#' task$get_best_result()  # Return the result with the best score (for single-objective tasks)
#' task$get_pareto_set()  # Return the set of Pareto front results (for multi-objective tasks)
#'
#' # To use batching (i.e. parallel score evaluation):
#' configurations <- task$generate_configurations(number_of_workers)
#' results <- your_parallel_scoring_function(configurations)
#' # NB results needs to be a list of (configuration$id, score) lists
#' next_configurations <- task$record_results(results)
#'
#' # Other functions:
#' task$get_results()  # Get all recorded results
#' task$get_surrogate_predictions()  # Get predicted scores for specific configurations
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
            private$pareto_url <- json$'_links'$pareto$href
            private$predictions_url <- json$'_links'$predictions$href
        },
        run = function(scoring_function, number_of_iterations) {
            configuration <- self$generate_configuration()
            
            for (i in 1:number_of_iterations) {
                score <- do.call(scoring_function, configuration$values)
                print(paste("Iteration:", i, " ", "Score:", to_string(score)))
                flush.console()
                configuration <- self$record_result(configuration, score)
            }
            
            if (is.null(self$json$objectives)) {
                self$get_best_result()
            } else {
                self$get_pareto_set()
            }
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
        generate_configurations = function(quantity) {
            response <- private$session$post(private$configurations_url, body=NULL, query=list(quantity=quantity))
            response$configurations
        },
        record_results = function(results) {
            response <- private$session$post(private$results_url, body=list(results=results))
            response$nextConfigurations
        },
        get_results = function(best_first = FALSE, limit = NULL) {
            query <- list()
            if (isTRUE(best_first)) {
                query$order <- "bestFirst"
            }
            if (!is.null(limit)) {
                query$limit <- limit
            }
            response <- private$session$get(private$results_url, query=query)
            response$results
        },
        get_best_result = function() {
            self$get_results(best_first=TRUE, limit=1)[[1]]
        },
        get_pareto_set = function() {
            response <- private$session$get(private$pareto_url)
            response$results
        },
        get_surrogate_predictions = function(configurations) {
            wrapped_configurations <- lapply(configurations, function(values) { list(values=values) })
            body <- list(configurations = wrapped_configurations)
            response <- private$session$post(private$predictions_url, body)
            response$predictions
        },
        delete = function() {
            private$session$delete(private$self_url)
        }
    ),
    private = list(
        session = NULL,
        self_url = NULL,
        configurations_url = NULL,
        results_url = NULL,
        pareto_url = NULL,
        predictions_url = NULL
    )
)


to_string <- function(score) {
    if (class(score) == "list") {
        paste(names(score), score, sep = "=", collapse = ", ")
    } else {
        score
    }
}
