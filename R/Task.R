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
#' # The function should return a score value 
#' # Then to run the task for n iterations and store the best result:
#' best_result <- task$run(scoring_function, n)
#' # Or to run for at most n iterations, but stop when the score is X or better:
#' best_result <- task$run(scoring_function, n, X)
#' 
#' # Or if you prefer to do things manually:
#' configuration <- task$generate_configuration()
#' result <- Result$new(configuration=configuration, score=score, ...)
#' next_configuration <- task$record_result(result)
#' 
#' task$get_best_result()  # Result with the best score (for single-objective tasks)
#' task$get_pareto_set()  # Set of Pareto front results (for multi-objective tasks)
#'
#' # To use batching (i.e. parallel score evaluation):
#' configurations <- task$generate_configurations(number_of_workers)
#' results <- your_parallel_scoring_function(configurations)
#' next_configurations <- task$record_results(results)
#'
#' # Other functions:
#' task$add_user_defined_configuration()  # Warm-start the optimization with pre-calculated results
#' task$get_results()  # Get all recorded results
#' task$get_surrogate_predictions()  # Get predicted scores for specific configurations
#' task$complete()  # Complete the task (no more configurations or results can be added)
#' task$resume()  # Resume a completed task
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
            private$complete_url <- json$'_links'$complete$href
            private$resume_url <- json$'_links'$resume$href
            private$configurations_url <- json$'_links'$configurations$href
            private$results_url <- json$'_links'$results$href
            private$pareto_url <- json$'_links'$pareto$href
            private$predictions_url <- json$'_links'$predictions$href
        },
        run = function(scoring_function, number_of_iterations, score_threshold=NULL) {
            print(paste("Running", self$json$title, "for", number_of_iterations, "iterations"))

            reached_threshold <- private$make_reached_threshold_function(score_threshold)
            flush.console()
            
            configuration <- self$generate_configuration()
            
            for (i in 1:number_of_iterations) {
                score <- do.call(scoring_function, configuration$values)
                print(paste("Iteration:", i, " ", "Score:", to_string(score)))
                flush.console()
                
                result <- Result$new(configuration=configuration, score=score)
                configuration <- self$record_result(result)
                
                if (reached_threshold(score)) {
                    break
                }
            }
            
            self$complete()
            print('Task Completed')
            flush.console()
            
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
        record_result = function(result) {
            result_url <- result$configuration$'_links'$results$href
            result_body <- result$to_json_without_configuration()
            response <- private$session$post(result_url, result_body)
            response$nextConfiguration
        },
        add_user_defined_configuration = function(configuration_values, score=NULL, variance=NULL, error=NULL,
                                                  user_defined_data=NULL) {
            body <- list(values=configuration_values)
            
            if (!is.null(score)) {
                result <- list(score=score)
                result[["variance"]] <- variance
                result[["userDefined"]] <- user_defined_data
                body[["results"]] <- list(result)
            } else {
                if (!is.null(error)) {
                    result <- list(error=error)
                    result[["userDefined"]] <- user_defined_data
                    body[["results"]] <- list(result)
                }
            }
            
            response <- private$session$post(private$configurations_url, body)
            response$configurations[[1]]
        },
        generate_configurations = function(quantity) {
            response <- private$session$post(private$configurations_url, body=NULL, query=list(quantity=quantity))
            response$configurations
        },
        record_results = function(results) {
            result_jsons <- lapply(results, function(result) { result$to_json() })
            response <- private$session$post(private$results_url, body=list(results=result_jsons))
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
        complete = function() {
            private$session$put(private$complete_url)
        },
        resume = function() {
            private$session$put(private$resume_url)
        },
        delete = function() {
            private$session$delete(private$self_url)
        }
    ),
    private = list(
        session = NULL,
        self_url = NULL,
        complete_url = NULL,
        resume_url = NULL,
        configurations_url = NULL,
        results_url = NULL,
        pareto_url = NULL,
        predictions_url = NULL,
        make_reached_threshold_function = function(score_threshold=NULL) {
            if (is.null(self$json$objectives)) {
                if (is.null(score_threshold)) {
                    if (self$json$goal == "min") {
                        score_threshold = self$json$minKnownScore
                    } else {
                        score_threshold = self$json$maxKnownScore
                    }
                }
            } else {
                if (is.null(score_threshold)) {
                    score_threshold <- list()
                }
                
                for (objective in self$json$objectives) {
                    if (is.null(score_threshold[[objective$id]])) {
                        if (objective$goal == "min") {
                            best_known_score <- objective$minKnownScore
                        } else {
                            best_known_score <- objective$maxKnownScore
                        }
                        if (!is.null(best_known_score)) {
                            score_threshold[[objective$id]] <- best_known_score
                        }
                    }
                }
                
                if (length(score_threshold) == 0) {
                    score_threshold <- NULL
                }
            }
            
            if (is.null(score_threshold)) {
                function(score) { FALSE }
            } else {
                print(paste("(or until score is", to_string(score_threshold), "or better)"))
                
                if (is.null(self$json$objectives)) {
                    if (self$json$goal == "min") {
                        function(score) { score <= score_threshold }
                    } else {
                        function(score) { score >= score_threshold }
                    }
                } else {
                    function(scores) {
                        all_thresholds_reached <- TRUE
                        
                        for (objective in self$json$objectives) {
                            threshold <- score_threshold[[objective$id]]
                            if (!is.null(threshold)) {
                                score <- scores[[objective$id]]
                                if (is.null(score)) {
                                    all_thresholds_reached <- FALSE
                                    break
                                }
                                
                                if (objective$goal == "min") {
                                    if (threshold < score) {
                                        all_thresholds_reached <- FALSE
                                        break
                                    }
                                } else {
                                    if (threshold > score) {
                                        all_thresholds_reached <- FALSE
                                        break
                                    }
                                }
                            }
                        }
                        
                        all_thresholds_reached
                    }
                }   
            }
        }
    )
)


to_string <- function(number_or_list) {
    if (class(number_or_list) == "list") {
        paste(names(number_or_list), number_or_list, sep = "=", collapse = ", ")
    } else {
        number_or_list
    }
}
