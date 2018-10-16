#' Result
#'
#' The result obtained by taking a Configuration and running it through your scoring function.
#' 
#' Must contain either a score value or an error.
#' You cannot specify variance without score.
#' 
#' @param configuration The Configuration used to obtain this Result.
#' @param score The score obtained. 
#' For multi-objective tasks, a named list of scores for each objective (using the objective IDs as keys).
#' @param variance (optional, defaults to 0) Variance associated with the score. 
#' For multi-objective tasks, a named list  of variances for each objective (using the objective IDs as keys).
#' @param error Any data related to an error encountered while calculating the score.
#' @param user_defined_data (optional) Any other data you wish to store as part of this Result.
#' 
#' @export

Result <- R6::R6Class(
    'Result',
    public = list(
        configuration = NULL,
        score = NULL,
        variance = NULL,
        error = NULL,
        user_defined_data = NULL,
        initialize = function(configuration, score=NULL, variance=NULL, error=NULL, user_defined_data=NULL) {
            if (is.null(score) && is.null(error)) {
                stop("Must specify either score or error")
            }
            if (!is.null(score) && !is.null(error)) {
                stop("Cannot specify both score and error")
            }
            if (is.null(score) && !is.null(variance)) {
                stop("Cannot specify variance without score")
            }
            self$configuration <- configuration
            self$score <- score
            self$variance <- variance
            self$error <- error
            self$user_defined_data <- user_defined_data
        },
        to_json = function() {
            json <- self$to_json_without_configuration()
            json[["configuration"]] <- self$configuration$id
            json
        },
        to_json_without_configuration = function() {
            json <- list()
            json[["score"]] <- self$score
            json[["variance"]] <- self$variance
            json[["error"]] <- self$error
            json[["userDefined"]] <- self$user_defined_data
            json
        }
    )
)
