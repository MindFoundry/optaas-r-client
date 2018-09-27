#' Objective
#'
#' Specification of an objective for a multi-objective Task.
#' 
#' @param id Unique id for the objective
#' @param goal (optional, defaults to "max") Use "min" if the goal is to minimise the score for this objective.
#' @param min_known_score (optional) The lowest possible score value, if known.
#' @param max_known_score (optional) The highest possible score value, if known.
#'
#' @export

Objective <- function(id, goal=NULL, min_known_score=NULL, max_known_score=NULL) {
    json <- list(id=id)
    json[["goal"]] <- goal
    json[["minKnownScore"]] <- min_known_score
    json[["maxKnownScore"]] <- max_known_score
    json
}
