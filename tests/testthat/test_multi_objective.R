context("Multi Objective Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Multi Objective Task"

parameters <- list(
    FloatParameter('float', minimum=0, maximum=1),
    IntParameter('int', minimum=1, maximum=4)
)

objectives <- list(
    Objective("objective1"),
    Objective("objective2", goal="min"),
    Objective("objective3", min_known_score=-123, max_known_score=456)
)

scoring_function <- function(float, int) {
    score1 <- float * float
    score2 <- (int * int) - 1
    list(objective1=score1, objective2=score2)
}

number_of_iterations = 6

test_that("Can run a multi-objective task", {
    task <- client$create_task(
        title = title, 
        parameters = parameters, 
        objectives = objectives
    )
    
    expected_objectives <- list(
        list(id="objective1", goal="max"),
        list(id="objective2", goal="min"),
        list(id="objective3", minKnownScore=-123, maxKnownScore=456, goal="max")
    )
    expect_equal(expected_objectives, task$json$objectives)
    
    task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations)
    expect_equal(number_of_iterations, length(task$get_results()))
})
