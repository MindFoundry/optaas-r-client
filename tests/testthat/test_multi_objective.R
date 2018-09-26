context("Multi Objective Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Multi Objective Task"

parameters <- list(
    FloatParameter('float', minimum=0, maximum=1),
    IntParameter('int', minimum=1, maximum=4)
)

objectives <- list(
    list(id="objective1"),
    list(id="objective2", goal="min")
)

scoring_function <- function(float, int) {
    score1 <- float * float
    score2 <- (int * int) - 1
    list(objective1=score1, objective2=score2)
}

test_that("Can run a multi-objective task", {
    task <- client$create_task(
        title = title, 
        parameters = parameters, 
        objectives = objectives
    )
    task$run(scoring_function=scoring_function, number_of_iterations=6)
})
