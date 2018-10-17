context("Multi Objective Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Multi Objective Task"

parameters <- list(
    FloatParameter('x', minimum=0, maximum=1),
    FloatParameter('y', minimum=0, maximum=1)
)

objectives <- list(
    Objective("sum"),
    Objective("product", goal="min")
)

scoring_function <- function(x, y) {
    list(sum=x + y, product=x * y)
}

number_of_iterations = 6

test_that("Can run a multi-objective task", {
    task <- client$create_task(
        title = title, 
        parameters = parameters, 
        objectives = objectives,
        initial_configurations = 3
    )
    
    expected_objectives <- list(
        list(id="sum", goal="max"),
        list(id="product", goal="min")
    )
    expect_equal(expected_objectives, task$json$objectives)
    
    pareto_set <- task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations)
    expect_lt(length(pareto_set), number_of_iterations)
    expect_gt(length(pareto_set), 0)
    for (result in pareto_set) {
        expected_score <- do.call(scoring_function, result$configuration$values)
        expect_equal(expected_score, result$score, tolerance=0.01)
    }
    
    all_results <- task$get_results()
    expect_equal(number_of_iterations, length(all_results))
    for (result in all_results) {
        expected_score <- do.call(scoring_function, result$configuration$values)
        expect_equal(expected_score, result$score, tolerance=0.01)
    }
})


test_that("Can run a multi-objective task with thresholds", {
    # TODO
})
