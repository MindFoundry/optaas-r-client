context("Multi Objective Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

parameters <- list(
    FloatParameter('x', minimum=0, maximum=1),
    FloatParameter('y', minimum=0, maximum=1)
)

scoring_function <- function(x, y) {
    list(sum=x + y, product=x * y)
}

test_that("Can run a multi-objective task", {
    number_of_iterations = 6
    
    objectives <- list(
        Objective("sum"),
        Objective("product", goal="min")
    )
    
    task <- client$create_task(
        title = "No thresholds", 
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
    number_of_iterations = 10
    
    objectives <- list(
        Objective("sum", min_known_score=0, max_known_score=2),
        Objective("product", goal="min", min_known_score=0, max_known_score=1)
    )

    task <- client$create_task(
        title = "With thresholds", 
        parameters = parameters, 
        objectives = objectives,
        initial_configurations = 3
    )
    
    score_threshold=list(sum=1.01, product=0.99)
    pareto_set <- task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations,
                           score_threshold=score_threshold)
    
    all_results <- task$get_results()
    result_count <- length(all_results)
    expect_lt(result_count, number_of_iterations)
    
    last_result <- all_results[[result_count]]
    expect_gte(last_result$score$sum, 1.01)
    expect_lte(last_result$score$product, 0.99)
    
    expect_lt(length(pareto_set), result_count)
    expect_gt(length(pareto_set), 0)
})


test_that("Can run a multi-objective task with partial threshold", {
    number_of_iterations = 10

    objectives <- list(
        Objective("sum"),
        Objective("product", goal="min", max_known_score=1)
    )
    
    task <- client$create_task(
        title = "Partial threshold", 
        parameters = parameters, 
        objectives = objectives,
        initial_configurations = 3
    )
    
    score_threshold=list(sum=1.01)
    pareto_set <- task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations,
                           score_threshold=score_threshold)
    
    all_results <- task$get_results()
    result_count <- length(all_results)
    expect_lt(result_count, number_of_iterations)
    
    last_result <- all_results[[result_count]]
    expect_gte(last_result$score$sum, 1.01)

    expect_lt(length(pareto_set), result_count)
    expect_gt(length(pareto_set), 0)
    
})

test_that("Threshold defaults to min/max known scores", {
    number_of_iterations = 10
    
    objectives <- list(
        Objective("sum", max_known_score=1.01),
        Objective("product", goal="min", min_known_score=0.99)
    )
    
    task <- client$create_task(
        title = "Defaulted thresholds", 
        parameters = parameters, 
        objectives = objectives,
        initial_configurations = 3
    )
    
    pareto_set <- task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations)
    
    all_results <- task$get_results()
    result_count <- length(all_results)
    expect_lt(result_count, number_of_iterations)
    
    last_result <- all_results[[result_count]]
    expect_gte(last_result$score$sum, 1.01)
    expect_lte(last_result$score$product, 0.99)
    
    expect_lt(length(pareto_set), result_count)
    expect_gt(length(pareto_set), 0)
})

