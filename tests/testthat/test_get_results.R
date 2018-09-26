context("Get results")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Dummy task"
parameters <- list(BoolParameter('bool1'), BoolParameter('bool2'))
scoring_function <- function(bool1, bool2) {
    score1 <- if (isTRUE(bool1)) 3 else -2
    score2 <- if (isTRUE(bool2)) 5 else -6
    score1 * score2
}

number_of_iterations = 4
limit = 2

test_that("Can run task and then get recorded results", {
    task <- client$create_task(title = title, parameters = parameters)
    best_result <- task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations)
    
    # Get all results in posted order
    all_results <- task$get_results()
    expect_equal(number_of_iterations, length(all_results))
    
    # Get all results best-first
    best_first_results <- task$get_results(best_first=TRUE)
    expect_equal(number_of_iterations, length(best_first_results))
    expect_equal(best_result, best_first_results[[1]])
    
    # Get limited results
    limited_results <- task$get_results(limit=limit)
    expect_equal(limit, length(limited_results))
    expect_equal(limited_results, all_results[1:limit])
    
    # Get limited results best-first
    limited_best_first_results <- task$get_results(limit=limit, best_first=TRUE)
    expect_equal(limit, length(limited_best_first_results))
    expect_equal(limited_best_first_results, best_first_results[1:limit])
})
