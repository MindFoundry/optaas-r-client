context("Record result")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

task <- client$create_task(
    title="Dummy task",
    parameters=list(
        FloatParameter('x', minimum = 0, maximum = 1),
        FloatParameter('y', minimum = 0, maximum = 1)
    )
)

configuration <- task$generate_configuration()

test_that("Can record result with score and variance", {
    score <- 123
    variance <- 0.468
    result <- Result$new(configuration=configuration, score=score, variance=variance)
    next_configuration <- task$record_result(result)
    
    stored_results <- task$get_results()
    most_recent_result <- stored_results[[length(stored_results)]]
    
    expect_equal(configuration$id, most_recent_result$configuration$id)
    expect_equal(score, most_recent_result$score)
    expect_equal(variance, most_recent_result$variance)
    expect_true(is.null(most_recent_result$error))
    expect_true(is.null(most_recent_result$user_defined_data))
})

test_that("Can record result with error", {
    error <- list(code=123, message="Dummy error")
    result <- Result$new(configuration=configuration, error=error)
    next_configuration <- task$record_result(result)
    
    stored_results <- task$get_results()
    most_recent_result <- stored_results[[length(stored_results)]]
    
    expect_equal(configuration$id, most_recent_result$configuration$id)
    expect_equal(error, most_recent_result$error)
    expect_true(is.null(most_recent_result$score))
    expect_true(is.null(most_recent_result$variance))
    expect_true(is.null(most_recent_result$user_defined_data))
})

test_that("Can record result with custom data but no variance", {
    score <- 456
    custom <- list(data="some custom data")
    result <- Result$new(configuration=configuration, score=score, user_defined_data=custom)
    next_configuration <- task$record_result(result)
    
    stored_results <- task$get_results()
    most_recent_result <- stored_results[[length(stored_results)]]
    
    expect_equal(configuration$id, most_recent_result$configuration$id)
    expect_equal(score, most_recent_result$score)
    expect_equal(custom, most_recent_result$userDefined)
    expect_equal(0, most_recent_result$variance)
    expect_true(is.null(most_recent_result$error))
})

test_that("Cannot record result with both score and error", {
    expect_error(Result$new(configuration=configuration, score=789, error="Dummy error"), 
                 'Cannot specify both score and error')
})

test_that("Cannot record variance without score", {
    expect_error(Result$new(configuration=configuration, error="Dummy error", variance=0.123), 
                 'Cannot specify variance without score')
})

test_that("Must have either score or error", {
    expect_error(Result$new(configuration=configuration), 
                 'Must specify either score or error')
})
