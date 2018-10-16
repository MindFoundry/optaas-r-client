context("Warm Start")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

create_task = function() {
    client$create_task(
        title="Dummy task",
        parameters=list(
            FloatParameter('x', minimum = 0, maximum = 1),
            FloatParameter('y', minimum = 0, maximum = 1)
        )
    )
}

test_that("Can add a user-defined configuration and then record result for it", {
    task <- create_task()
    
    values <- list(x=0.1, y=0.2)
    configuration <- task$add_user_defined_configuration(values)
    expect_equal(values, configuration$values)

    stored_results <- task$get_results()
    expect_equal(0, length(stored_results))
    
    result <- Result$new(configuration=configuration, score=123)
    next_configuration <- task$record_result(result)
})

test_that("Can add a user-defined configuration with a result", {
    task <- create_task()
    
    values <- list(x=0.3, y=0.4)
    score <- 456
    variance <- 0.789
    configuration <- task$add_user_defined_configuration(values, score=score, variance=variance)
    expect_equal(values, configuration$values)
    
    stored_results <- task$get_results()
    expect_equal(1, length(stored_results))
    result <- stored_results[[1]]
    expect_equal(configuration, result$configuration)
    expect_equal(score, result$score)
    expect_equal(variance, result$variance)
    expect_true(is.null(result$error))
    expect_true(is.null(result$userDefined))
})

test_that("Can add a user-defined configuration with an error", {
    task <- create_task()
    
    values <- list(x=0.5, y=0.6)
    error <- "Dummy error"
    custom <- list(a=1, b=345)
    configuration <- task$add_user_defined_configuration(values, error=error, user_defined_data=custom)
    expect_equal(values, configuration$values)
    
    stored_results <- task$get_results()
    expect_equal(1, length(stored_results))
    result <- stored_results[[1]]
    expect_equal(configuration, result$configuration)
    expect_equal(error, result$error)
    expect_equal(custom, result$userDefined)
    expect_true(is.null(result$score))
    expect_true(is.null(result$variance))
})
