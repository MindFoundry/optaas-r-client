context("Batching")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

task <- client$create_task(
    title="Batching Test",
    parameters=list(
        FloatParameter('x', minimum = 0, maximum = 1),
        FloatParameter('y', minimum = 0.1, maximum = 2)
    )
)

test_that("Can run task with batching", {
    library(parallel)
    
    number_of_workers <- 4
    
    scoring_function <- function(configuration) {
        x <- configuration$values[["x"]]
        y <- configuration$values[["y"]]
        score <- (x * y) - (x / y)
        variance <- abs(score / 100)
        Result$new(configuration=configuration, score=score, variance=variance)
    }
    
    spin_off_workers_and_get_results <- function(configurations) {
        mclapply(configurations, scoring_function)
    }
    
    configurations <- task$generate_configurations(number_of_workers)
    expect_equal(number_of_workers, length(configurations))
    
    results <- spin_off_workers_and_get_results(configurations)

    next_configurations <- task$record_results(results)
    expect_equal(number_of_workers, length(next_configurations))
    
    next_results <- spin_off_workers_and_get_results(next_configurations)
})

