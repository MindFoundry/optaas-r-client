context("Surrogate Predictions")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

task <- client$create_task(
    title="Basic 2D Example",
    parameters=list(
        FloatParameter('x', minimum = -3, maximum = 1),
        FloatParameter('y', minimum = -6, maximum = 21)
    ),
    goal="min",
    initial_configurations=4
)

scoring_function <- function(x, y) {
    x**2 + y**2
}

test_that("Can get predicted scores for specific configurations", {
    task$run(scoring_function, 10)
    
    configurations <- list(list(x=0, y=0), list(x=-0.5, y=0.5))
    predictions <- task$get_surrogate_predictions(configurations)

    expect_equal(length(predictions), 2)

    expect_equal(predictions[[1]]$mean, 0, tolerance=0.01)
    expect_equal(predictions[[1]]$variance, 0, tolerance=0.01)
    
    expect_equal(predictions[[2]]$mean, 0.5, tolerance=0.05)
    expect_equal(predictions[[2]]$variance, 0, tolerance=0.01)
})
