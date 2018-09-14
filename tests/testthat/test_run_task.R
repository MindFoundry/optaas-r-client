context("Create Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Dummy task"

bool_param <- BoolParameter('my_bool')
cat_param <- CategoricalParameter('my_cat', values=list('a', 'b', 'c'), default='c')

int_param <- IntParameter('my_int', minimum=0, maximum=20)
optional_int_param <- IntParameter('my_optional_int', minimum=-10, maximum=10, optional=TRUE)

parameters <- list(
    bool_param,
    cat_param,
    ChoiceParameter('ints_or_floats', choices=list(
        GroupParameter('ints', items=list(int_param, optional_int_param)),
        GroupParameter('floats', items=list(
            FloatParameter('float1', minimum=0, maximum=1),
            FloatParameter('float2', minimum=0.5, maximum=4.5)
        ))
    ))
)

scoring_function <- function(my_bool, my_cat, ints_or_floats) {
    score <- if (isTRUE(my_bool)) 5 else -5
    score <- if (my_cat == 'a') score + 1 else score + 3
    if (!is.null(ints_or_floats$ints)) {
        score <- score + do.call(sum, ints_or_floats$ints)
    } else {
        score <- score * do.call(sum, ints_or_floats$floats)
    }
    score
}


test_that("Can run task for a fixed number of iterations, then some more iterations", {
    task <- client$create_task(title = title, parameters = parameters, initial_configurations = 3)
    
    best_result <- task$run(scoring_function=scoring_function, number_of_iterations=5)
    expect_false(is.null(best_result$configuration$values))
    expect_equal(do.call(scoring_function, best_result$configuration$values), best_result$score)
    
    best_result <- task$run(scoring_function=scoring_function, number_of_iterations=2)
    expect_false(is.null(best_result$configuration$values))
    expect_equal(do.call(scoring_function, best_result$configuration$values), best_result$score)
})
