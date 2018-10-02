context("Run Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Dummy task"

bool_param <- BoolParameter('my_bool', id='x')
cat_param <- CategoricalParameter('my_cat', values=list('a', 'b', 'c'), default='c', id='y')

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

constraints <- list(
    'if #x == true then #y != "b"'
)

scoring_function <- function(my_bool, my_cat, ints_or_floats) {
    if (isTRUE(my_bool) && my_cat == 'b') {
        stop('Configuation violates constraint!')
    }
    score <- if (isTRUE(my_bool)) 5 else -5
    score <- if (my_cat == 'a') score + 1 else score + 3
    if (!is.null(ints_or_floats$ints)) {
        score <- score + do.call(sum, ints_or_floats$ints)
    } else {
        score <- score * do.call(sum, ints_or_floats$floats)
    }
    score
}

task <- client$create_task(title = title, parameters = parameters, 
                           constraints = constraints, initial_configurations = 3)

run_task_and_verify <- function(number_of_iterations) {
    best_result <- task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations)
    
    expect_false(is.null(best_result$configuration$values))
    
    expected_score <- do.call(scoring_function, best_result$configuration$values)
    expect_equal(expected_score, best_result$score, tolerance=0.01)
}


test_that("Can run task for a fixed number of iterations, then some more iterations", {
    run_task_and_verify(5)
    run_task_and_verify(2)
})

