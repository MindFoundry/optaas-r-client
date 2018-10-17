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

max_scoring_function <- function(my_bool, my_cat, ints_or_floats) {
    if (isTRUE(my_bool) && my_cat == 'b') {
        stop('Configuration violates constraint!')
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

min_scoring_function <- function(my_bool, my_cat, ints_or_floats) {
    -1 * max_scoring_function(my_bool, my_cat, ints_or_floats)
}


run_task_and_verify <- function(task, number_of_iterations, score_threshold=NULL, goal="max",
                                min_known_score=NULL, max_known_score=NULL) {
    task <- client$create_task(title = title, parameters = parameters, goal=goal,
                               constraints = constraints, initial_configurations = 3,
                               min_known_score=min_known_score, max_known_score=max_known_score)
    if (goal == "max") {
        scoring_function = max_scoring_function
    } else {
        scoring_function = min_scoring_function
    }
    
    best_result <- task$run(scoring_function=scoring_function, number_of_iterations=number_of_iterations,
                            score_threshold=score_threshold)
    
    expect_false(is.null(best_result$configuration$values))
    expected_score <- do.call(scoring_function, best_result$configuration$values)
    expect_equal(expected_score, best_result$score, tolerance=0.01)
    
    all_results <- task$get_results()
    result_count <- length(all_results)

    if (is.null(score_threshold)) {
        if (goal == "max") {
            score_threshold = max_known_score
        } else {
            score_threshold = min_known_score
        }
    }
    
    if (is.null(score_threshold)) {
        expect_equal(result_count, number_of_iterations)
    } else {
        expect_lt(result_count, number_of_iterations)
        if (goal == "max") {
            expect_gte(best_result$score, score_threshold)
        } else {
            expect_lte(best_result$score, score_threshold)
        }
    }
    
    # Task should now be complete
    expect_error(task$generate_configuration(),
                 "Status: 400   Message: Cannot add configurations to a completed task")
}


test_that("Can run task for a fixed number of iterations", {
    run_task_and_verify(number_of_iterations=5)
})

test_that("Can run task until max threshold is reached", {
    run_task_and_verify(number_of_iterations=10, score_threshold=18)
})

test_that("Can run task until min threshold is reached", {
    run_task_and_verify(number_of_iterations=10, score_threshold=-18, goal="min")
})

test_that("Can run task until min known score is reached", {
    run_task_and_verify(number_of_iterations=10, min_known_score=-18, goal="min")
})

test_that("Can run task until max known score is reached", {
    run_task_and_verify(number_of_iterations=10, max_known_score=18)
})
