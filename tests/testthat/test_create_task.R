context("Create Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Dummy task"
parameters <- list(list(type = "boolean", name = "dummy_bool"))

test_that("Task can be created, retrieved and deleted", {
    task <- client$create_task(title = title, parameters = parameters)
    
    expect_equal(title, task$json$title)
    expect_equal(parameters, task$json$parameters)
    expect_equal("max", task$json$goal)
    expect_equal(10, task$json$initialConfigurations)
    expect_null(task$json$targetScore)
    expect_null(task$json$randomSeed)
    expect_null(task$json$userDefined)
    
    expect_equal(task, client$get_task(task$id))
    
    all_tasks = client$get_all_tasks()
    number_of_tasks = length(all_tasks)
    most_recent_task = all_tasks[[number_of_tasks]]
    expect_equal(task, most_recent_task)
    for (existing_task in all_tasks[1:number_of_tasks - 1]) {
        expect_false(isTRUE(all.equal(task, existing_task)))
    }
    
    task$delete()
    
    all_tasks = client$get_all_tasks()
    expect_equal(number_of_tasks - 1, length(all_tasks))
    for (existing_task in all_tasks) {
        expect_false(isTRUE(equals(task, existing_task)))
    }
    
    expect_error(client$get_task(task$id),
                 paste("400 No task found with id", task$id, sep = "="))
})

test_that("Optional arguments can be set", {
    task <- client$create_task(
        title = title,
        parameters = parameters,
        goal = "min",
        target_score = 100,
        initial_configurations = 5,
        random_seed = 123,
        user_defined_data = list(any = 1, data = 2)
    )
    
    expect_equal(title, task$json$title)
    expect_equal(parameters, task$json$parameters)
    expect_equal("min", task$json$goal)
    expect_equal(100, task$json$targetScore)
    expect_equal(5, task$json$initialConfigurations)
    expect_equal(123, task$json$randomSeed)
    expect_equal(list(any = 1, data = 2), task$json$userDefined)
})
