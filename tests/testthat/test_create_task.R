context("Create Task")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)

title <- "Dummy task"

bool_with_id <- BoolParameter("my bool with id", id="bool_id")
parameters <- list(
    CategoricalParameter("my cat", list("a", 1, "1", 1.1, TRUE, FALSE, 0), default=TRUE, id='cat'),
    ChoiceParameter("my choice", choices=list(
        BoolParameter("my bool"),
        bool_with_id,
        BoolParameter("my bool with id and default", default=FALSE, id="bool_id_2"),
        BoolParameter("my optional bool", optional=TRUE),
        BoolParameter("my optional bool not in default", optional=TRUE, include_in_default=FALSE)
    ), default=bool_with_id),
    GroupParameter("ints", items=list(
        IntParameter('my int', minimum=0, maximum=20, default=5, distribution="Uniform", id='int'),
        IntParameter('my optional int', minimum=-10, maximum=10, optional=TRUE, id='opt_int')
    )),
    GroupParameter("empty group", items=list()),
    FloatParameter('float1', minimum=0, maximum=1, default=0.2),
    FloatParameter('float2', minimum=0.5, maximum=4.5, distribution="LogUniform"),
    SubsetParameter('subset with default', values=list("b", 3, TRUE, 1.2), default=list(TRUE, "b")),
    SubsetParameter('subset without default', values=list(FALSE, -2.3)),
    ConstantParameter('constant', value=123.456)
)

constraints <- list(
    '#int * 2 != 14',
    'if #cat == "a" then #opt_int is_present'
)

test_that("Task can be created, retrieved and deleted", {
    task <- client$create_task(title = title, parameters = parameters, constraints = constraints)
    
    expect_equal(title, task$json$title)
    expect_equal(parameters, task$json$parameters)
    expect_equal("max", task$json$goal)
    expect_equal(10, task$json$initialConfigurations)
    expect_equal(constraints, task$json$constraints)
    expect_null(task$json$minKnownScore)
    expect_null(task$json$maxKnownScore)
    expect_null(task$json$objectives)
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
                 paste("Status: 400   Message: No task found with id", task$id, sep = "="))
})

test_that("Optional arguments can be set", {
    task <- client$create_task(
        title = title,
        parameters <- list(BoolParameter("my bool")),
        goal = "min",
        min_known_score = 23, max_known_score = 78,
        initial_configurations = 5,
        random_seed = 123,
        user_defined_data = list(any = 1, data = 2)
    )
    
    expect_equal(title, task$json$title)
    expect_equal(list(list(type="boolean", name="my bool", optional=FALSE, includeInDefault=TRUE)), task$json$parameters)
    expect_equal(list(), task$json$constraints)
    expect_equal("min", task$json$goal)
    expect_equal(23, task$json$minKnownScore)
    expect_equal(78, task$json$maxKnownScore)
    expect_equal(5, task$json$initialConfigurations)
    expect_equal(123, task$json$randomSeed)
    expect_equal(list(any = 1, data = 2), task$json$userDefined)
})

test_that("Cyclical parameters are supported", {
    cyclical <- FloatParameter('cyclical', minimum=-1.1, maximum=2.2, cyclical=TRUE)

    task <- client$create_task(
        title = title,
        parameters <- list(cyclical)
    )
    
    expect_equal(list(cyclical), task$json$parameters)
})
