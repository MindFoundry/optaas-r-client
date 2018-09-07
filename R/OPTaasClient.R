API_ROOT = '/api/v1'

#' OPTaaSClient
#'
#' Sets up a connection to OPTaaS and allows you to create a Task, retrieve existing Tasks etc.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' client <- OPTaaSClient$new("Your OPTaaS URL", "Your OPTaaS API Key")
#'
#' # Create a task:
#' task <- client$create_task(
#'     title="Dummy task",
#'     parameters=list(
#'         BoolParameter("my_bool"),
#'         CategoricalParameter("my_cat", values=list("a", "b", "c")),
#'         ChoiceParameter('ints_or_floats', choices=list(
#'             GroupParameter('ints', items=list(
#'                 IntParameter('my_int', minimum=0, maximum=20),
#'                 IntParameter('my_optional_int', minimum=-10, maximum=10, optional=TRUE)
#'             )),
#'             GroupParameter('floats', items=list(
#'                 FloatParameter('float1', minimum=0, maximum=1),
#'                 FloatParameter('float2', minimum=0.5, maximum=4.5)
#'             ))
#'         ))
#'     ),
#'     # optional arguments
#'     goal="min",  # default is "max"
#'     target_score=100,  # optimal score (if known)
#'     initial_configurations=5,  # default is 10
#'     random_seed=123,  # use only if you need reproducible results
#'     user_defined_data=list(any="data")  # any other data you wish to store
#' )
#' 
#' # Get a task:
#' task <- client$get_task("Task ID")
#' 
#' # Get all tasks:
#' tasks <- client$get_all_tasks()
#' }

OPTaaSClient <- R6::R6Class(
    'OPTaaSClient',
    public = list(
        initialize = function(server_url, api_key) {
            private$session <- OPTaaSSession$new(server_url, api_key)
            private$tasks_endpoint <- tryCatch({
                private$session$get(API_ROOT)$'_links'$tasks$href
            },
            error = function(cond) {
                paste(API_ROOT, 'tasks', sep = '/')
            })
        },
        create_task = function(title,
                               parameters,
                               goal = "max",
                               target_score = NULL,
                               initial_configurations = NULL,
                               random_seed = NULL,
                               user_defined_data = NULL) {
            body <- list(
                title = title,
                parameters = parameters,
                goal = goal,
                targetScore = target_score,
                initialConfigurations = initial_configurations,
                randomSeed = random_seed,
                userDefined = user_defined_data
            )
            task_json <- private$session$post(private$tasks_endpoint, body)
            Task$new(task_json, private$session)
        },
        get_all_tasks = function() {
            tasks <- list()
            tasks_json <- private$session$get(private$tasks_endpoint)$tasks
            for (i in 1:length(tasks_json)) {
                tasks[[i]] <- Task$new(tasks_json[[i]], private$session)
            }
            tasks
        },
        get_task = function(task_id) {
            endpoint <- paste(private$tasks_endpoint, task_id, sep = '/')
            task_json <- private$session$get(endpoint)
            Task$new(task_json, private$session)
        }
    ),
    private = list(
        session = NULL,
        tasks_endpoint = NULL
    )
)
