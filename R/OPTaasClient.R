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
#'         CategoricalParameter("my_cat", values=list("a", "b", "c"), id="cat"),
#'         ChoiceParameter('ints_or_floats', choices=list(
#'             GroupParameter('ints', items=list(
#'                 IntParameter('my_int', minimum=0, maximum=20, id="int"),
#'                 IntParameter('optional_int', minimum=-10, maximum=10, optional=TRUE, id="opt_int")
#'             )),
#'             GroupParameter('floats', items=list(
#'                 FloatParameter('float1', minimum=0, maximum=1),
#'                 FloatParameter('float2', minimum=0.5, maximum=4.5)
#'             ))
#'         ))
#'     ),
#'     
#'     # for single-objective tasks
#'     goal="min",  # optional, default is "max"
#'     min_known_score=0, max_known_score=100,  # optional
#'     
#'     # for multi-objective tasks
#'     objectives=list(
#'         Objective(id="objective1", goal="min", min_known_score=0, max_known_score=100)
#'         Objective(id="objective2"),  # defaults to goal="max"
#'     ),
#'     
#'     # optional arguments
#'     constraints=list(
#'         '#int * 2 != 14',  # reference parameters using # followed by the parameter id
#'         'if #cat == "a" then #opt_int is_present'
#'     ),
#'     initial_configurations=5,  # default is 10
#'     random_seed=123,  # use only if you need reproducible results
#'     user_defined_data=list(any="data"),  # any other data you wish to store
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
        initialize = function(server_url, api_key, disable_version_check=FALSE) {
            private$session <- OPTaaSSession$new(server_url, api_key, disable_version_check)
            endpoints <- private$session$get(API_ROOT)$'_links'
            private$tasks_endpoint <- endpoints$tasks$href
            private$api_keys_endpoint <- endpoints$apiKeys$href
        },
        create_task = function(title,
                               parameters,
                               constraints = NULL,
                               goal = NULL,
                               min_known_score = NULL,
                               max_known_score = NULL,
                               objectives = NULL,
                               initial_configurations = NULL,
                               random_seed = NULL,
                               user_defined_data = NULL) {
            body <- list(
                title = title,
                parameters = parameters,
                constraints = constraints,
                goal = goal,
                minKnownScore = min_known_score,
                maxKnownScore = max_known_score,
                objectives = objectives,
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
            if (length(tasks_json) > 0) {
                for (i in 1:length(tasks_json)) {
                    tasks[[i]] <- Task$new(tasks_json[[i]], private$session)
                }
            }
            tasks
        },
        get_task = function(task_id) {
            endpoint <- paste(private$tasks_endpoint, task_id, sep = '/')
            task_json <- private$session$get(endpoint)
            Task$new(task_json, private$session)
        },
        get_api_keys = function() {
            api_keys <- list()
            api_keys_json <- private$session$get(private$api_keys_endpoint)$apiKeys
            if (length(api_keys_json) > 0) {
                for (i in 1:length(api_keys_json)) {
                    api_keys[[i]] <- ApiKey$new(api_keys_json[[i]], private$session)
                }
            }
            api_keys
        },
        generate_api_key = function(role="standard") {
            api_key_json <- private$session$post(private$api_keys_endpoint, list(role=role))
            ApiKey$new(api_key_json, private$session)
        }
    ),
    private = list(
        session = NULL,
        tasks_endpoint = NULL,
        api_keys_endpoint = NULL
    )
)
