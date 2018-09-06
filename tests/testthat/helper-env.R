getenv_must_exist <- function(var_name) {
    var_value = Sys.getenv(var_name)
    if (var_value == "") {
        stop(simpleError(paste(
            "Please define env var" , var_name, "and restart R"
        )))
    }
    var_value
}

OPTAAS_URL <- getenv_must_exist("OPTAAS_URL")
OPTAAS_API_KEY <- getenv_must_exist("OPTAAS_API_KEY")
