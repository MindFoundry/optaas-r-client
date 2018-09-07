#' BoolParameter
#'
#' A parameter which will be assigned either TRUE or FALSE.
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param default (optional) Initial value for parameter. If not defined, it will be FALSE.
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

BoolParameter <- function(name, id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE) {
    make_parameter_json("boolean", name, id, default, optional, include_in_default)
}

make_parameter_json <- function(type, name, id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE, ...) {
    json = list(type=type, name=name, optional=optional, includeInDefault=include_in_default, ...)
    json[["id"]] <- id
    json[["default"]] <- default
    json
}


#' CategoricalParameter
#'
#' A parameter which can be assigned any value from a specified list of allowed values.
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param values List of allowed values (can be string, numeric, boolean or any mixture of those)
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param default (optional) Initial value for parameter (must be one of the allowed values). If not defined, it will be the first allowed value.
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

CategoricalParameter <- function(name, values, id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE) {
    make_parameter_json("categorical", name, id, default, optional, include_in_default, 
                        enum=values)
}


#' IntParameter
#'
#' A parameter which can be assigned integer values within a specified range (inclusive).
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param minimum Smallest allowed value
#' @param maximum Largest allowed value
#' @param distribution (optional) Distribution of parameter values ("Uniform" or "LogUniform").
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param default (optional) Initial value for parameter (must be within the allowed range). If not defined, it will be the midpoint of the range.
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

IntParameter <- function(name, minimum, maximum, distribution="Uniform", id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE) {
    make_parameter_json("integer", name, id, default, optional, include_in_default, 
                        minimum=minimum, maximum=maximum, distribution=distribution)
}


#' FloatParameter
#'
#' A parameter which can be assigned float values within a specified range (inclusive).
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param minimum Smallest allowed value
#' @param maximum Largest allowed value
#' @param distribution (optional) Distribution of parameter values ("Uniform" or "LogUniform").
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param default (optional) Initial value for parameter (must be within the allowed range). If not defined, it will be the midpoint of the range.
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

FloatParameter <- function(name, minimum, maximum, distribution="Uniform", id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE) {
    make_parameter_json("number", name, id, default, optional, include_in_default, 
                        minimum=minimum, maximum=maximum, distribution=distribution)
}


#' ChoiceParameter
#'
#' A set of parameters, only one of which will be chosen to appear in each Configuration.
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param choices List of parameters to choose from (cannot be empty).
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param default (optional) Initial value for parameter (must be present in the choices list). If not defined, it will be the first item from the list.
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

ChoiceParameter <- function(name, choices, id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE) {
    if (!is.null(default)) {
        if (is.null(default$id)) {
            stop(paste("Must define id for parameter ", default$name, " in order to use as default for choice ", name, "", sep="'"))
        }
        default <- paste('#', default$id, sep="")
    }
    make_parameter_json("choice", name, id, default, optional, include_in_default, 
                        choices=choices)
}


#' GroupParameter
#'
#' A set of parameters, all of which will be included in each Configuration (unless any of them are optional).
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param items List of parameters to be included in group (can be empty).
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param default (optional) Initial value for parameter (must be present in the choices list). If not defined, it will be the first item from the list.
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

GroupParameter <- function(name, items, id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE) {
    make_parameter_json("group", name, id, default, optional, include_in_default, 
                        items=items)
}


#' SubsetParameter
#'
#' A parameter that may contain any subset of a valid set of values.
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param values List of allowed values (can be string, numeric, boolean or any mixture of those)
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param default (optional) Initial value for parameter (must be a subset of the values list). If not defined, it will be the empty set.
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

SubsetParameter <- function(name, values, id=NULL, default=NULL, optional=FALSE, include_in_default=TRUE) {
    make_parameter_json("subset", name, id, default, optional, include_in_default, 
                        allowedValues=values)
}


#' ConstantParameter
#'
#' A parameter which will always be assigned a specified value.
#' 
#' @param name Human-readable name for the parameter, must match argument of scoring function
#' @param value Can be string, numeric or boolean
#' @param id (optional) Unique id for parameter (no spaces allowed)
#' @param optional (optional) Whether the parameter can be omitted in a Configuration
#' @param include_in_default (optional) Whether an optional parameter will be included in the default Configuration
#'
#' @export

ConstantParameter <- function(name, value, id=NULL, optional=FALSE, include_in_default=TRUE) {
    make_parameter_json("constant", name, id, default=NULL, optional=optional, include_in_default=include_in_default, 
                        value=value)
}


