% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{CategoricalParameter}
\alias{CategoricalParameter}
\title{CategoricalParameter}
\usage{
CategoricalParameter(name, values, id = NULL, default = NULL,
  optional = FALSE, include_in_default = TRUE)
}
\arguments{
\item{name}{Human-readable name for the parameter, must match argument of scoring function}

\item{values}{List of allowed values (can be string, numeric, boolean or any mixture of those)}

\item{id}{(optional) Unique id for parameter (no spaces allowed)}

\item{default}{(optional) Initial value for parameter (must be one of the allowed values). If not defined, it will be the first allowed value.}

\item{optional}{(optional) Whether the parameter can be omitted in a Configuration}

\item{include_in_default}{(optional) Whether an optional parameter will be included in the default Configuration}
}
\description{
A parameter which can be assigned any value from a specified list of allowed values.
}
