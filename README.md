# Mind Foundry OPTaaS R Client

![GitHub Release](https://img.shields.io/github/release/mindfoundry/optaas-r-client.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![tutorials-binder](https://img.shields.io/badge/tutorials-binder-ff69b4.svg)](https://tutorial.optaas.mindfoundry.ai)

[OPTaaS](https://mindfoundry.ai/optaas) (Optimization as a Service) is a general-purpose Bayesian optimizer developed
by [Mind Foundry](https://mindfoundry.ai) which provides optimal hyper-parameter configurations via web-services.
It can handle any parameter type and does not need to know the underlying process, models, or data.

This R client makes it easier to interact with the web service and integrate with your R code.

## Installation

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("MindFoundry/optaas-r-client")
library(optaas.client)
```

## Usage

Connect to your OPTaaS server:
```{r example, eval = FALSE}
client <- OPTaaSClient$new("Your OPTaaS URL", "Your OPTaaS API Key")
```

Define your parameters:
```{r example, eval = FALSE}
parameters <- list(
    BoolParameter('my_bool'),
    CategoricalParameter('my_cat', values=list('a', 'b', 'c'), default='c'),
    ChoiceParameter('ints_or_floats', choices=list(
        GroupParameter('ints', items=list(
            IntParameter('my_int', minimum=0, maximum=20), 
            IntParameter('my_optional_int', minimum=-10, maximum=10, optional=TRUE)
        )),
        GroupParameter('floats', items=list(
            FloatParameter('float1', minimum=0, maximum=1),
            FloatParameter('float2', minimum=0.5, maximum=4.5)
        ))
    ))
)
```

Define your scoring function:
```{r example, eval = FALSE}
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
```

Create your task:
```{r example, eval = FALSE}
task <- client$create_task(
    title="Dummy task",
    parameters=parameters,
    goal="min",  # optional (default is "max")
    min_known_score=-22, max_known_score=44  # optional
)
```

Run your task:
```{r example, eval = FALSE}
best_result <- task$run(scoring_function=scoring_function, number_of_iterations=20)
```

See also [our tutorial notebooks](https://mybinder.org/v2/gh/MindFoundry/optaas-tutorials/1.3.5?filepath=notebooks).

## Development

[RStudio](https://www.rstudio.com/) is recommended.

Each class/function should be documented using [roxygen comments](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html). Use `Ctrl+Shift+D` in RStudio to generate the Rd files.

To run tests: set env vars `OPTAAS_URL` and `OPTAAS_API_KEY` accordingly, then `Ctrl+Shift+T`.

To run all checks (including tests): `Ctrl+Shift+E`.

## Deployment

For each production release, set the version in the `DESCRIPTION` file to match the version of the OPTaaS server and Python client. Then [create a new GitHub Release](https://github.com/MindFoundry/optaas-r-client/releases/new) with the same version.
