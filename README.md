# Mind Foundry OPTaaS R Client

[![tutorials-binder](https://img.shields.io/badge/tutorials-binder-ff69b4.svg)](https://tutorial.optaas.mindfoundry.ai)

[OPTaaS](https://mindfoundry.ai/optaas) (Optimization as a Service) is a general-purpose Bayesian optimizer developed
by [Mind Foundry](https://mindfoundry.ai) which provides optimal hyper-parameter configurations via web-services.
It can handle any parameter type and does not need to know the underlying process, models, or data.

This R client makes it easier to interact with the web service and integrate with your R code.

## Installation

You can install optaas.client from github with:

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("MindFoundry/optaas-r-client")
library("optaas.client")
```

## Example

```{r example, eval = FALSE}
client <- OPTaaSClient$new("Your OPTaaS URL", "Your OPTaaS API Key")

# Create a task:
task <- client$create_task(
    title="Dummy task",
    parameters=list(
        list(type="boolean", name="dummy_bool")
    ),
    # optional arguments
    goal="min",  # default is "max"
    target_score=100,  # optimal score (if known)
    initial_configurations=5,  # default is 10
    random_seed=123,  # use only if you need reproducible results
    user_defined_data=list(any=data)
)
```
