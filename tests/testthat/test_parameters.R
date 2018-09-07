context("Parameters")

bool_with_id <- BoolParameter("my bool with id", id="bool_id")
bool_without_id <- BoolParameter("my bool without id")
another_bool_with_id <- BoolParameter("another bool with id", id="another_bool_id")

test_that("Parameter with id can be used as default choice", {
    expect_error(ChoiceParameter("my choice", choices=list(bool_with_id, bool_without_id, another_bool_with_id), 
                                 default=another_bool_with_id), NA)
})

test_that("Parameter without id can be first choice", {
    expect_error(ChoiceParameter("my choice", choices=list(bool_without_id, bool_with_id, another_bool_with_id)), NA)
})

test_that("Parameter without id throws error if set as default choice", {
    expect_error(ChoiceParameter("my choice", choices=list(bool_without_id, bool_with_id), default=bool_without_id),
                 "Must define id for parameter 'my bool without id' in order to use as default for choice 'my choice'")
})
