context("Complete and Resume")

client <- OPTaaSClient$new(OPTAAS_URL, OPTAAS_API_KEY)


test_that("Task can be completed and resumed", {
    task <- client$create_task(
        title = "Dummy task", 
        parameters = list(BoolParameter("dummy bool"))
    )
    configuration <- task$generate_configuration()
    result <- Result$new(configuration=configuration, score=123)
    
    task$complete()
    expect_error(task$record_result(result),
                 "Status: 400   Message: Cannot add results to a completed task")
    expect_error(task$generate_configuration(),
                 "Status: 400   Message: Cannot add configurations to a completed task")
    
    task$resume()
    next_configuration <- task$record_result(result)
    another_configuration <- task$generate_configuration()
})
