context("Hello world")

test_that("hello says Hello, world", {
    expect_equal(hello(), "Hello, world!")
})