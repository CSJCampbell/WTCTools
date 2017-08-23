
context("check harmonizeCasters")

test_that("harmonizeCasters", {
    expect_equal(suppressWarnings(harmonizeCasters(c("Vlad", "Vlad1", "Vald 1"))), 
        expected = c("Vladimir 1", "Vladimir 1", "Vald 1"))
})
