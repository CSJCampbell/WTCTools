
context("check harmonizeCasters")

test_that("harmonizeCasters", {
    expect_equal(suppressWarnings(harmonizeCasters(c("Vlad", "Vlad1", "Vald 1"))), 
        expected = c("Vladimir 1", "Vladimir 1", "Vald 1"))
    
    expect_equal(harmonizeCasters("Saeryn 2 & Rhyas 2"), 
        expected = "Saeryn 2 & Rhyas 2")
    
    expect_equal(harmonizeCasters(c("Saeryn 2", "Saeryn 2 & Rhyas 2")), 
        expected = c("Saeryn 2 & Rhyas 2", "Saeryn 2 & Rhyas 2"))
})
