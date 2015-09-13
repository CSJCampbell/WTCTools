
context("check get splitting pairs")

test_that("splitPairs", {
    
    expect_equal(object = splitPairs(n = 0), 
        expected = logical(0))
    
    set.seed(23454)
    expect_equal(object = splitPairs(n = 2), 
        expected = c(FALSE, TRUE))
    
    set.seed(234541)
    expect_equal(object = splitPairs(n = 4), 
        expected = c(FALSE, TRUE, TRUE, FALSE))
})
