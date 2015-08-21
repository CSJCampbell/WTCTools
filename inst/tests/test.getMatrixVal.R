
context("check get matrix value")

test_that("getMatrixVal", {

    m1 <- matrix(1:9, nrow = 3, dimnames = list(letters[1:3], letters[1:3])) / 10
    
    out <- getMatrixVal(list1 = "b", list2 = "a", x = m1)
    
    expect_equal(object = out, expected = 0.2)
    
    out <- getMatrixVal(
        list1 = c("b", "b", "c", "a"), 
        list2 = c("a", "c", "c", "b"), x = m1)

    expect_equal(object = out, expected = c(0.2, 0.8, 0.9, 0.4))
    
})


test_that("selectSequences", {

    m1 <- matrix(0, nrow = 3, ncol = 3, dimnames = list(letters[1:3], letters[1:3])) / 10
    
    out <- setMatrixVal(list1 = "b", list2 = "a", x = m1, val = -2)
    
    expect_equal(object = out, 
        expected = structure(
            c(0, -2, 0, 2, 0, 0, 0, 0, 0), 
            .Dim = c(3L, 3L), 
            .Dimnames = list(c("a", "b", "c"), c("a", "b", "c"))))
    
    out <- setMatrixVal(
        list1 = c("a", "b", "c"), 
        list2 = c("b", "c", "a"), x = m1, val = 1:3)
    
    expect_equal(object = out, expected = structure(
            c(0, -1, 3, 1, 0, -2, -3, 2, 0), 
            .Dim = c(3L, 3L), 
            .Dimnames = list(c("a", "b", "c"), c("a", "b", "c"))))
    
})
