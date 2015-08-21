
context("check get statistic")

test_that("getStat", {
    
    dat <- data.frame(round = rep(1:6, each = 2),
        player1 = rep(c("A", "B"), times = 2), 
        player2 = rep(c("B", "A"), times = 2),
        result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
        list1 = c("a", "b", "b", "a"), 
        list2 = c("b", "a", "a", "b"), 
        scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
        stringsAsFactors = FALSE)
    
    out <- updateLookup(data = dat)
    
    expect_equal(object = out, 
        expected = structure(
            c(0, 229.512748310598, -229.512748310598, 0), 
            .Dim = c(2L, 2L), 
            .Dimnames = list(c("a", "b"), c("a", "b"))))
    
})
