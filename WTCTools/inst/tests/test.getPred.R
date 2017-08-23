
context("check get predicted value")

test_that("getPred", {
    
    pl <- matrix(data = c(0, 30, -30, 0), nrow = 2, 
        dimnames = list(c("a", "b"), c("a", "b")))
    
    dat <- data.frame(round = rep(1:6, each = 2),
        player1 = rep(c("A", "B"), times = 2), 
        player2 = rep(c("B", "A"), times = 2),
        result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
        list1 = c("a", "b", "b", "a"), 
        list2 = c("b", "a", "a", "b"), stringsAsFactors = FALSE)
    
    out <- getPred(data = dat, pairlookup = pl, result = "result")
    
    expect_equal(object = out, expected = 
        rep(c(0.480444519243456, 0.519555480756544, 0.555395831458498, 0.444604168541502), times = 3))
    
})
