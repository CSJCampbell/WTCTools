
context("check get statistic")

test_that("getStat", {
    
    pl <- matrix(data = c(0, 30, -30, 0), nrow = 2, 
        dimnames = list(c("a", "b"), c("a", "b")))
    
    dat <- data.frame(round = rep(1:6, each = 2),
        player1 = rep(c("A", "B"), times = 2), 
        player2 = rep(c("B", "A"), times = 2),
        result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
        list1 = c("a", "b", "b", "a"), 
        list2 = c("b", "a", "a", "b"), 
        scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
        stringsAsFactors = FALSE)
    
    out <- getStat(data = dat, pairlookup = pl, result = "result")
    
    expect_equal(object = out, expected = 1.3692667430363)
    
})


test_that("getNewStat", {
    
    pl <- matrix(data = c(0, 30, -30, 0), nrow = 2, 
        dimnames = list(c("a", "b"), c("a", "b")))
    
    dat <- data.frame(round = rep(1:6, each = 2),
        player1 = rep(c("A", "B"), times = 2), 
        player2 = rep(c("B", "A"), times = 2),
        result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
        list1 = c("a", "b", "b", "a"), 
        list2 = c("b", "a", "a", "b"), 
        scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
        stringsAsFactors = FALSE)
    
    out <- getNewStat(val = 60, data = dat, pairlookup = pl, 
        pair = c("a", "b"))
    
    expect_equal(object = out, expected = 2.14598134587704)
    
})
