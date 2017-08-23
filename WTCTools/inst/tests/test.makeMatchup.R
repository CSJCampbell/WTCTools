
context("check make all matchups")

test_that("makeAllMatchups", {
    
    eg <- data.frame(Total.Games.Won = c(5, 4, 0, 1), 
        Matches.Won = c(1, 1, 0, 0), 
        Opponent.Round.1 = c("c", "d", "a", "b"), 
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:4]
    
    expect_equal(object = makeAllMatchups(results = eg), 
        expected = c("a", "b", "c", "d"))
    
    eg <- data.frame(Total.Games.Won = c(5, 4, 0, 1, 2, 3, 3, 2), 
        Matches.Won = c(1, 1, 0, 0, 0, 1, 1, 0), 
        Opponent.Round.1 = c("c", "d", "a", "b", "f", "e", "h", "g"), 
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:8]
    
    set.seed(34325)
    expect_equal(object = makeAllMatchups(results = eg), 
        expected = c("a", "f", "b", "g", "c", "e", "d", "h"))
    
    set.seed(24325)
    expect_equal(object = makeAllMatchups(results = eg), 
        expected = c("a", "f", "b", "g", "c", "d", "e", "h"))
    
    # handle pairdown
    eg <- data.frame(Total.Games.Won = c(5, 4, 3, 2, 0, 1), 
        Matches.Won = c(1, 1, 1, 0, 0, 0), 
        Opponent.Round.1 = c("e", "d", "f", "b", "a", "c"), 
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:6]
    
    set.seed(43256)
    expect_equal(object = makeAllMatchups(results = eg), 
        expected = c("b", "c", "d", "a", "e", "f"))
    
    eg <- data.frame(Total.Games.Won = c(10, 6, 7, 4, 0, 1, 2, 4, 3, 3), 
        Matches.Won = c(2, 2, 2, 0, 0, 0, 1, 1, 1, 1), 
        Opponent.Round.1 = c("e", "i", "f", "g", "a", "c", "d", "j", "b", "h"), 
        Opponent.Round.2 = c("d", "j", "g", "a", "h", "i", "c", "e", "f", "b"),
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:10]
    
    # note a gets pairdown twice
    set.seed(43256)
    expect_equal(object = makeAllMatchups(results = eg), 
        expected = c("a", "h", "b", "c", "d", "j", "g", "i", "e", "f"))
    
})


context("check make matchup")

test_that("makeMatchup", {
    
    eg <- data.frame(Total.Games.Won = c(5, 4), 
        Matches.Won = c(1, 1), 
        Opponent.Round.1 = c("c", "d"), 
        stringsAsFactors = FALSE)
    rownames(eg) <- c("a", "b")
    expect_equal(object = makeMatchup(results = eg), 
        expected = c("a", "b"))
    
    eg <- data.frame(Total.Games.Won = c(5, 4, 4, 3), 
        Matches.Won = c(1, 1, 1, 1), 
        Opponent.Round.1 = c("e", "c", "b", "f"), 
        Opponent.Round.2 = c("b", "a", "f", "e"), 
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:4]
    expect_equal(object = makeMatchup(results = eg), 
        expected = c("b", "d", "a", "c"))

    eg <- data.frame(Total.Games.Won = c(10, 6, 7, 9), 
        Matches.Won = c(1, 1, 1, 1), 
        Opponent.Round.1 = c("b", "a", "d", "c"), 
        Opponent.Round.2 = c("d", "b", "c", "a"), 
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:4]
    set.seed(32552)
    expect_equal(object = makeMatchup(results = eg), 
        expected = c("a", "c", "b", "d"))
    
    eg <- data.frame(Total.Games.Won = c(8, 7, 8, 7, 6, 6, 7, 8), 
        Matches.Won = c(2, 2, 2, 2, 2, 2, 2, 2), 
        Opponent.Round.1 = c("h", "d", "i", "b", "j", "k", "l", "a"), 
        Opponent.Round.2 = c("c", "n", "a", "p", "e", "f", "q", "r"),
        Opponent.Round.3 = c("o", "s", "h", "g", "r", "t", "d", "c"),
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:8]
    
    set.seed(34325)
    expect_equal(object = makeMatchup(results = eg), 
        expected = c("a", "e", "c", "b", "d", "h", "f", "g"))
    
    set.seed(24325)
    expect_equal(object = makeMatchup(results = eg), 
        expected = c("a", "e", "c", "d", "h", "b", "f", "g"))
    
    eg <- data.frame(Total.Games.Won = c(5, 7, 6, 3, 4, 3, 3, 5), 
        Matches.Won = c(1, 1, 1, 1, 1, 1, 1, 1), 
        Opponent.Round.1 = c("b", "a", "d", "c", "f", "e", "h", "g"), 
        Opponent.Round.2 = c("c", "d", "a", "b", "h", "g", "f", "e"),
        Opponent.Round.3 = c("e", "g", "f", "h", "a", "c", "b", "d"),
        stringsAsFactors = FALSE)
    rownames(eg) <- letters[1:8]
    
    set.seed(34325)
    expect_equal(object = makeMatchup(results = eg), 
        expected = c("a", "g", "b", "e", "c", "h", "d", "f"))
        
    set.seed(14337)
    expect_equal(object = suppressWarnings(makeMatchup(results = eg, maxit = 1)), 
        expected = c("a", "g", "b", "f", "c", "e", "d", "h"))
    
    set.seed(14337)
    expect_equal(object = makeMatchup(results = eg), 
        expected = c("a", "d", "b", "c", "e", "g", "f", "h"))
    
    
})

