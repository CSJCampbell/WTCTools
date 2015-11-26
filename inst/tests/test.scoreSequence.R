
context("check scoreSequence")

test_that("scoreSequence", {
    
    expect_equal(object = scoreSequence(guess = c("c", "b", "a"), result = letters), 
        expected = structure(4/3, n = 3L))
    
    expect_equal(object = scoreSequence(guess = letters[1:3], result = letters), 
        expected = structure(0, n = 3L))
    
    expect_equal(object = suppressWarnings(
            scoreSequence(guess = "ZZ", result = letters)), 
        expected = structure(26, n = 1L))
    
    expect_equal(object = scoreSequence(guess = c("a", "a", "a"), result = letters), 
        expected = structure(1, n = 3L))
    
    expect_equal(object = suppressWarnings(
            scoreSequence(guess = c("aa", "ab", "a"), result = paste0(letters, "b"))), 
        expected = structure(29/3, n = 3L))
    
    expect_equal(object = scoreSequence(guess = "Australia", result = leaderboard15$Team), 
        expected = structure(2, n = 1L))
    
    expect_equal(object = suppressWarnings(
            scoreSequence(guess = letters, result = "ZZ")), 
        expected = structure(1, n = 26L))
    
    expect_equal(object = scoreSequence(guess = NA, result = c("a", "b", "c")), 
        expected = structure(NA_integer_, n = 0L))
    
    expect_equal(object = scoreSequence(guess = c(rep("", times = 10), "a"), result = letters), 
        expected = structure(10, n = 1L))
    
    expect_equal(object = scoreSequence(guess = c(rep("", times = 3), "a", "b", "c"), result = letters), 
        expected = structure(3, n = 3L))
    
    expect_equal(object = scoreSequence(guess = c(rep("", times = 3), "c", "b", "a"), result = letters), 
        expected = structure(3, n = 3L))
    
    expect_equal(object = scoreSequence(guess = c("m", "f", "q", "n", "j", "k", 
            "d", "h", "u", "x", "y", "i", "p", "c", "r", "g", "a", "v", "b", 
            "l", "w", "t", "s", "z", "e", "o"), result = letters), 
        expected = structure(8, n = 26L))
    
    expect_equal(object = scoreSequence(guess = c("m", "f", "q", "n", "j", "k", 
            "d", "h", "u", "x", "y", "i", "p", "c", "r", "g", "a", "v", "b", 
            "l", "w", "t", "s", "z", "e"), result = letters), 
        expected = structure(7.88, n = 25L))
    
    expect_equal(object = scoreSequence(guess = c("m", "f", "q", "n", "j", "k", 
            "d", "h", "u", "x", "", "i", "p", "c", "r", "g", "a", "v", "b", 
            "l", "w", "t", "s", "z", "e", "o"), result = letters), 
        expected = structure(7.76, n = 25L))
    
})
