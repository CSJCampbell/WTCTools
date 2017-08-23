
context("check make pairing data frame")

test_that("makePairing", {
    
    set.seed(3552566)
    dat <- makePairing(team1players = letters[1:5], 
        team2players = letters[11:15], 
        team1lists = LETTERS[1:10], 
        team2lists = LETTERS[11:20])
    
    cf <- structure(
        list(
            round = c(7, 7, 7, 7, 7), 
                team1players = c("d", "e", "a", "c", "b"), 
                team2players = c("m", "l", "o", "k", "n"), 
                team1lists = c("G", "I", "A", "F", "C"), 
                team2lists = c("O", "M", "S", "L", "R")), 
            .Names = c("round", "team1players", "team2players", 
                "team1lists", "team2lists"), 
            row.names = c(NA, -5L), 
            class = "data.frame")
    
    expect_equal(object = dat, expected = cf)
    
    expect_error(makePairing(team1players = character(0), 
        team2players = letters[11:15], 
        team1lists = LETTERS[1:10], 
        team2lists = LETTERS[11:20]), regex = "length")
})
