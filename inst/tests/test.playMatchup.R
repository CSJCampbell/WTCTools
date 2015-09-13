
context("check play matchup")

test_that("playMatchup", {
    
    data(wtc2014)
    data(wtc2015_players)
    set.seed(436260)
    ratingtst <- steph(x = wtc2014[, c("round", "player1", "player2", "TP")])
    
    ratingtst$ratings <- rbind(ratingtst$ratings, 
    data.frame(Player = wtc2015_players$Player[!wtc2015_players$Player %in% c(wtc2014$player1, wtc2014$player2)], 
        Rating = 2200,
        Deviation = 300, 
        Games = 0, Win = 0, Draw = 0, Loss = 0, Lag = 0))
    
    listsCCG <- unique(wtc2015_players$List[wtc2015_players$Team %in% c("China", "Canada Goose")])
    pairLookupCCG <- initializeLookup(listsCCG)
    
    
    res <- playMatchup(team1 = "China", team2 = "Canada Goose", 
        data = wtc2015_players, ratings = ratingtst, pairlookup = pairLookupCCG)
    
    cf <- c(1L, 1L, 1L, 0L, 1L)
    
    expect_equal(object = res, expected = cf)
})

