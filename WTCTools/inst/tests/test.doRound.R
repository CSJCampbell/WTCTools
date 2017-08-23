
context("check do round")

test_that("doRound", {
    
    data(wtc2014)
    data(wtc2015_players)
    
    rat <- steph(x = wtc2014[, c("round", "player1", "player2", "TP")])
    
    p2014 <- unique(c(wtc2014$player1, wtc2014$player2))
    p2015 <- unique(wtc2015_players$Player)
    
    rat$ratings <- rbind(rat$ratings, 
        data.frame(Player = p2015[!p2015 %in% p2014], 
            Rating = 2200, Deviation = 300, 
            Games = 0, Win = 0, Draw = 0, Loss = 0, Lag = 0))
    
    set.seed(323522)
    out <- doRound(data = wtc2015_players, results = NULL, 
        ratings = rat, pairlookup = initializeLookup(data = wtc2015_players$List))
    
    cf <- structure(
        list(
            Total.Games.Won = c(3, 4, 4, 1, 2, 2, 3, 2, 4, 
                3, 3, 1, 2, 1, 1, 2, 2, 3, 1, 1, 1, 1, 4, 4, 3, 4, 1, 1, 2, 3, 
                1, 3, 2, 3, 2, 2, 3, 3, 2, 4, 3, 4, 2, 3, 4, 3, 2, 2, 4, 4), 
            Matches.Won = c(1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 
                0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 
                1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1), 
            Opponent.Round.7 = c("France Obelix", 
                    "France Asterix", "Ireland Ceol", "Wales Storm", "Germany Dichter & Denker", 
                    "Scotland Bru", "Sweden Nobel", "Norway Blue", "Finland White", 
                    "Poland Leaders", "Scotland Irn", "Spain North", "Russia Wolves", 
                    "Denmark Red", "Australia Wombat", "Australia Platypus", 
                    "Portugal KULT", "Canada Goose", "Italy Leonardo", "Wales Fire", 
                    "Belgium Blonde", "USA Stars", "Greece Epic", "Netherlands Lion", 
                    "Ukraine", "Northern Ireland 1", "Italy Michelangelo", "Netherlands Hero", 
                    "Poland Grunts", "Czech Republic", "Sweden Dynamite", "Northern Ireland 2", 
                    "Denmark White", "Germany Bier & Brezel", "Switzerland Red", 
                    "USA Stripes", "Finland Blue", "Canada Moose", "England Lions", 
                    "England Roses", "United Nations", "Norway Red", "China", 
                    "Portugal Prime", "Ireland Craic", "Russia Bears", "Middle East", 
                    "Spain South", "Greece Prime", "Belgium Brown")), 
        .Names = c("Total.Games.Won", "Matches.Won", "Opponent.Round.7"), 
        row.names = c("Australia Platypus", 
            "Australia Wombat", "Belgium Blonde", "Belgium Brown", "Canada Goose", 
            "Canada Moose", "China", "Czech Republic", "Denmark Red", "Denmark White", 
            "England Lions", "England Roses", "Finland Blue", "Finland White", 
            "France Asterix", "France Obelix", "Germany Bier & Brezel", "Germany Dichter & Denker", 
            "Greece Epic", "Greece Prime", "Ireland Ceol", "Ireland Craic", 
            "Italy Leonardo", "Italy Michelangelo", "Middle East", "Netherlands Hero", 
            "Netherlands Lion", "Northern Ireland 1", "Northern Ireland 2", 
            "Norway Blue", "Norway Red", "Poland Grunts", "Poland Leaders", 
            "Portugal KULT", "Portugal Prime", "Russia Bears", "Russia Wolves", 
            "Scotland Bru", "Scotland Irn", "Spain North", "Spain South", 
            "Sweden Dynamite", "Sweden Nobel", "Switzerland Red", "USA Stars", 
            "USA Stripes", "Ukraine", "United Nations", "Wales Fire", "Wales Storm"), 
        class = "data.frame")

    expect_equal(object = out, expected = cf)
    
})

