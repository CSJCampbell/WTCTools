
#' Warmachine World Team Championships Player and List Rankings
#' 
#' Some utilities for extracting warcaster/warlock pairings 
#' from the WTC matchup results.
#' 
#' WARMACHINE is a registered trademark of Privateer Press Inc.
#' 
#' @docType package
#' @name WTCTools-package
#' @import PlayerRatings
#' @import dplyr
#' @author Lacerto

NULL


#' @name wtc2013
#' @title World Team Championships 2013 Results
#' @description Provided by Peter <committee@@wmh-wtc.com>
#' and converted to wide format so that every match is represented 
#' by one record.
#' 
#' @docType data
#' @format Data frame with columns:\itemize{
#'     \item game_id integer unique identifier for every match
#'     \item round integer for rounds 1 to 5
#'     \item TP integer 0 if player 1 lost or 1 if player 1 won
#'     \item victory_condition character description of how match ended
#'     \item scenario character name of scenario played
#'     \item player1 character name of player
#'     \item team1 character team name
#'     \item list1 character warcaster/warlock
#'     \item faction1 character faction
#'     \item CP1 integer control points scored
#'     \item AP1 integer army points scored
#'     \item player2 character name of player
#'     \item team2 character team name
#'     \item list2 character warcaster/warlock
#'     \item faction2 character faction
#'     \item CP2 integer control points scored
#'     \item AP2 integer army points scored
#' }
#' @keywords datasets
#' @examples
#' hist(wtc2013$AP1, col = "#99334455", 
#'     xlim = range(c(wtc2013$AP1, wtc2013$AP2)), ylim = c(0, 150), 
#'     xlab = "Army Points", main = "")
#' hist(wtc2013$AP2, col = "#33887755", add = TRUE)
#' legend("topright", legend = c("Player 1", "Player 2"), 
#'     pch = 22, pt.bg = c("#99334455", "#33887755"))

NULL


#' @name wtc2014
#' @title World Team Championships 2014 Results
#' @description Downloaded from http://wmh-wtc.com/query.php
#' and converted to wide format so that every match is represented 
#' by one record.
#' 
#' @docType data
#' @format Data frame with columns:\itemize{
#'     \item game_id integer unique identifier for every match
#'     \item round integer for rounds 1 to 6
#'     \item TP integer 1 (players are ordered so that player 1 is always the winner)
#'     \item victory_condition character description of how match ended
#'     \item player1 character name of player
#'     \item team1 character team name
#'     \item list1 character warcaster/warlock
#'     \item faction1 character faction
#'     \item CP1 integer control points scored
#'     \item AP1 integer army points scored
#'     \item player2 character name of player
#'     \item team2 character team name
#'     \item list2 character warcaster/warlock
#'     \item faction2 character faction
#'     \item CP2 integer control points scored
#'     \item AP2 integer army points scored
#' }
#' @keywords datasets
#' @examples
#' hist(wtc2014$AP1, col = "#99334455", 
#'     xlim = range(c(wtc2014$AP1, wtc2014$AP2)), ylim = c(0, 150), 
#'     xlab = "Army Points", main = "")
#' hist(wtc2014$AP2, col = "#33887755", add = TRUE)
#' legend("topright", legend = c("Player 1", "Player 2"), 
#'     pch = 22, pt.bg = c("#99334455", "#33887755"))

NULL


#' @name wtc2015_players
#' @title World Team Championships 2015 Players
#' @description Downloaded from http://www.discountgamesinc.com/tournaments/
#' 
#' @docType data
#' @format Data frame with columns:\itemize{
#'     \item Team character team name
#'     \item Faction character faction
#'     \item Player character name of player
#'     \item List character warcaster/warlock
#'     \item Objective character name of ojective selected
#'     \item NModels integer number of entries in list
#' }
#' @keywords datasets
#' @examples
#' table(wtc2015_players$Objective)
#' par(las = 2, mar = c(10, 4, 1, 1))
#' barplot(table(wtc2015_players$Faction))

NULL
