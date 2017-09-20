
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


#' @name wtc
#' @title World Team Championships 2013-2016 Results
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
#' hist(wtc$AP1, col = "#99334455", 
#'     xlim = range(c(wtc$AP1, wtc$AP2)), ylim = c(0, 150), 
#'     xlab = "Army Points", main = "")
#' hist(wtc$AP2, col = "#33887755", add = TRUE)
#' legend("topright", legend = c("Player 1", "Player 2"), 
#'     pch = 22, pt.bg = c("#99334455", "#33887755"))

NULL
