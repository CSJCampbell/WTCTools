WTCTools
=======

An [R package](http://www.r-project.org/) to optimize list pairing 
for gamma correction of chess rankings. 
See [analytical gaming](http://lacerto1.wordpress.com/) for more information.
Includes 2014 pairing data from the 
[Warmachine/Hordes World Team Championship](https://wmhwtc.wordpress.com/) 
website.

how to use this code
--------

```R
# install devtools for devtools::install_github
install.packages("devtools")
library(devtools)
# install WTCTools
install_github("CSJCampbell/WTCTools/WTCTools")
```

This package also includes tests in **testthat** format. 
From R run the call `test_package("WTCTools")`.
This package contains the WTC dataset for 2013 - 2015.
Player 1 won when TP is 1; Player 2 won when TP is 0.
```R
library(WTCTools)
head(wtc, n = 3)
#  game_id round TP year victory_condition        scenario
#1       1     1  1 2013       Caster Kill Into the Breach
#2       2     1  0 2013       Caster Kill Into the Breach
#3       3     1  1 2013       Caster Kill Into the Breach
#             player1            team1  list1
#1   Pär-Ola Nilsson Team Epic Sweden  Vlad2
#2 Robert Willemstein Team Netherlands Feora2
#3      Christian Aas Team Epic Sweden  Vayl2
#                faction1 CP1 AP1     player2            team2
#1                 Khador   0  35  Aat Niehot Team Netherlands
#2 Protectorate of Menoth   0   5 Joakim Rapp Team Epic Sweden
#3   Legion of Everblight   0  56 Tom Starren Team Netherlands
#     list2    faction2 CP2 AP2 allrounds
#1    Borka  Trollblood   2   2         1
#2  Bartolo Mercenaries   4   4         1
#3 Butcher2      Khador   1   1         1
```
Player ratings can be calculated using the **PlayerRatings** 
package.

```R
wtc2013 <- na.omit(wtc[wtc$year == 2013, ])
rating13 <- steph(
    x = wtc2013[, c("round", "player1", "player2", "TP")])
head(rating13$ratings)
#              Player   Rating Deviation Games Win Draw Loss Lag
#1  Andrzej Kasiewicz 2635.488  165.7519     5   5    0    0   0
#2        Will Pagani 2610.185  172.8573     5   5    0    0   0
#3     Moritz Riegler 2589.179  175.2350     5   5    0    0   0
#4 Keith Christianson 2579.238  172.4108     5   5    0    0   0
#5      Johan Persson 2567.632  173.0174     5   5    0    0   0
#6           Enno May 2510.991  167.9497     5   4    0    1   0
```
The **WTCTools** package extends **PlayerRatings** by providing some utilities 
for estimating warcaster/warlock strength. A matrix can be created for 
each warcaster/warlock pairing.

```R
# create blank matrix
pairLookup13 <- initializeLookup(
    data = unique(c(wtc2013$list1, wtc2013$list2)))
# populate
pairLookup13 <- updateLookup(
    data = wtc2013, 
    pairlookup = pairLookup13, 
    penalty = 10, 
    round = "round", 
    compare = "TP", 
    result = "TP")
```
The resulting matrix can be used estimate of the 'home' advantage of playing
a particular warcaster/warlock against another warcaster/warlock.

```R
ratingPairs13 <- steph(
    x = wtc2013[, c("round", "player1", "player2", "TP")], 
    gamma = getMatrixVal(
        list1 = wtc2013[, "list1"], 
        list2 = wtc2013[, "list2"], 
        x = pairLookup13))
head(ratingPairs13$ratings)
#              Player   Rating Deviation Games Win Draw Loss Lag
#1  Andrzej Kasiewicz 2643.172  166.4335     5   5    0    0   0
#2        Will Pagani 2601.361  172.7013     5   5    0    0   0
#3     Moritz Riegler 2589.462  175.2995     5   5    0    0   0
#4 Keith Christianson 2574.436  173.1644     5   5    0    0   0
#5      Johan Persson 2555.896  172.9327     5   5    0    0   0
#6        Brian White 2521.748  165.8261     5   4    0    1   0
```
This allows the relative strengths of the warcasters/warlocks 
to be objectively compared.

```R
h2 <- pairLookup13["Haley2", ]
par(mar = c(7, 4, 1, 1), las = 2)
barplot(sort(h2[h2 != 0], decreasing = TRUE), 
    col = "lightblue")
```

![Haley2 Barplot](https://github.com/CSJCampbell/WTCTools/blob/master/wtc2013_haley2.png)
