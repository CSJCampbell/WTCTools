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
install_github("CSJCampbell/WTCTools")
```

This package also includes tests in **testthat** format. 
From R run the call `test_package("WTCTools")`.
This package contains the 2014 dataset where player 1 was the winner of each game.
```R
library(WTCTools)
head(wtc2014)
rating <- steph(x = wtc2014[, c("round", "player1", "player2", "TP")])
head(rating$ratings)
#              Player   Rating Deviation Games Win Draw Loss Lag
# 1     Jake VanMeter 2676.402  157.0390     6   6    0    0   0
# 2       Brian White 2667.702  159.1511     6   6    0    0   0
# 3        Ben Leeper 2592.218  158.1336     6   6    0    0   0
# 4 Anthony Ferraiolo 2590.775  155.9631     6   5    0    1   0
# 5        Colin Hill 2587.563  165.7409     6   6    0    0   0
# 6       Will Pagani 2563.477  161.2261     6   5    0    1   0
```


```R
wtc2014 <- na.omit(wtc2014)
pairLookup <- initializeLookup(data = wtc2014)
```
