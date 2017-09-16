

#' @title Convert Warcaster and Warlock names to standard format
#' @description Inserts space between name and number. 
#' In addition adds 1 if no number, and manually updates
#' some common alternative names.
#' @param x single character vector of caster names
#' @return single character vector of harmonized caster names
#' @examples 
#' harmonizeCasters(c("Vlad", "Vlad1", "Vald 1"))
#' @export
#' @importFrom stringr str_replace str_c

harmonizeCasters <- function(x) {
    casters <- c("Absylonia 1", "Absylonia 2", "Agathia 1", "Amon 1", "Arkadius 1", 
        "Ashlynn 1", "Asphyxious 1", "Asphyxious 2", "Asphyxious 3", 
        "Aurora 1", "Axis 1", "Baldur 1", "Baldur 2", "Barnabas 1", "Bethayne 1", 
        "Blaize 1", "Borka 1", "Borka 2", "Bradigus 1", "Brisbane 1", "Butcher 1", 
        "Butcher 2", "Butcher 3", "Caine 1", "Caine 2", "Caine 3", "Calaban 1", 
        "Calandra 1", "Carver 1", "Child 1", "Cyphon 1", "Damiano 1", "Darius 1", 
        "Deneghra 1", "Deneghra 2", "Deneghra 3", "Directrix 1", "Doomshaper 1", "Doomshaper 2", 
        "Doomshaper 3", "Dreamer 1", "Durgen 1", "Durst 1", "Elara 2", "Feora 1", "Feora 2", 
        "Fiona 1", "Fyanna 2", "Garryth 1", "Goreshade 1", "Goreshade 2", "Goreshade 3", 
        "Gorten 1", "Grayle 1", "Grim 1", "Grim 2", "Grissel 1", "Grissel 2", 
        "Gunnbjorn 1", "Haley 1", "Haley 2", "Haley 3", "Harbinger 1", 
        "Harkevich 1", "Helga 1", "Helleana 1 & Morgaen 1 & Selene 1", 
        "Helynna 1", "Heretic 1", "Hexeris 1", "Hexeris 2", "High Reclaimer 1", "High Reclaimer 2", 
        "Horgle 2", "Iron Mother 1", "Irusk 1", "Irusk 2", "Issyria 1", "Jaga-Jaga 1", 
        "Kaelyssa 1", "Kallus 1", "Kallus 2", "Karchev 1", "Kaya 1", "Kaya 2", "King of Nothing 1", "Kozlov 1", 
        "Kraye 1", "Kreoss 1", "Kreoss 2", "Kreoss 3", "Kromac 1", "Kromac 2", 
        "Krueger 1", "Krueger 2", "Kryssa 1", "Lucant 1", "Lylyth 1", 
        "Lylyth 2", "Lylyth 3", "MacBain 1", "Maddox 1", "Madrak 1", 
        "Madrak 2", "Madrak 3", "Maelok 1", "Magnus 1", "Magnus 2", "Makeda 1", "Makeda 2", 
        "Makeda 3", "Malekus 1", "Midas 1", "Mohsar 1", "Montador 1", 
        "Mordikaar 1", "Morghoul 1", "Morghoul 2", "Mortenebra 1", "Morvahna 1", 
        "Morvahna 2", "Naaresh 1", "Nemo 1", "Nemo 2", "Nemo 3", "Old Witch 1", 
        "Ossrum 1", "Ossyan 1", "Ragnor 1", "Rahn 1", "Rasheth 1", "Rask 1", 
        "Ravyn 1", "Reznik 1", "Reznik 2", "Rhyas 1", "Saeryn 1", "Saeryn 2 & Rhyas 2", 
        "Scaverous 1", "Severius 1", "Severius 2", "Shae 1", "Siege 1", 
        "Skarre 1", "Skarre 2", "Skuld 1", "Sloan 1", "Sorscha 1", "Sorscha 2", 
        "Strakhov 1", "Strakhov 2", "Stryker 1", "Stryker 2", "Stryker 3", "Sturgis 1", 
        "Sturm 1 & Drang 1", "Syntherion 1", "Tanith 1", "Terminus 1", 
        "Thagrosh 1", "Thagrosh 2", "Thexus 1", "Thyra 1", "Thyron 1", 
        "Una 2", "Vayl 1", "Vayl 2", "Venethrax 1", "Vindictus 1", "Vladimir 1", 
        "Vladimir 2", "Vladimir 3", "Vyros 1", "Vyros 2", "Wanderer 1", "Wurmwood 1", 
        "Xekaar 1", "Xerxis 1", "Xerxis 2", "Zaal 1", "Zaal 2", "Zaadesh 2", "Zerkova 1", 
        "Zerkova 2")

    if (!all(x %in% casters)) {
        # no number, add 1
        x[grepl(pattern = "^[A-Za-z -&]+$", x = x)] <- paste0(
            grep(pattern = "^[A-Za-z -&]+$", x = x, value = TRUE), 
                " 1")
        # no space before number
        ind <- grepl(pattern = "[A-Za-z]+[1-4]$", x = x)
        x[ind] <- str_c(
            str_replace(
                string = x[ind], pattern = "[1-4]$", replacement = ""), 
            " ",
            str_replace(
                string = x[ind], pattern = "^[A-Za-z -]+", replacement = ""))
        # Vlad
        ind <- grepl(pattern = "^[Vv][Ll][Aa][Dd] [1-4]$", x = x)
        if (any(ind)) {
            x[ind] <- str_replace(string = x[ind], pattern = "^[Vv][Ll][Aa][Dd] ", 
                replacement = "Vladimir ")
        }
        # Reclaimer
        ind <- grepl(pattern = "^([Hh][Ii][Gh][Hh] ){0,1}Reclaimer [1-4]$", x = x)
        if (any(ind)) {
            x[ind] <- str_replace(string = x[ind], pattern = "^([Hh][Ii][Gh][Hh] ){0,1}Reclaimer ", 
                replacement = "High Reclaimer ")
        }
        # Wurmwood
        x <- str_replace(pattern = "^Cassius 1$", string = x, replacement = "Wurmwood 1")
        # Coven
        x <- str_replace(pattern = "^(Witch ){0,1}[Cc]oven 1$", string = x, 
            replacement = "Helleana 1 & Morgaen 1 & Selene 1")
        # Fiona
        x <- str_replace(pattern = "^Fiona the Black 1$", string = x, 
            replacement = "Fiona 1")
        # Bart
        x <- str_replace(pattern = "^Bartolo 1$", string = x, replacement = "Montador 1")
        # Jarl
        x <- str_replace(pattern = "^Jarl 1$", string = x, replacement = "Skuld 1")
        # Saeryn
        x <- str_replace(pattern = "^(Saeryn 2|Saeryn & Rhyas.*)$", string = x, replacement = "Saeryn 2 & Rhyas 2")
        # Amon
        x <- str_replace(pattern = "^Ad[- ]Raza 1$", string = x, replacement = "Amon 1")
        # Damiano
        x <- str_replace(pattern = "^Captain Damiano 1$", string = x, replacement = "Damiano 1")
        x <- str_replace(pattern = "^Cognifex Cyphon 1$", string = x, replacement = "Cyphon 1")
        x <- str_replace(pattern = "^Drake M(a){0,1}cBain 1$", string = x, replacement = "MacBain 1")
        x <- str_replace(pattern = "^Exulon Thexus 1$", string = x, replacement = "Thexus 1")
        x <- str_replace(pattern = "^Ragnar 1$", string = x, replacement = "Ragnor 1")
        
        isStillMissed <- !x %in% casters
        if (any(isStillMissed)) {
            warning(paste(sum(isStillMissed), "records not harmonised;", 
                paste(unique(x[isStillMissed]), collapse = ", ")))
        }
    }
    x
}
