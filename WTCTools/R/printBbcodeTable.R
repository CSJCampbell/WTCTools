
# printBbcodeTable(dat = data.frame(A = 1:3, B = letters[1:3]))

printBbcodeTable <- function(dat) {
    tab <- c("[TABLE]\n", 
        paste0(
            " [TR]\n  [TH]", 
            paste(colnames(dat), collapse = "[/TH] [TH]"), 
            " [/TH]\n[/TR]\n"),
        apply(X = dat, MARGIN = 1, FUN = function(x) {
            paste(
                " [TR]\n  [TD]", 
                paste(x, collapse = "[/TD] [TD]"), 
                "[/TD]\n [/TR]\n")
    }), "[/TABLE]\n")
    
    cat(paste0(tab, collapse = ""))
    invisible(tab)
}
