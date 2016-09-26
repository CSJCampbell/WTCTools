
if (require(visualTest)) {
    
    context("check allbarplots")
    
    test_that("tbarplot", {
        
        pl14 <- structure(c(4.80673409037624, 21.3865173349445, -5.17793802916065, 
            45.9138959155639, 14.5926699048978, -21.3865173349445, -33.4375895685107), 
        .Dim = c(1L, 7L), 
        .Dimnames = list("Butcher3", c("Caine2", "Deneghra", "Haley", "Haley2", 
            "Issyria", "Kreoss3", "Saeryn")))
        attr(x = pl14, which = "n") <- structure(c(3, 2, 2, 4, 3, 2, 3), 
            .Dim = c(1L, 7L), 
            .Dimnames = list("Butcher3", c("Caine2", "Deneghra", "Haley", "Haley2", 
                "Issyria", "Kreoss3", "Saeryn")))
        nm <- colnames(pl14)[order(pl14["Butcher3", ])]
        
        png("test-tbarplot1.png")
        tbarplot(x = pl14, list1 = "Butcher3", list2 = nm)
        dev.off()
        expect_true(object = isSimilar(
            file = "test-tbarplot1.png", 
            fingerprint = system.file(package = "WTCTools", "examples", "tbarplot1.png"))
        unlink("test-tbarplot1.png")
    })
    
    test_that("allbarplots", {
        
        pl14 <- structure(c(4.80673409037624, 21.3865173349445, -5.17793802916065, 
            45.9138959155639, 14.5926699048978, -21.3865173349445, -33.4375895685107), 
        .Dim = c(1L, 7L), 
        .Dimnames = list("Butcher3", c("Caine2", "Deneghra", "Haley", "Haley2", 
            "Issyria", "Kreoss3", "Saeryn")))
        attr(x = pl14, which = "n") <- structure(c(3, 2, 2, 4, 3, 2, 3), 
            .Dim = c(1L, 7L), 
            .Dimnames = list("Butcher3", c("Caine2", "Deneghra", "Haley", "Haley2", 
                "Issyria", "Kreoss3", "Saeryn")))
        nm <- colnames(pl14)[order(pl14["Butcher3", ])]
        
        png("test-tbarplot1.png")
        tbarplot(x = pl14, list1 = "Butcher3", list2 = nm)
        dev.off()
        expect_true(object = isSimilar(
            file = "test-tbarplot1.png", 
            fingerprint = system.file(package = "WTCTools", "examples", "tbarplot1.png"))
        unlink("test-tbarplot1.png")
    })
}
