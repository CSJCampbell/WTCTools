
context("check update lookup matrix")

test_that("updateLookup", {
    
    dat <- data.frame(round = rep(1:6, each = 2),
        player1 = rep(c("A", "B"), times = 2), 
        player2 = rep(c("B", "A"), times = 2),
        result = c(0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0), 
        list1 = c("a", "b", "b", "a"), 
        list2 = c("b", "a", "a", "b"), 
        scorefrac = c(0.1, 0.8, 0.7, 0.8, 0.3, 0.9, 0.1, 0.2, 0, 0.9, 0.8, 0.1), 
        stringsAsFactors = FALSE)
    
    out <- updateLookup(data = dat, penalty = 30, inflate = 2)
    
    expect_equal(object = out, 
        expected = structure(
            c(0, 229.512748310598, -229.512748310598, 0), 
            .Dim = c(2L, 2L), 
            .Dimnames = list(c("a", "b"), c("a", "b")), 
            n = structure(c(0, 12, 12, 0), 
                .Dim = c(2L, 2L), 
                .Dimnames = list(c("a", "b"), c("a", "b")))))
    
    dat <- data.frame(round = rep(1:3, each = 2), 
        player1 = c("Alf", "Denny", "Denny", "Brad", "Alf", "Cass"),
        player2 = c("Cass", "Brad", "Cass", "Alf", "Denny", "Brad"),
        result = c(0, 1, 1, 0, 1, 0), 
        list1 = c("white", "black", "black", "white", "black", "black"),
        list2 = c("black", "white", "white", "white", "white", "white"),
        scorefrac = c(1, 1, 0, 0, 1, 1),
        stringsAsFactors = FALSE)
    
    out1 <- updateLookup(data = dat, penalty = 10)
    
    expect_equal(object = out1, 
        expected = structure(
            c(0, 35.6559127808214, -35.6559127808214, 0), 
            .Dim = c(2L, 2L), 
            .Dimnames = list(c("black", "white"), c("black", "white")), 
            n = structure(
                c(0, 5, 5, 0), 
                .Dim = c(2L, 2L), 
                .Dimnames = list(c("black", "white"), c("black", "white")))))
    
    dat$result <- c(0, 0, 1, 0, 1, 1)
    out2 <- updateLookup(data = dat, pairlookup = out1, penalty = 10)
    
    expect_equal(object = out2, 
        expected = structure(
            c(0, 64.9850515847168, -64.9850515847168, 0), 
            .Dim = c(2L, 2L), 
            .Dimnames = list(c("black", "white"), c("black", "white")), 
            n = structure(
                c(0, 10, 10, 0), 
                .Dim = c(2L, 2L), 
                .Dimnames = list(c("black", "white"), c("black", "white")))))
    
    out3 <- updateLookup(data = dat, pairlookup = out1, penalty = 5)
    
    expect_equal(object = out3, 
        expected = structure(
            c(0, 50.2822861041733, -50.2822861041733, 0), 
            .Dim = c(2L, 2L), 
            .Dimnames = list(c("black", "white"), c("black", "white")), 
            n = structure(
                c(0, 10, 10, 0), 
                .Dim = c(2L, 2L), 
                .Dimnames = list(c("black", "white"), c("black", "white")))))
})

test_that("getPairs", {
    
    dat1 <- data.frame(list1 = c("a", "b", "b"), list2 = c("b", "b", "a"))
    expect_equal(object = getPairs(data = dat1), 
        expected = matrix(data = c("a", "b"), ncol = 2))
    
    dat2 <- data.frame(list1 = c("A", "A", "B"), list2 = c("C", "B", "D"))
    expect_equal(object = getPairs(data = dat2), 
        expected = matrix(data = c("A", "B", "A", "C", "B", "D"), 
            ncol = 2, byrow = TRUE))

    mat1 <- matrix(c("a", "a", "b", "a", "b", "a"), ncol = 2)
    expect_equal(object = getPairs(data = mat1), 
        expected = matrix(c("a", "b"), ncol = 2))

    colnames(mat1) <- c("XB", "XA")
    mat1 <- rbind(mat1, c("c", "d"))
    expect_equal(object = getPairs(data = mat1), 
        expected = matrix(c("a", "b", "c", "d"), ncol = 2, byrow = TRUE))    
})
