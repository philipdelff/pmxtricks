context("rbindUnion")

test_that("Stack data.frames",{

    fileRef <- "testReference/rbindUnion.rds"
    
    x <- data.frame(a=3:5)
    y <- data.frame(b=3:5,c="f")
    z <- data.frame(a=4:5,b=letters[4:5],d=c(NA,5))
    expect_equal_to_reference(rbindUnion(x,y,z),fileRef)
    
})

test_that("Stack elements of list",{

    ## notice we are expecting the exact same result as above
    fileRef <- "testReference/rbindUnion.rds"
    
    x <- data.frame(a=3:5)
    y <- data.frame(b=3:5,c="f")
    z <- data.frame(a=4:5,b=letters[4:5],d=c(NA,5))
    dfs <- list(x,y,z)
    
    expect_equal_to_reference(rbindUnion(dfs),fileRef)
    
})
