context("mergeCheck")

test_that("basic",{

    fileRef <- "testReference/mergeCheck1.rds"

    df1 <- data.frame(x = 1:10,
                      y=letters[1:10])
    df2 <- data.frame(y=letters[1:11],
                      x2 = 1:11)

    mc1 <- mergeCheck(df1,df2)
    
    expect_equal_to_reference(mc1,fileRef)
})


test_that("increasing number of rows",{

    fileRef <- "testReference/mergeCheck2.rds"

    df1 <- data.frame(x = 1:10,
                      y=letters[1:10])
    df2 <- data.frame(y=c("a",letters[1:11]),
                      x2 = c(44,1:11))

    expect_error( mergeCheck(df1,df2))
    
})

test_that("a dt and a df",{
    library(data.table)
    
    fileRef <- "testReference/mergeCheck3.rds"

    dt1 <- data.table(x = 1:10,
                      y=letters[1:10])
    df2 <- data.frame(y=letters[1:11],
                      x2 = 1:11)

    ### notice, this is a left join
    res3 <- merge(dt1,df2)
    
    expect_equal_to_reference(res3,fileRef)
        
})
