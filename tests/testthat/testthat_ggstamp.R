context("ggstamp")

test_that("general use",{

    fileRef <- "testReference/ggstamp1.rds"

    data(pksim1,package="pmxtricks")
    p1 <- ggIndProfs(pksim1,amt=NULL)[[1]]
    stamp <- "testthat_ggstamp.R"
    p1 <- ggstamp(p1,stamp)

    expect_equal_to_reference(p1,fileRef)
    
})

