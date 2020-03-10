context("NMscanData")

test_that("basic",{

    fileRef <- "testReference/NMscanData.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- pmxtricks_filepath("examples/nonmem/run001.lst")
    ## NMgetSection(pmxtricks_filepath("examples/nonmem/run001.lst"),section="DATA")

    ## res1 <- NMscanData(file=file.lst,debug=F)
    ## expect_equal_to_reference(res1,fileRef)
})
