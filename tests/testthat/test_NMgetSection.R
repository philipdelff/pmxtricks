context("NMgetSection")

test_that("basic",{

    fileRef <- "testReference/NMgetSection.rds"

    ## file.lst <- "../../inst/examples/nonmem/run001.lst"
    file.lst <- pmxtricks_filepath("examples/nonmem/run001.lst")
    ## NMgetSection(pmxtricks_filepath("examples/nonmem/run001.lst"),section="DATA")

    res1 <- NMgetSection(file=file.lst,section="DATA")

    expect_equal_to_reference(res1,fileRef)
})
