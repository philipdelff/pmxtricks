context("NMplotFit")
### the function is not yet ready
if(F){
    test_that("minimal",{


        fileRef <- "testReference/NMplotFit1.rds"
        
        data(pksim1,package="pmxtricks")
        NMpF1 <- pmxtricks:::NMplotFit(pksim1)

        expect_equal_to_reference(NMpF1,fileRef)

    })

    test_that("with-mean-obs-logy",{


        fileRef <- "testReference/NMplotFit2.rds"
        
        data(pksim1,package="pmxtricks")
        NMpF2 <- pmxtricks:::NMplotFit(pksim1,geom.dv="p",geom.dvmean="p",geom.predmean="",col.ntim="TIME",log.y=T,debug=F)

        expect_equal_to_reference(NMpF2,fileRef)

    })


}

