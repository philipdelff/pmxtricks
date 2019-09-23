### overview of functions by family
dir.r <- file.path("c:/Users/kctw748/working_copies/pmxtricks/R")
scripts <- list.files(dir.r)

fams <- lapply(scripts,function(s){

    code <- readLines(file.path(dir.r,s))
    code[grep("@family",code)]

})

names(fams) <- scripts

fams
