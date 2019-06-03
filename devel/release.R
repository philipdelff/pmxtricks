library(devtools)

### we use develR
load_all("c:/Users/kctw748/working_copies/develR")

releaseR(name.pkg="pmxtricks",incV=F)

getOption("develR.dir.devel")
getOption("develR.dir.releases")
getOption("develR.dir.lib")
?releaseR
