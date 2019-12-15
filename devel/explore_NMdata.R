library(devtools)


res1 <- NMscanData("c:/Users/kctw748/OneDrive/azd5363/poppk_20190127_Ph1Ph2Pooled_explore/Models/BaseModel/run7161.lst")

names(res1)
attributes(res1)
str(res1)


res2 <- NMscanData("c:/Users/kctw748/OneDrive/azd5363/poppk_20190127_Ph1Ph2Pooled_explore/Models/BaseModel/run7110.lst")


load_all("c:/Users/kctw748/working_copies/pmxtricks")
resall <- rbind(res1,res2)
resall$pop


