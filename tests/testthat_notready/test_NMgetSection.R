NMgetSection()

## how to read the included run?
NMgetSection2("run001.mod",name="PK")
NMgetSection2("run001.lst",name="PK")
NMgetSection2("run001.lst",name="PK",keepComments=F,return="idx")
NMgetSection2("run001.lst",name="PK",keepComments=F)

## need to include a run with multiple $TABLE
NMgetSection2("run001.lst",name="TABLE")
