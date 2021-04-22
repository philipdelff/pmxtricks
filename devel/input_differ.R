strsplit(NMreadSection("../../models/run3001.lst",section="INPUT"),split=" +")[[1]]
l.inp <- strsplit(NMreadSection("../../models/run3001.lst",section="INPUT"),split=" +")
inp <- do.call(c,l.inp)
inp <- inp[inp!=""]
inp
l.new <- strsplit(text.nm[["INPUT"]],split=" +")
new <- do.call(c,l.new)
dt <- data.table(inp,new)
dt[,differ:=(inp!=new)]
dt
