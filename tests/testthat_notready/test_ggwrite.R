load_all("c:/Users/kctw748/working_copies/pmxtricks")

df <- data.frame(a=1,b=2)
p1 <- ggplot(df,aes(a,b))+geom_point()

ggwrite(p1,show=T)
ggwrite(p1,show=F)

ggwrite(p1,file="c:/Users/kctw748/tmp/tmp.png",show=F)
ggwrite(p1,file="c:/Users/kctw748/tmp/tmp.png",show=T)

