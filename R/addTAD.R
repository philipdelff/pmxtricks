
#### add NDOSES, TAD

## NDOSPERIOD
dpk2[PART==1&EVID==0,NDOSPERIOD:=findInterval(NOMTIME,c(0),left.open=TRUE,rightmost.closed = FALSE)]
dpk2[PART==1&EVID==1,NDOSPERIOD:=findInterval(NOMTIME,c(0),left.open=FALSE,rightmost.closed = FALSE)]

seq.dos.1 <- 0
seq.dos.2 <- seq(0,by=24,length.out=14)
dpk2[PART==2&EVID==0,NDOSPERIOD:=findInterval(NOMTIME,seq.dos.2,left.open=TRUE,rightmost.closed = FALSE)]
dpk2[PART==2&EVID==1,NDOSPERIOD:=findInterval(NOMTIME,seq.dos.2,left.open=FALSE,rightmost.closed = FALSE)]



### HERE: NTAD
## NTPD, NTAD
dpk2[,REC:=.I]
min.of.pos <- function(x) min(x[x>=0])
dpk2[PART==2,NTAD:=min.of.pos(NOMTIME-seq.dos.1),by=.(REC)]
dpk2[PART==2,NTAD:=min.of.pos(NOMTIME-seq.dos.2),by=.(REC)]
dpk2[SAD==1,NTPD:=seq.dos.1[NDOSPERIOD],by=.(REC)]
dpk2[MAD==1,NTPD:=seq.dos.2[NDOSPERIOD],by=.(REC)]

