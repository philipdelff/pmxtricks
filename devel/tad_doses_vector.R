    ######## this is if dosing times are in a separate vector, i.e. not in data
    ##     data[PART==1&EVID==0,NDOSPERIOD:=findInterval(NOMTIME,c(0),left.open=TRUE,rightmost.closed = FALSE)]
    ##     data[PART==1&EVID==1,NDOSPERIOD:=findInterval(NOMTIME,c(0),left.open=FALSE,rightmost.closed = FALSE)]

    ##     seq.dos.1 <- 0
    ##     seq.dos.2 <- seq(0,by=24,length.out=14)
    ##     data[PART==2&EVID==0,NDOSPERIOD:=findInterval(NOMTIME,seq.dos.2,left.open=TRUE,rightmost.closed = FALSE)]
    ##     data[PART==2&EVID==1,NDOSPERIOD:=findInterval(NOMTIME,seq.dos.2,left.open=FALSE,rightmost.closed = FALSE)]



    ## ### HERE: NTAD
    ##     ## NTPD, NTAD
    ##     data[,REC:=.I]
    ##     min.of.pos <- function(x) min(x[x>=0])
    ##     data[PART==2,NTAD:=min.of.pos(NOMTIME-seq.dos.1),by=.(REC)]
    ##     data[PART==2,NTAD:=min.of.pos(NOMTIME-seq.dos.2),by=.(REC)]
    ##     data[SAD==1,NTPD:=seq.dos.1[NDOSPERIOD],by=.(REC)]
    ##     data[MAD==1,NTPD:=seq.dos.2[NDOSPERIOD],by=.(REC)]
