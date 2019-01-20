###{ Section start: Initialisation
library(PKPDsim)
library(ggplot2)
###} Section end



###{ Section start: Simulate covariates
nSubjs <- 82
df.cov <- data.frame(ID=sprintf("%03d",1:nSubjs))


###} Section end



###{ Section start: simulate PK model parameters
## model:
## dA1dt = -ka1*A1
## dA2dt = ka1*A1-kt1*A2 
## dA3dt = kt1 * A2 - CL*A3/VC - A2/VC*VM/(KM+A2/V2)
## dA4dt = Q*(A3/VC-A4/VP)




tvpars=data.frame(KA1=1,KT1=3,CL=.25,KM=5,VM=5,VC=1,VP=10,Q=0.15)

diag.omega <- rep(.2,ncol(tvpars))
diag.omega[1:3] <- c(.3,.2,1)
omega <- diag(diag.omega)
rownames(omega) <- colnames(tvpars)
colnames(omega) <- colnames(tvpars)
omega
omega[2,1] <- omega[1,2] <- .3/2   ## ka kt
omega[3,6] <- omega[6,3] <- .1     ## CL VC
omega[3,8] <- omega[8,3] <- .1     ## CL Q
omega[6,7] <- omega[7,6] <- .3/2   ## VP VC
omega

library(MASS)
set.seed(9854934)
ETAS <- mvrnorm(n=nSubjs,mu=rep(0,ncol(tvpars)),Sigma=omega)

pars <- exp(ETAS)*tvpars[rep(1,nSubjs),]
## pairs(pars)

pars$ID <- df.cov$ID
pars$F1 <- 1

###} Section end


###{ Section start: simulate doses
## daily dosing for 14 days
NTIM.doses <- seq(0,by=24,length.out=7*5)
df.doses <- merge(data.frame(ID=df.cov$ID),data.frame(NTIM=NTIM.doses))
## df.doses

## use gaussian noise on NTIM
df.doses$noise.time.dose <- rnorm(n=nrow(df.doses),sd=2)
df.doses$TIME <- df.doses$NTIM + df.doses$noise.time.dose
df.doses <- as.data.frame(do.call(rbind,by(df.doses,df.doses$ID,function(x)
    data.frame(ID=unique(x$ID)
              ,NTIM=x$NTIM
              ,TIME=x$TIME-min(x$TIME))
    )))

df.doses

## miss doses at random


###} Section end

###{ Section start: simulate observation times

###} Section end




###{ Section start: Simulate with PKPDsim

## dA1dt = -ka1*A1
## dA2dt = ka1*A1-kt1*A2 
## dA3dt = kt1 * A2 - CL*A3/VC - A2/VC*VM/(KM+A2/V2)
## dA4dt = Q*(A3/VC-A4/VP)
pk1 <- PKPDsim::new_ode_model(code ="
dAdt[1] = -KA1*A[1]
dAdt[2] = KA1*A[1]-KT1*A[2] 
dAdt[3] = KT1 * A[2] - CL*A[3]/VC - A[2]/VC*VM/(KM+A[2]/VC)
dAdt[4] = Q*(A[3]/VC-A[4]/VP)
"
,dose = list(cmt = 1, bioav = "F1") ### param goes here
,obs = list(cmt = 3, scale = "VC")
,parameters = c("F1","VC","CL","KA1","KT1","VP","Q","VM","KM")
)

pkpdSimMany <- function(model,doses,pars,simtimes,col.id,debug=F,...){
    if(debug) browser()

#### check arguments

    if(nrow(pars)>1) {
        if(missing(col.id)) stop("pars is a data.frame - col.id must be supplied")
        stopifnot(col.id%in%names(pars))
        if(any(duplicated(pars[,col.id]))) stop("Duplicated ID's not allowed.")
    }
    
   
    simres <- do.call(rbind,lapply(1:nrow(pars),function(row.pars){
        
        doses <- transform(doses[,c("times","amt")]
                          ,cmt=1
                          ,amt = amt  # from mg to microgram
                          ,type="bolus"
                          ,stringsAsFactors=F
                           )
        regimen <- do.call(PKPDsim::new_regimen,as.list(doses))

        s.row <- PKPDsim::sim(
                              ode = model
                              ,parameters = pars[row.pars,,drop=F]
                              ,regimen = regimen
                              ,t_obs = simtimes
                              ,...
                             ## ,int_step=.001
                          )
        if(nrow(pars)>1){
            s.row[,col.id] <- pars[row.pars,col.id]
        }
        if(any(!is.finite(s.row[,"y"]))) warning(paste("Numerical issue for subject: ",col.id,"=",unique(s.row[,col.id]),". Try reducing t_sim?"))
        s.row
    }))

    simres
    
}

df.doses$amt <- 200
df.doses$times <-  df.doses$TIME
sim1 <- pkpdSimMany(pk1,df.doses,pars=pars,simtimes=seq(0,by=4,to=8*7*24),col.id="ID")
head(sim1)

p1 <- ggplot(sim1,aes(t/24/7,y,group=ID))+geom_line()+facet_wrap(~comp,scales="free")
p1
p1+scale_y_log10()

## subset samples from simpulation times.

## observation noise
## miss samples at random

###} Section end


###{ Section start: merge and export data for Nonmem


###} Section end

