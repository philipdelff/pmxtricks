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




tvpars=data.frame(ka1=1,kt1=3,CL=.2,KM=10,VM=10,VC=1,VP=10,Q=0.1)

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
pairs(pars)

pars$ID <- df.cov$ID
pars$F1 <- 1

###} Section end


###{ Section start: simulate doses
## daily dosing for 14 days
NTIM.doses <- seq(0,by=24,length.out=14)
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




###{ Section start: Simulate with PKPDsim
library(PKPDsim)


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
,obs = list(cmt = 2, scale = "VC")
,parameters = c("F1","VC","CL","KA1","KT1","VP","Q","VM","KM")
)



## observation noise
## miss samples at random

###} Section end


###{ Section start: merge and export data for Nonmem


###} Section end

