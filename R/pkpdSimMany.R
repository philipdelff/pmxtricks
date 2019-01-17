##' Simulate multiple subjects with PKPDsim

##' @param doses amt is in mg.

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
                          ,t_inf=pars[row.pars,"D1"]
                          ,type="infusion"
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






