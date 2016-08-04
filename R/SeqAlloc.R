#' @importFrom graphics box boxplot par plot text
#' @importFrom stats lm quantile rbinom

#' @export
SeqAlloc <-
function(xmat,carwt,strata=NULL,blksize,pbcd,pcar,bsdtol,caittol,niter,seed = 12345) {


# Error check for parameters

   if (pbcd > 1 | pbcd < 0) return("pbcd out of range [0,1]")
   if (pcar > 1 | pcar < 0) return("pcar out of range [0,1]")
   if (bsdtol < 0) return("bcdtol is negative")
   if (caittol < 0) return("caittol is negative")
   if (any(carwt < 0)) return("carwt component is negative")
   if (sum(carwt ) <=0 ) return("carwt must have positive sum")
   if (any(blksize %% 2 != 0) ) return("block size must be multiple of 2")

   carwt = carwt/sum(carwt)

   set.seed(seed)

   num_strat_schemes = 2
   sampsize = nrow(xmat)

   if (is.null(strata) ) strata = rep(1,sampsize)
   if ( length(unique(strata)) == 1) num_strat_schemes = 1

   xmatc = scale(xmat,center=TRUE,scale=FALSE) 
                                                                  
   # Create scheme names 
   P_SPBD = c("PBD","SPBD")[1:num_strat_schemes]   
   schemes=c("CR","RAR","BCD","CAR","BSD","CAIT")
   for (j in 1:num_strat_schemes) {
      schemes = c(schemes, paste(P_SPBD[j],blksize,sep=''))
   }
   loc_PBD = 7:(7+length(blksize)-1)
   loc_SPBD = NULL
   if (num_strat_schemes == 2) loc_SPBD = (loc_PBD[length(loc_PBD)]+1):(loc_PBD[length(loc_PBD)]+length(blksize))
   
   numschemes = length(schemes) 
   
   alloc=data.frame(matrix(0,nrow=sampsize,ncol=numschemes))
   
   colnames(alloc)  = schemes
   corrguess = alloc
   corrguess_strat = alloc

   strata_vals = unique(sort(strata))

   # Find weighted match variables for rows of xmat

   carwt_x = apply(xmat,1,weight_x,xmat,carwt)

   AI = matrix(0,nrow=niter,ncol=numschemes,dimnames = list(1:niter,schemes))
   Rsquared = AI
   percentcorrect = AI
   percentcorr_strat = AI
   MAIC = AI
   WAIC = AI


   for (i in 1:niter) {

 # Permute the order of subject entry into the study

      permute = sample(1:sampsize,sampsize,replace=F)

      xmat1 = xmat[permute,]
      xmatc1 = xmatc[permute,]
      strata1 = strata[permute]
      carwt_x1 = carwt_x[permute,permute]

      alloc[,"CR"] = rbinom(sampsize,1,0.5)
      alloc[,"RAR"] = sample(c(rep(0,sampsize/2),rep(1,sampsize/2)),sampsize,replace=F)

      for(m in 1:length(blksize)){
            alloc[,loc_PBD[m]] = stratblockalloc(sampsize,blksize[m])
      }

      for (k in strata_vals) {
         ix = strata1 == k
         if (sum(ix) > 0) {

           for(m in 1:length(blksize)){
               alloc[ix,loc_SPBD[m]] = stratblockalloc(sum(ix),blksize[m])
           }
           alloc[ix,"BCD"] = bcdalloc(sum(ix),pbcd,tol=1e-6)

           alloc[ix,"BSD"] = bcdalloc(sum(ix),1,tol=bsdtol)

      }} 

      alloc[,"CAR"] = caralloc(xmat1,carwt,pcar,tol=1e-6)
      alloc[,"CAIT"] = caitalloc(carwt_x1,1,tol=caittol)    

      corrguess = apply(alloc,2,cguess)
      for (k in strata_vals) {
          ix = strata1==k
          if (sum(ix) > 0) corrguess_strat[ix,] = apply(alloc[ix,],2,cguess)
      } # end for (k)

      AI[i,] = abs(2*apply(alloc,2,mean)-1 )

      # Get covariate imbalance
      MAIC[i,] = apply(alloc,2,MAICimbal,xmat1)
      WAIC[i,] = apply(alloc,2,WAICimbal,xmat1,carwt)
      Rsquared[i,] = apply(alloc,2,covimbal,xmatc1)

      percentcorrect[i,] = apply(corrguess,2,mean)
      percentcorr_strat[i,] = apply(corrguess_strat,2,mean)
       
   }   # end of iteration 
   
    # output the basic specifications
    cat ("#########################################################","\n")
    cat ("Specifications in Simulation Study","\n")
    cat("\n")
    
    cat ("Number of rows in the input data matrix:",dim(xmat)[1],"\n")
    cat ("Candidate covariates in the input data matrix:",colnames(xmat),"\n")
    cat ("Number of iterations for simulation:", niter,"\n")
    cat ("\n")
    
    cat ("Block Size for PBD(s) and SPBD(s):", blksize,"\n")
    cat ("Probability for BCD:",pbcd,"\n")
    cat ("Tolerance (d value) for BSD:", bsdtol,"\n")  
    cat ("\n")
    
    cat ("Covariates specified in CAR and CAIT:",names(xmat),"\n")
    cat ("Weights used in CAR and CAIT:","\n")
    print(carwt)
    cat ("Probability for CAR:",pcar,"\n")
    cat ("Tolerance (d value) for CAIT:", caittol,"\n")
    cat ("#########################################################","\n")
    
 
    # Calculate summary statistics

      AI_p = apply(AI,2,summary95)
   #   imbalcov_p = apply(imbalcov,2,summary95)
      Rsquared_p = apply(Rsquared,2,summary95)
      MAIC_p = apply(MAIC,2,summary95)
      WAIC_p = apply(WAIC,2,summary95)
      perccorr_p = 100*apply(percentcorrect,2,summary95)
      perccorr_strat_p = 100*apply(percentcorr_strat,2,summary95)
    

    
    # return output
   list(  schemes=schemes,
          AI = AI_p,
  Rsquared = Rsquared_p,
  MAIC = MAIC_p,
  WAIC = WAIC_p,
  perccorr = perccorr_p,
  perccorr_strat = perccorr_strat_p,
          carwt = carwt)
    
}
