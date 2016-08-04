#' @export
SeqAllocplot <- 
function(mysim,bporder=NULL,stratum=F,cexsize=0.7) {

      numschemes = length(mysim$schemes)
      if (is.null(bporder) ) bporder = 1:numschemes
      if (stratum==T) {
           bporder = bporder[substr(mysim$schemes[bporder],1,1)!="P"]
      }

      deslabel  = mysim$schemes
      cguesslim = c(min(mysim$perccorr,mysim$perccorr_strat),max(mysim$perccorr,mysim$perccorr_strat))
      cguess90lim = cguesslim
      cguess90lim[1] = min(c(50,mysim$perccorr[6,]))
      MAIClim = c(0,max(mysim$MAIC))

      Rsquared = 100*mysim$Rsquared
      R2lim = c(0,max(Rsquared))
          
      # Plot CG, imbalance


          old.par <- par(no.readonly = TRUE) # current par settings, to restore at end    
          par(pty="s",ask=T)


          boxplot(mysim$perccorr[c(1,2,3,4,5,8),bporder] ,names=deslabel[bporder],range=0,ylim=cguesslim,
                  ylab="Percent Guessed Correctly (CG)",xlab="Randomization Design",cex.axis=cexsize)
   
          boxplot(mysim$perccorr_strat[c(1,2,3,4,5,8),bporder] ,names=deslabel[bporder],range=0,ylim=cguesslim,
                  ylab="Percent Guessed Correctly by Stratum (CGs)",xlab="Randomization Design",cex.axis=cexsize)
                    
          boxplot(Rsquared[c(1,2,3,4,5,8),bporder] ,names=deslabel[bporder],range=0,ylim=R2lim,
                  ylab="R Squared (percent)",xlab="Randomization Design",cex.axis=cexsize)


          boxplot(mysim$MAIC[c(1,2,3,4,5,8),bporder] ,names=deslabel[bporder],range=0,ylim=MAIClim,
                  ylab="Maximum Covariate Imbalance (MAIC)",xlab="Randomization Design",cex.axis=cexsize)
          
          

          boxplot(mysim$WAIC[c(1,2,3,4,5,8),bporder] ,names=deslabel[bporder],range=0,ylim=MAIClim,
                  ylab="Weighted Average Covariate Imbalance (WAIC)",xlab="Randomization Design",cex.axis=cexsize)
          
          boxplot(mysim$AI[c(1,2,3,4,5,8),bporder] ,names=deslabel[bporder],range=0,
                  ylab="Imbalance in Treatment Allocation (AI)",xlab="Randomization Design",cex.axis=cexsize)
          
       

          plot(mysim$AI[8,bporder],mysim$perccorr[8,bporder],type="n", 
               xlab="Maximum Imbalance in Treatment Allocation (AI)",
               ylab="Maximum Percent of Assignments Guessed Correctly (CG)",
               ylim=cguess90lim)
          text(mysim$AI[8,bporder],mysim$perccorr[8,bporder],labels=deslabel[bporder],cex=cexsize)
          box()


          plot(mysim$AI[6,bporder],mysim$perccorr[6,bporder],type="n", 
               xlab="90th Percentile of Imbalance in Treatment Allocation (AI)",
               ylab="90th Percentile of Percent of Assignments Guessed Correctly (CG)",
               ylim=cguess90lim)
          text(mysim$AI[6,bporder],mysim$perccorr[6,bporder],labels=deslabel[bporder],cex=cexsize)
          box()



          plot(mysim$AI[8,bporder],mysim$perccorr_strat[8,bporder],type="n", 
               xlab="Maximum Imbalance in Treatment Allocation (AI)",
               ylab="Maximum Percent of Assignments Guessed Correctly by Stratum (CGs)",
               ylim=cguess90lim)
          text(mysim$AI[8,bporder],mysim$perccorr_strat[8,bporder],labels=deslabel[bporder],cex=cexsize)
          box()


          plot(mysim$AI[6,bporder],mysim$perccorr_strat[6,bporder],type="n", 
               xlab="90th Percentile of Imbalance in Treatment Allocation (AI)",
               ylab="90th Percentile of Percent of Assignments Guessed Correctly by Stratum (CGs)",
               ylim=cguess90lim)
          text(mysim$AI[6,bporder],mysim$perccorr[6,bporder],labels=deslabel[bporder],cex=cexsize)
          box()


          plot(Rsquared[8,bporder],mysim$perccorr_strat[8,bporder],type="n", 
               xlab="Maximum of Rsquared (percent)",
               ylab="Maximum Percent of Assignments Guessed Correctly by Stratum (CGs)",
               xlim = R2lim, ylim=cguess90lim)
          text(Rsquared[8,bporder],mysim$perccorr_strat[8,bporder],labels=deslabel[bporder],cex=cexsize)
          box()

          plot(Rsquared[6,bporder],mysim$perccorr_strat[6,bporder],type="n", 
               xlab="90th Percentile of Rsquared (percent)",
               ylab="90th Percentile, Assignments Guessed Correctly by Stratum (CGs)",
               xlim = c(0,max(Rsquared[6,bporder])), ylim=cguess90lim)
          text(Rsquared[6,bporder],mysim$perccorr_strat[6,bporder],labels=deslabel[bporder],cex=cexsize)
          box()


          plot(mysim$WAIC[8,bporder],mysim$perccorr_strat[8,bporder],type="n", 
               xlab="Maximum of Weighted Avg Covariate Imbalance (WAIC)",
               ylab="Maximum Percent of Assignments Guessed Correctly by Stratum (CGs)",
               xlim = MAIClim, ylim=cguess90lim)
          text(mysim$WAIC[8,bporder],mysim$perccorr_strat[8,bporder],labels=deslabel[bporder],cex=cexsize)
          box()

          plot(mysim$WAIC[6,bporder],mysim$perccorr_strat[6,bporder],type="n", 
               xlab="90th Percentile of Weighted Avg Covariate Imbalance (WAIC)",
               ylab="90th Percentile, Assignments Guessed Correctly by Stratum (CGs)",
               xlim = c(0,max(mysim$WAIC[6,bporder])), ylim=cguess90lim)
          text(mysim$WAIC[6,bporder],mysim$perccorr_strat[6,bporder],labels=deslabel[bporder],cex=cexsize)
          box()



          plot(mysim$MAIC[8,bporder],mysim$perccorr_strat[8,bporder],type="n", 
               xlab="Maximum of Maximum Covariate Imbalance (MAIC)",
               ylab="Maximum Percent of Assignments Guessed Correctly by Stratum (CGs)",
               xlim = MAIClim, ylim=cguess90lim)
          text(mysim$MAIC[8,bporder],mysim$perccorr_strat[8,bporder],labels=deslabel[bporder],cex=cexsize)
          box()

          plot(mysim$WAIC[6,bporder],mysim$perccorr_strat[6,bporder],type="n", 
               xlab="90th Percentile of Maximum Covariate Imbalance (MAIC)",
               ylab="90th Percentile, Assignments Guessed Correctly by Stratum (CGs)",
               xlim = MAIClim, ylim=cguess90lim)
          text(mysim$MAIC[6,bporder],mysim$perccorr_strat[6,bporder],labels=deslabel[bporder],cex=cexsize)
          box()

         
          par(old.par)
}
