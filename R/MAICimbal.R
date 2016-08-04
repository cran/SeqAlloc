#' @export
MAICimbal <-
function(alloc,xmat) {
   # evaluate covariate imbalance for each factor level of each covariate
   # returns vector of AIC(j) for each column of xmat

   cov_diff = apply(xmat,2,cov_diff_fcn,alloc)
   max(cov_diff)/2
}

cov_diff_fcn <- function(xvec,alloc) {  
   sum (tapply(alloc,xvec, function(y) {abs(mean(2*y - 1))} ) )
}
