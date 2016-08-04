#' @export
caitalloc <-
function(carwt_x1,p,tol) {
  
   n = nrow(carwt_x1)  
   result = rbinom(n,1,0.5)
   if (n > 1) {
     for (j in 2:n) {
       
      prevobs = 2*result[1:(j-1)] - 1   # gives the previous observations values of  -1, 1
      cresult = crossprod(prevobs,carwt_x1[j,1:(j-1)])
      if (cresult  <= -tol) result[j] = rbinom(1,1,p) 
      if (cresult  >= tol) result[j] = rbinom(1,1,1-p)
      }
   }
   result
}


