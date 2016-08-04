#' @export
bcdalloc <-
function(n,pbcd,tol = 1e-6) {

  # Gives 1 for intervention, 0 for control

   result =  rbinom(n,1,0.5)
   if (n > 1) {
   for (j in 2:n) {
      cresult = 2*sum(result[1:(j-1)]) - (j-1)
      if (cresult <= - tol) result[j] = rbinom(1,1,pbcd)
      if (cresult >= tol) result[j] = rbinom(1,1,1-pbcd)
   }}
   result
}
