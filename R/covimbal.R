#' @export
covimbal <-
function(y,xmatc) {
   # evaluate covariate imbalance by seeing how well we can predict treatment using linear regression with covariates
   summary(lm(2*y -1 ~ cbind(rep(1,nrow(xmatc)),xmatc) - 1))$r.squared
}
