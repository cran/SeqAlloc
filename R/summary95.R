#' @export
summary95 <-
function (x) {
 
    qq = quantile(x,probs=c(0,.25,.5), na.rm=T)
    qq = c(qq,mean(x,na.rm=T))
    qq = c(qq,quantile(x,probs=c(.75,.9,.95,1), na.rm=T))
    names(qq) = c("Minimum", "25th", "Median", "Mean", "75th","90th","95th","Maximum")
    qq
}
