#' Filter out genes (features) with no samples above noise (low expression).
#'
#' @param eset - the ExpressionSet to use
#' @param threshold - a minimum acceptable threshold of gene expression (default 5)
#'
#' @return A new ExpressionSet reduced to genes passing the criteria.
#' @export
#'
#' @examples
filterLowExpressionAbsolute<-function(eset, threshold=5) {
  numabove<-apply(exprs(x),1,function(y){length(which(y>threshold))})
  return(x[which(numabove>0),])
}
