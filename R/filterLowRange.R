#' Filter genes with range less than threshold.
#'
#' @param eset - ExpressionSet to use for filtering.
#' @param threshold - Range to use as minimum (default 1)
#'
#' @return A new ExpressionSet with low range genes removed.
#' @export
#'
#' @examples
filterLowRange<-function(x, threshold=1) {
  ranges<-apply(exprs(x),1,function(y){max(y)-min(y)})
  return(x[which(ranges>threshold),])
}
