
#' Reduce an ExpressionSet by a given list of ids. Note the ids must match
#' the featureNames of the ExpressionSet. The result is an ExpressionSet
#' limited to only those features in "id" list.
#'
#' @param eset An ExpressionSet to subset
#' @param ids A vector of featureNames to subset the ExpressionSet on.
#'
#' @return A new ExpressionSet reduced by the features in 'ids'.
#' @export
#'
#' @examples
featureSubset<-function(eset, ids, debug=TRUE) {
  stopifnot(class(eset)=="ExpressionSet" && any(ids %in% featureNames(eset)))

  # Not all may be in the ExpressionSet
  ids_reduced<-ids[ids %in% featureNames(eset)]
  eset_reduced<-eset[ids_reduced,]

  message(sprintf("featureSubset: %d ids not in the eset.\nfeatureSubset: %d features removed, %d features remaining\n",
                  length(ids)-length(ids_reduced),
                  dim(eset)[1]-dim(eset_reduced)[1],
                  dim(eset_reduced)[1]
                  )
  )
  return(eset_reduced)

}
