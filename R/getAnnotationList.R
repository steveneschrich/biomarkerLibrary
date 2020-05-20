#' getAnnotationList - get the annotation list for a given type
#'
#' @param eset The expression set to get annotation for.
#' @param mapping The specific field to get (e.g. SYMBOL, ENTREZID) as a string.
#'
#' @return A list of annotations by probeset.
#' @export
#'
#' @examples
getAnnotationList<-function(eset, mapping) {
  stopifnot(class(eset)=="ExpressionSet")
  annotationName<-activateAnnotation(eset)

  return(unlist(as.list(get(paste0(annotationName,mapping))))[rownames(eset)])
}
