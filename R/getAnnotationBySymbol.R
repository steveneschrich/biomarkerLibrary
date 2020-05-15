#' getAnnotationBySymbol - get the annotation table of gene symbols for a given ExpressionSet
#'
#' @param eset The expression set that has a corresponding (affy) annotation.
#'
#' @return A list of annotations (SYMBOL) by probeset.
#' @export
#'
#' @examples
getAnnotationBySymbol<-function(eset) {
  activateAnnotation(eset)

  return(saeAffy::getAnnotationList(eset, "SYMBOL"))
}
