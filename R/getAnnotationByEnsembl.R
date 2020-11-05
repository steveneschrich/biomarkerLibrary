#' getAnnotationByEnsembl - get the annotation table of ensembl ID's for a given ExpressionSet
#'
#' @param eset The expression set that has a corresponding (affy) annotation.
#'
#' @return A list of annotations (ENSEMBL) by probeset.
#' @export
#'
#' @examples
getAnnotationBySymbol<-function(eset ) {
  activateAnnotation(eset)

  return(getAnnotationList(eset, "ENSEMBL"))
}
