#' getAnnotationByEntrezID - get the annotation table of EntrezID's for a given ExpressionSet
#'
#' @param eset The expression set that has a corresponding (affy) annotation.
#'
#' @return A list of annotations (ENTREZID) by probeset.
#' @export
#'
#' @examples
getAnnotationByEntrezID<-function(eset) {
  activateAnnotation(eset)

  return(getAnnotationList(eset, "ENTREZID"))
}
