#' getAnnotation - get the annotation table for a given ExpressionSet
#'
#' @param eset The expression set that has a corresponding (affy) annotation.
#'
#' @return The name of the annotation map. Store this in saeAffyAnnotation variable to get automatic access for subsequent calls.
#' @export
#'
#' @examples
getAnnotation<-function(eset) {
  annotationName<-activateAnnotation(eset)

  return(annotationName)
}
