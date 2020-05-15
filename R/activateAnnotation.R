#' activateAnnotation - Get the right annotation (possibly installing it) for an expressionset. It will load the package.
#'
#' @param eset The expression set that has a corresponding (affy) annotation.
#'
#' @return The name of the expression set.
#' @export
#'
#' @examples

activateAnnotation<-function(eset) {
  annotationName<-annotation(eset)
  annotationPackage<-paste0(annotationName,".db")
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  if (!requireNamespace(annotationPackage, quietly=TRUE))
    BiocManager::install(annotationPackage)

  library(annotationPackage, character.only = TRUE)

  return(annotationName)
}
