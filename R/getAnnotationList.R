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

  probeset_list<-data.frame(gene=rep(NA, nrow(eset)), row.names=featureNames(eset))
  gene_mapping<-unlist(as.list(get(paste0(annotationName,mapping))))
  probeset_list[names(gene_mapping),"gene"]<-gene_mapping

  annotation_list <- setNames(as.character(probeset_list$gene), rownames(probeset_list))

  annotation_list
}
