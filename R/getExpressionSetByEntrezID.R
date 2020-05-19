#' This function takes an expression set (assumed to be Affymetrix), and reduces the features (probesets) to only one
#' entry per gene id (EntrezID). The featurenames of the returned expression set are the EntrezID's.
#' Currently, this is only implemented for taking the highest median expression if there
#' are duplicates.
#'
#' TODO: Add more ways of summarizing.
#'
#' @param eset - An expression set to reduce to EntrezID reporters.
#'
#' @return A modified expression set (based on eset) with only features that are EntrezIDs.
#' @export
#'
#' @examples
getExpressionSetByEntrezID<-function(eset) {
  require(Biobase)

  stopifnot(class(eset)=="ExpressionSet")

  # Get feature medians
  medians<-Biobase::rowMedians(exprs(eset), na.rm=TRUE)
  names(medians)<-featureNames(eset)

  gene_ids<-getAnnotationByEntrezID(eset)

  # Calculate the gene symbol to probeset mapping (one to one).
  res<-sapply(unique(na.omit(gene_ids)), function(g) {
      potential.probesets<-names(gene_ids)[which(gene_ids==g)]
      chosen.probeset<-names(which.max(medians[potential.probesets]))
  })

  # Reduce the ExpressionSet to only the unique mappings, with gene symbols instead.
  new_eset<-eset[res,]
  featureNames(new_eset)<-names(res)

  return(new_eset)
}
