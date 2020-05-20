#' This function takes an expression set (assumed to be Affymetrix), and reduces the features (probesets) to only one
#' entry per gene symbol. The featurenames of the returned expression set are the gene symbols.
#' Currently, this is only implemented for taking the highest median expression if there
#' are duplicates.
#'
#' TODO: Add more ways of summarizing.
#'
#' @param eset - An expression set to reduce to gene symbol reporters.
#' @param gene_symbols - A list of gene symbols (in order of reporters).
#' @return A modified expression set (based on eset) with only features that are genes.
#' @export
#'
#' @examples
getExpressionSetByGeneSymbol<-function(eset, gene_symbols ) {
  require(Biobase)

  stopifnot(class(eset)=="ExpressionSet")

  # Get feature medians
  medians<-Biobase::rowMedians(exprs(eset), na.rm=TRUE)
  names(medians)<-featureNames(eset)

  if (missing(gene_symbols)) gene_symbols<-getAnnotationBySymbol(eset)

  # Calculate the gene symbol to probeset mapping (one to one).
  res<-sapply(unique(na.omit(gene_symbols)), function(g) {
      potential.probesets<-names(gene_symbols)[which(gene_symbols==g)]
      chosen.probeset<-names(which.max(medians[potential.probesets]))
  })

  # Reduce the ExpressionSet to only the unique mappings, with gene symbols instead.
  new_eset<-eset[res,]
  featureNames(new_eset)<-names(res)

  return(new_eset)
}
