% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getExpressionSetByGeneSymbol.R
\name{getExpressionSetByGeneSymbol}
\alias{getExpressionSetByGeneSymbol}
\title{This function takes an expression set (assumed to be Affymetrix), and reduces the features (probesets) to only one
entry per gene symbol. The featurenames of the returned expression set are the gene symbols.
Currently, this is only implemented for taking the highest median expression if there
are duplicates.}
\usage{
getExpressionSetByGeneSymbol(eset, gene_symbols)
}
\arguments{
\item{eset}{\itemize{
\item An expression set to reduce to gene symbol reporters.
}}

\item{gene_symbols}{\itemize{
\item A list of gene symbols (in order of reporters).
}}
}
\value{
A modified expression set (based on eset) with only features that are genes.
}
\description{
TODO: Add more ways of summarizing.
}
