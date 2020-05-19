#' Write an ExpressionSet to a GCT-formatted output file.
#'
#' @param eset The ExpressionSet to write.
#' @param file The file to write to.
#'
#' @return NULL
#' @export
#'
#' @examples
#' writeGCT(eset, file="/tmp/exprs.gct")
#'
writeGCT<-function(eset, file) {
  stopifnot(class(eset)=="ExpressionSet")

  # GCT requires two columns: a NAME and Description. They will both be the
  # featureNames from the ExpressionSet.
  output<-data.frame(NAME=featureNames(eset),
                     Description=featureNames(eset),
                     exprs(eset),
                     stringsAsFactors = FALSE,
                     check.names = FALSE)

  # Create the header separately
  hdr<-data.frame(a=c("#1.2",dim(eset)[1]),b=c("",dim(eset)[2]))

  # This bit writes the header first, then appends the data. The warnings are
  # suppressed for the second part, since it's a different size. The first one
  # may fail due to file not being created, so the suppress may be ok.
  write.table(hdr, file=file, quote=F, sep="\t", row.names=FALSE, col.names=FALSE)
  suppressWarnings(
    write.table(output, file=file, append=TRUE, quote=F, sep="\t",row.names=FALSE)
  )


}
