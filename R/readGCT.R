#' Read a GCT-formatted file into an ExpressionSet object.
#'
#' @param file The name of the file (GCT format) to read in.
#'
#' @return An ExpressionSet representing the GCT data.
#' @export
#'
#' @examples
#' eset<-readGCT("/tmp/data.gct")
#'
readGCT<-function(file) {
  require(Biobase)

  stopifnot(typeof(file)=="character")

  # A GCT is a matrix (after line 2), but with two identifier columns
  d<-read.table(file=file,
                    header=TRUE,
                    row.names=1,
                    sep="\t",
                    skip=2,
                    as.is=TRUE,
                    check.names=FALSE,
                    comment.char="",
                    na.strings="",
                    quote="",
                    blank.lines.skip=TRUE
                    )

  vals<-data.matrix(d[,-1])
  descriptions<-data.frame(Description=d[,1],
                           row.names=rownames(d),
                           stringsAsFactors = FALSE
                           )
  res<-Biobase::ExpressionSet(assayData=vals,
                              featureData=AnnotatedDataFrame(descriptions))

  return(res)
}
