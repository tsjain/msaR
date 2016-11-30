#' as.fasta 
#' 
#' functionality to convert objects to a fasta string. Currently
#' this can handle character objects which are interpreted as filenames or
#' several of the popular means of storing sequence data: \code{\link[ape]{DNAbin}}, 
#' \code{\link[Biostrings]{DNAStringSet}},  \code{\link[Biostrings]{AAStringSet}},
#' \code{\link[Biostrings]{RNAStringSet}}, or \code{\link[Biostrings]{BStringSet}}.
#' 
#' @param obj. (Required.) the sequence/alignment to be displayed. A character vector,  \code{\link[ape]{DNAbin}}, \code{\link[Biostrings]{DNAStringSet}},  \code{\link[Biostrings]{AAStringSet}},
#' or \code{\link[Biostrings]{RNAStringSet}}.
#' 
#' @return A character string in fasta format.
#'  
#' @importFrom ape as.alignment
#' @importFrom ape read.dna
#' @importFrom Biostrings RNAStringSet
#' @importFrom Biostrings AAStringSet
#' @importFrom Biostrings DNAStringSet
#' @importFrom Biostrings BStringSet
#' @importFrom Biostrings readDNAStringSet
#' @importFrom Biostrings readAAStringSet
#' @importFrom Biostrings readRNAStringSet
#' @export
#' @rdname as.fasta
#' @examples 
#' seqfile <- system.file("sequences","AHBA.aln",package="msaR")
#' as.fasta(seqfile)
#' help("as.fasta")
setGeneric("as.fasta", function(obj) {
  standardGeneric("as.fasta")
})
#'
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "DNAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "AAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "RNAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "BStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "DNAMultipleAlignment"), function(obj) {  
  as.fasta(DNAStringSet(obj))
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "AAMultipleAlignment"), function(obj) {  
  as.fasta(DNAStringSet(obj))
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "RNAMultipleAlignment"), function(obj) {  
  as.fasta(DNAStringSet(obj))
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "character"), function(obj) {  
  try(sequences <- read.dna(obj, format = "fasta"))
  if (exists("sequences")) return(as.fasta(sequences))
  try(sequences <- readAAStringSet(obj))
  if (exists("sequences")) return(as.fasta(sequences))
  try(sequences <- readRNAStringSet(obj))
  if (exists("sequences")) return(as.fasta(sequences))
})
#' @rdname as.fasta
setMethod("as.fasta",  signature(obj = "DNAbin"), function(obj) {  
  aln <- as.alignment(obj)
  paste0(">", aln$nam, "\n", aln$seq, collapse="\n")
})


