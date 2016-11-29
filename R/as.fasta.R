#' to_fasta functionality to convert objects to a fasta string
#' 
#' @export
setGeneric("as.fasta",function(obj) {standardGeneric("as.fasta")})
#'
#' @importFrom Biostrings DNAStringSet
setMethod("as.fasta",  signature(obj = "DNAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#' @importFrom Biostrings AAStringSet
setMethod("as.fasta",  signature(obj = "AAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#' @importFrom Biostrings RNAStringSet
setMethod("as.fasta",  signature(obj = "RNAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#' @importFrom Biostrings BStringSet
setMethod("as.fasta",  signature(obj = "BStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
#'
setMethod("as.fasta",  signature(obj = "DNAMultipleAlignment"), function(obj) {  
  as.fasta(DNAStringSet(obj))
})
setMethod("as.fasta",  signature(obj = "AAMultipleAlignment"), function(obj) {  
  as.fasta(DNAStringSet(obj))
})
setMethod("as.fasta",  signature(obj = "RNAMultipleAlignment"), function(obj) {  
  as.fasta(DNAStringSet(obj))
})
#' @importFrom ape read.dna
#' @importFrom Biostrings readDNAStringSet
#' @importFrom Biostrings readAAStringSet
#' @importFrom Biostrings readRNAStringSet
setMethod("as.fasta",  signature(obj = "character"), function(obj) {  
  try(sequences <- read.dna(obj, format = "fasta"))
  if (exists("sequences")) return(as.fasta(sequences))
  try(sequences <- readAAStringSet(obj))
  if (exists("sequences")) return(as.fasta(sequences))
  try(sequences <- readRNAStringSet(obj))
  if (exists("sequences")) return(as.fasta(sequences))
})
#'
#'  @importFrom ape as.alignment
setMethod("as.fasta",  signature(obj = "DNAbin"), function(obj) {  
  aln <- as.alignment(obj)
  paste0(">", aln$nam, "\n", aln$seq, collapse="\n")
})
