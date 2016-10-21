#' to_fasta functionality to convert objects to a fasta string
#' 
#' @importFrom Biostrings DNAStringSet
#' @importFrom Biostrings AAStringSet
#' @importFrom Biostrings DNAMultipleAlignment
#' @importFrom Biostrings AAMultipleAlignment
#' @importFrom Biostrings readDNAStringSet
#' @importFrom Biostrings readAAStringSet
#' 
setGeneric("as.fasta",function(obj) {standardGeneric("as.fasta")})
#'
#'
setMethod("as.fasta",  signature(obj = "DNAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
setMethod("as.fasta",  signature(obj = "AAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
setMethod("as.fasta",  signature(obj = "RNAStringSet"), function(obj) {  
  newnames <- paste0(">", names(obj))
  recs <- c(rbind(newnames, as.character(obj)))
  paste(recs, collapse="\n")
})
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
#'
setMethod("as.fasta",  signature(obj = "character"), function(obj) {  
  try(sequences <- readDNAStringSet(obj))
  if (exists("sequences")) return(as.fasta(sequences))
  try(sequences <- readAAStringSet(obj))
  if (exists("sequences")) return(as.fasta(sequences))
  try(sequences <- readRNAStringSet(obj))
  if (exists("sequences")) return(as.fasta(sequences))
})

