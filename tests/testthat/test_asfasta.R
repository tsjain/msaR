################################################################################
# Use testthat to test fasta conversion
################################################################################
library("msaR"); packageVersion("msaR")
library("testthat"); packageVersion("testthat")
context('Checking as.fasta conversions')

seqfile <- system.file("sequences","AHBA.aln",package="msaR")


test_that("as.fasta conversions work", {
  expect_is(as.fasta(seqfile), "character")
})

test_that("as.fasta conversions work for Biostrings", {
  if (!require("Biostrings")) {
    skip("Biostrings not availalbe. Skip")
  }
  
  seqs <- Biostrings::readDNAStringSet(seqfile)
  expect_is(as.fasta(seqs), "character")
})


