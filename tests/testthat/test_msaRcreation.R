################################################################################
# Use testthat to test fasta conversion
################################################################################
library("msaR"); packageVersion("msaR")
library("testthat"); packageVersion("testthat")
context('Checking basic msaR creation')

seqfile <- system.file("sequences","AHBA.aln",package="msaR")


test_that("basic msaR works", {
  expect_is(msaR(seqfile), "htmlwidget")
})
