test_that("test iSEEinit",{
  sce <- scRNAseq::ReprocessedAllenData(assays = "tophat_counts")
  sce <- scuttle::logNormCounts(sce, exprs_values="tophat_counts")
  sce <- scater::runPCA(sce)
  sce <- scater::runTSNE(sce)
  initial <- iSEEinit(sce = sce,
                      feature.list = c("IL2rb",
                                       "Klre1"),
                      clusters = "Primary.Type",
                      groups = "Secondary.Type")
  expect_true(class(initial)== "list")
})
