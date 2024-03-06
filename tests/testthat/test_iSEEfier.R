test_that("test iSEEfier",{
  sce <- scRNAseq::RichardTCellData()
  sce <- scuttle::logNormCounts(sce)
  sce <- BiocSingular::runPCA(sce)
  sce <- scater::runTSNE(sce)
  initial <- iSEEfier(sce = sce,
                      feature.list = c("ENSMUSG00000026581", #SELL
                                       "ENSMUSG00000005087"), #CD44
                      clusters = "stimulus",
                      conditions = "single cell quality")
  expect_true(class(initial)== "list")
})
