test_that("test iSEEfier",{
  sce <- scRNAseq::RichardTCellData()
  sce <- scuttle::logNormCounts(sce)
  sce <- BiocSingular::runPCA(sce, ncomponents=5)
  sce <- scater::runTSNE(sce)
  initial <- iSEEfier(sce = sce,
                      feature.list = c("ENSMUSG00000026581", "ENSMUSG00000005087"),
                      clusters = "stimulus",
                      conditions = "single cell quality")
})