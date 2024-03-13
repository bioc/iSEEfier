test_that("test iSEEnrich", {
  sce <- scRNAseq::BacherTCellData()
  sce <- scuttle::logNormCounts(sce)
  sce <- scater::runPCA(sce)
  GO_collection <- "GO"
  Hs_organism <- "org.Hs.eg.db"
  gene_id <- "SYMBOL"
  results <- iSEEnrich(sce = sce, collection = GO_collection,
                       organism = Hs_organism,
                       gene_identifer = gene_id)
  expect_true(class(results)== "list")
})
