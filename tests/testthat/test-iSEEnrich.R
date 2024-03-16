test_that("test iSEEnrich", {

  GO_collection <- "GO"
  Hs_organism <- "org.Hs.eg.db"
  gene_id <- "SYMBOL"

  results <- iSEEnrich(sce = sce_allen, collection = GO_collection,
                       organism = Hs_organism,
                       gene_identifer = gene_id)
  expect_true(class(results)== "list")
})
