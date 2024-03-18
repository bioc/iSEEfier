test_that("test iSEEnrich", {

  GO_collection <- "GO"
  Mm_organism <- "org.Mm.eg.db"
  gene_id <- "SYMBOL"

  results <- iSEEnrich(sce = sce_allen,
                       collection = GO_collection,
                       organism = Mm_organism,
                       gene_identifier = gene_id)
  expect_true(is.list(results))
  expect_true(is.list(results$initial))
  expect_true(is(results$sce, "SingleCellExperiment"))

  ## This is to trigger the argument checks
  expect_error({
    iSEEnrich(sce = "Pippo",
              collection = GO_collection,
              organism = Mm_organism,
              gene_identifier = gene_id)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = "MSIGDB",
              organism = Mm_organism,
              gene_identifier = gene_id)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = c("MSIGDB", "GO"),
              organism = Mm_organism,
              gene_identifier = gene_id)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = TRUE,
              gene_identifier = gene_id)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = c("org.Mm.eg.db", "mouse"),
              gene_identifier = gene_id)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = c("mouse"),
              gene_identifier = gene_id)
  })





})
