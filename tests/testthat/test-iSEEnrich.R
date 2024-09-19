test_that("test iSEEnrich", {

  GO_collection <- "GO"
  Mm_organism <- "org.Mm.eg.db"
  gene_id <- "SYMBOL"
  clusters <- "Primary.Type"
  groups <- "Secondary.Type"
  reddim_type <- "PCA"

  results <- iSEEnrich(sce = sce_allen,
                       collection = GO_collection,
                       organism = Mm_organism,
                       gene_identifier = gene_id,
                       clusters = clusters,
                       groups = groups,
                       reddim_type = reddim_type)
  
  expect_true(is.list(results))
  expect_true(is.list(results$initial))
  expect_true(is(results$sce, "SingleCellExperiment"))

  ## This is to trigger the argument checks
  expect_error({
    iSEEnrich(sce = "Pippo",
              collection = GO_collection,
              organism = Mm_organism,
              gene_identifier = gene_id,
              clusters = clusters,
              groups = groups,
              reddim_type = reddim_type)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = "MSIGDB",
              organism = Mm_organism,
              gene_identifier = gene_id,
              clusters = clusters,
              groups = groups,
              reddim_type = reddim_type)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = c("MSIGDB", "GO"),
              organism = Mm_organism,
              gene_identifier = gene_id,
              clusters = clusters,
              groups = groups,
              reddim_type = reddim_type)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = TRUE,
              gene_identifier = gene_id,
              clusters = clusters,
              groups = groups,
              reddim_type = reddim_type)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = c("org.Mm.eg.db", "mouse"),
              gene_identifier = gene_id,
              clusters = clusters,
              groups = groups,
              reddim_type = reddim_type)
  })

  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = c("mouse"),
              gene_identifier = gene_id,
              clusters = clusters,
              groups = groups,
              reddim_type = reddim_type)
  })
  
  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = Mm_organism,
              gene_identifier = gene_id,
              clusters = TRUE,
              groups = groups,
              reddim_type = reddim_type)
  })
  
  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = Mm_organism,
              gene_identifier = gene_id,
              clusters = TRUE,
              groups = c("Primary.Type", "Secondary.Type"),
              reddim_type = reddim_type)
  })
  
  expect_error({
    iSEEnrich(sce = sce_allen,
              collection = GO_collection,
              organism = Mm_organism,
              gene_identifier = gene_id,
              clusters = clusters,
              groups = groups,
              reddim_type = TRUE)
  })
  

})
