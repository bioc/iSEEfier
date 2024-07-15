test_that("test iSEEmarker", {
  initial <- iSEEmarker(sce = sce_allen,
                        clusters = "Primary.Type",
                        groups = "Secondary.Type")
  
  expect_true(is.list(initial))
  
  initial1 <- iSEEmarker(
    sce = sce_allen,
    clusters = "Primary.Type",
    groups = "Secondary.Type",
    selection_plot_format = "ReducedDimensionPlot"
  )
  expect_true(is.list(initial1))
  
  ## This is to trigger the argument checks
  
  expect_error({
    iSEEmarker(sce = "UwU")
  }, "Please provide a SingleCellExperiment as input!")
  
  
  expect_error({
    iSEEmarker(
      sce = sce_allen,
      reddim_type = "PCA",
      clusters = TRUE,
      groups = "Secondary.Type"
    )
  })
  
  expect_error({
    iSEEmarker(
      sce = sce_allen,
      reddim_type = "PCA",
      clusters = "Secondary.Type",
      groups = FALSE
    )
  })
  
  expect_error({
    iSEEmarker(
      sce = sce_allen,
      reddim_type = "PCA",
      clusters = "Secondary.Type",
      groups = "Primary.Type",
      selection_plot_format = TRUE
    )
  })
  
  expect_error({
    iSEEmarker(
      sce = sce_allen,
      reddim_type = "PCA",
      clusters = "Secondary.Type",
      groups = "Primary.Type",
      selection_plot_format = "reddim"
    )
    
  })
})
