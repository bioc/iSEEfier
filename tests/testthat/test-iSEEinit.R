test_that("test iSEEinit",{
  initial <- iSEEinit(sce = sce_allen,
                      features = c("Il2rb",
                                   "Klre1"),
                      clusters = "Primary.Type",
                      groups = "Secondary.Type")
  expect_true(is.list(initial))

  initial_with_board <- iSEEinit(sce = sce_allen,
                                 features = c("Il2rb",
                                              "Klre1"),
                                 clusters = "Primary.Type",
                                 groups = "Secondary.Type",
                                 add_markdown_panel = TRUE)
  expect_true(is.list(initial_with_board))
  
  initial_with_df <- iSEEinit(sce = sce_allen,
                                 features = as.data.frame(c("Il2rb",
                                              "Klre1"),nm = "features"),
                                 clusters = "Primary.Type",
                                 groups = "Secondary.Type",
                                 gene_id = "features")
  expect_true(is.list(initial_with_df))

  ## This is to trigger the argument checks
  expect_error({
    iSEEinit(sce = "Pippo",
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  }, "Please provide a SingleCellExperiment as input!")

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = TRUE,
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = character(0L),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = TRUE,
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "pca",
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = c("PCA", "TSNE"),
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = c("Il2rb",
                          "Klre1"),
             clusters = TRUE,
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = c("Il2rb",
                          "Klre1"),
             clusters = c("JaneDoe", "JohnDoe"),
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = FALSE)
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = c("Primary.Type", "Secondary.Type"))
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type",
             add_markdown_panel = "yes")
  })
  expect_error({
    iSEEinit(sce = sce_allen,
             reddim_type = "PCA",
             features = c("Il2rb",
                          "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type",
             add_markdown_panel = c(TRUE, TRUE))
  })


  expect_message({
    init_pippo <- iSEEinit(sce = sce_allen,
                           reddim_type = "PCA",
                           features = c("Il2rb",
                                        "Klre1",
                                        "Pippo"),
                           clusters = "Primary.Type",
                           groups = "Secondary.Type")
  })
  expect_error({
    init_pippo_pluto <- iSEEinit(sce = sce_allen,
                                 reddim_type = "PCA",
                                 features = c("Pluto",
                                              "Pippo"),
                                 clusters = "Primary.Type",
                                 groups = "Secondary.Type")
  })

  sce_nocd <- sce_allen
  colData(sce_nocd) <- NULL

  expect_message({
    initial_cluster_fallback <- iSEEinit(sce = sce_allen,
                                         features = c("Il2rb",
                                                      "Klre1"),
                                         clusters = "Primary_Type",
                                         groups = "Secondary.Type")
  })

  expect_error({
    initial <- iSEEinit(sce = sce_nocd,
                        features = c("Il2rb",
                                     "Klre1"),
                        clusters = "anything",
                        groups = "Secondary.Type")
  })

  expect_message({
    initial_group_fallback <- iSEEinit(sce = sce_allen,
                                       features = c("Il2rb",
                                                    "Klre1"),
                                       clusters = "Primary.Type",
                                       groups = "Secondary_Type")
  })

  expect_error({
    initial <- iSEEinit(sce = sce_nocd,
                        features = c("Il2rb",
                                     "Klre1"),
                        clusters = "Primary.Type",
                        groups = "anything_else")
  })
  
  expect_error({
    initial <- iSEEinit(sce = sce_allen,
                        features = c("Il2rb",
                                     "Klre1"),
                        clusters = "Primary.Type",
                        groups = "Secondary_Type",
                        gene_id = TRUE)
  })
  
  expect_error({
    initial <- iSEEinit(sce = sce_allen,
                        features = as.data.frame(c("Il2rb",
                                                   "Klre1"),nm = "features"),
                        clusters = "Primary.Type",
                        groups = "Secondary_Type",
                        gene_id = "gene_name")
  })

})
