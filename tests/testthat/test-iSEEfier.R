test_that("test iSEEinit",{
  initial <- iSEEinit(sce = sce_allen,
                      feature.list = c("Il2rb",
                                       "Klre1"),
                      clusters = "Primary.Type",
                      groups = "Secondary.Type")
  expect_true(is.list(initial))

  initial_with_board <- iSEEinit(sce = sce_allen,
                                 feature.list = c("Il2rb",
                                                  "Klre1"),
                                 clusters = "Primary.Type",
                                 groups = "Secondary.Type",
                                 markdownboard = TRUE)
  expect_true(is.list(initial_with_board))

  initial_with_dynamictable <- iSEEinit(sce = sce_allen,
                                        reddim.type = "PCA",
                                        feature.list = c("Il2rb",
                                                         "Klre1"),
                                        clusters = "Primary.Type",
                                        groups = "Secondary.Type",
                                        dynamicMarkerTable = TRUE)
  expect_true(is.list(initial_with_dynamictable))

  ## This is to trigger the argument checks
  expect_error({
    iSEEinit(sce = "Pippo",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  }, "Please provide a SingleCellExperiment as input!")

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = TRUE,
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = character(0L),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = TRUE,
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "pca",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = c("PCA", "TSNE"),
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = TRUE,
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = c("JaneDoe", "JohnDoe"),
             groups = "Secondary.Type")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = FALSE)
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = c("Primary.Type", "Secondary.Type"))
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type",
             markdownboard = "yes")
  })
  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type",
             markdownboard = c(TRUE, TRUE))
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type",
             dynamicMarkerTable= "no")
  })

  expect_error({
    iSEEinit(sce = sce_allen,
             reddim.type = "PCA",
             feature.list = c("Il2rb",
                              "Klre1"),
             clusters = "Primary.Type",
             groups = "Secondary.Type",
             dynamicMarkerTable= c(FALSE, FALSE))
  })

  expect_message({
    init_pippo <- iSEEinit(sce = sce_allen,
                           reddim.type = "PCA",
                           feature.list = c("Il2rb",
                                            "Klre1",
                                            "Pippo"),
                           clusters = "Primary.Type",
                           groups = "Secondary.Type")
  })
  expect_error({
    init_pippo_pluto <- iSEEinit(sce = sce_allen,
                                 reddim.type = "PCA",
                                 feature.list = c("Pluto",
                                                  "Pippo"),
                                 clusters = "Primary.Type",
                                 groups = "Secondary.Type")
  })

  sce_nocd <- sce_allen
  colData(sce_nocd) <- NULL

  expect_message({
    initial_cluster_fallback <- iSEEinit(sce = sce_allen,
                                         feature.list = c("Il2rb",
                                                          "Klre1"),
                                         clusters = "Primary_Type",
                                         groups = "Secondary.Type")
  })

  expect_error({
    initial <- iSEEinit(sce = sce_nocd,
                        feature.list = c("Il2rb",
                                         "Klre1"),
                        clusters = "anything",
                        groups = "Secondary.Type")
  })

  expect_message({
    initial_group_fallback <- iSEEinit(sce = sce_allen,
                                         feature.list = c("Il2rb",
                                                          "Klre1"),
                                         clusters = "Primary.Type",
                                         groups = "Secondary_Type")
  })

  expect_error({
    initial <- iSEEinit(sce = sce_nocd,
                        feature.list = c("Il2rb",
                                         "Klre1"),
                        clusters = "Primary.Type",
                        groups = "anything_else")
  })

})
