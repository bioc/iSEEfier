test_that("test iSEEinit",{
  initial <- iSEEinit(sce = sce_allen,
                      feature.list = c("IL2rb",
                                       "Klre1"),
                      clusters = "Primary.Type",
                      groups = "Secondary.Type")
  expect_true(is.list(initial))

  initial_with_board <- iSEEinit(sce = sce_allen,
                                 feature.list = c("IL2rb",
                                                  "Klre1"),
                                 clusters = "Primary.Type",
                                 groups = "Secondary.Type",
                                 markdownboard = TRUE)
  expect_true(is.list(initial_with_board))

  initial_with_dynamictable <- iSEEinit(sce = sce_allen,
                                        reddim.type = "PCA",
                                        feature.list = c("IL2rb",
                                                         "Klre1"),
                                        clusters = "Primary.Type",
                                        groups = "Secondary.Type",
                                        dynamicMarkerTable = TRUE)
  expect_true(is.list(initial_with_dynamictable))
})
