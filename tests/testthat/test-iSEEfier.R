test_that("test iSEEinit",{
  initial <- iSEEinit(sce = sce_allen,
                      feature.list = c("IL2rb",
                                       "Klre1"),
                      clusters = "Primary.Type",
                      groups = "Secondary.Type")
  expect_true(class(initial)== "list")
})
