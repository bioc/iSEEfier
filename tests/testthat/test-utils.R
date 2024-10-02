test_that("iSEEfier utils work", {
  init_1 <- iSEEinit(sce = sce_allen,
                     features = c("Il2rb",
                                  "Klre1"),
                     clusters = "Primary.Type",
                     groups = "Secondary.Type")
  expect_true(is.list(init_1))

  init_2 <- iSEEinit(sce = sce_allen,
                     features = c("Actb", "Gapdh"),
                     clusters = "Primary.Type",
                     groups = "Secondary.Type",
                     add_markdown_panel = TRUE)
  expect_true(is.list(init_2))

  expect_true(length(init_1) == 10)
  expect_true(length(init_2) == 11)

  p_tiles_1 <- view_initial_tiles(init_1)
  p_tiles_2 <- view_initial_tiles(init_2)

  expect_true(is(p_tiles_1, "gg"))
  expect_true(is(p_tiles_2, "gg"))

  g_tiles_1 <- view_initial_network(init_1)
  g_tiles_2 <- view_initial_network(init_2, plot_format = "visNetwork")
  expect_true(is(g_tiles_1, "igraph"))
  expect_true(is(g_tiles_2, "igraph"))

  init_chopped <- init_1[c(3,6)]
  expect_message({
    g_tiles_noedges <- view_initial_network(init_chopped, plot_format = "none")
  }, "Returning the graph object")

  expect_true(length(igraph::E(g_tiles_noedges)) == 0)

  expect_message({
    expect_message({
      init_combined <- glue_initials(init_1, init_2, remove_duplicate_panels = TRUE)
      }, "Dropping"
    )}, "Merging together")

  expect_true(length(init_combined) == 20)

  p_tiles_combined <- view_initial_tiles(init_combined)
  expect_true(is(p_tiles_combined, "gg"))

  ## Covering edge cases
  expect_error(
    glue_initials(init_1, init_2$ReducedDimensionPlot1),
    "You need to provide a set of `initial` configuration lists for iSEE"
  )

  init_mod <- init_1
  names(init_mod)[1] <- "ImaginaryPanel1"
  class(init_mod[[1]]) <- "ImaginaryPanel"
  expect_message({
    expect_error(
      glue_initials(init_mod, init_2),
      "Some elements included in the provided input are not recognized as iSEE panels!"
    )}, "Combining sets of 10, 11 different panels")

  ## This one works by enabling the specific panel
  expect_message({
    glued_custom <- glue_initials(init_mod, init_2,
                                  custom_panels_allowed = "ImaginaryPanel",
                                  remove_duplicate_panels = FALSE)},
    "Returning an `initial` configuration including 21 different panels. Enjoy!")

  expect_true(length(glued_custom) == 21)

})
