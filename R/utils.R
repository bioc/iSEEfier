#' Title
#'
#' @param initial
#'
#' @return
#' @export
#'
#' @examples
iSEEconfigviewer <- function(initial) {

  panel_widths <- vapply(initial,
                         function(arg) {
                           arg@PanelWidth
                         },
                         FUN.VALUE = numeric(1))

  # check: max value should be 12 (but it is a given through iSEE)

  panel_types <- vapply(initial, class, character(1))

  total_tiles <- sum(panel_widths)

  nr_rows <- ceiling(total_tiles / 12)

  # pre-fill all with white
  tiles_vector <- rep("white", nr_rows * 12)
  panel_vector <- rep(NA, nr_rows * 12)

  cur_rowpos <- 0
  abs_pos <- 0
  cur_row <- 1

  # fill in the tiles vector "with the rule of 12"
  for(i in seq_len(length(initial))) {
    this_width <- panel_widths[i]
    this_paneltype <- panel_types[i]
    this_color <- iSEE_panel_colors[this_paneltype]

    max_end <- cur_row * 12 # for each row, to be updated

    tiles_start <- abs_pos + 1
    tiles_end <- abs_pos + this_width

    if (tiles_end <= max_end) {
      # color as usual, update current position(s)
      tiles_vector[tiles_start:tiles_end] <- this_color
      panel_vector[tiles_start:tiles_end] <- this_paneltype
      cur_rowpos <- cur_rowpos + this_width
      abs_pos <- abs_pos + this_width

    } else {
      # re-start from the position in the new row
      cur_row <- cur_row + 1
      cur_rowpos <- 0
      abs_pos <- (cur_row - 1) * 12

      # update tiles start
      tiles_start <- abs_pos + 1
      tiles_end <- abs_pos + this_width

      # color, and update as usual
      tiles_vector[tiles_start:tiles_end] <- this_color
      panel_vector[tiles_start:tiles_end] <- this_paneltype
      cur_rowpos <- cur_rowpos + this_width
      abs_pos <- abs_pos + this_width
    }

  }

  # if needed, might require extra rows...
  if (cur_row > nr_rows) {
    # fill with white & NA the dedicated vectors
    tiles_vector[(abs_pos + 1):(cur_row * 12)] <- "white"
    panel_vector[(abs_pos + 1):(cur_row * 12)] <- NA
  }

  # waffled_matrix <- matrix(data = tiles_vector,
                           # nrow = nr_rows, byrow = TRUE)

  waffled_matrix_long <- expand.grid(seq_len(12), seq_len(cur_row))
  # waffled_matrix_long <- expand.grid(seq_len(12), seq_len(nr_rows))
  waffled_matrix_long$Var1 <- as.factor(waffled_matrix_long$Var1)
  waffled_matrix_long$Var2 <- as.factor(waffled_matrix_long$Var2)
  waffled_matrix_long$panel_color <- tiles_vector
  waffled_matrix_long$panel_type <- panel_vector

  # waffled_matrix_long$panel_name <- panel_types


  # p <- ggplot(waffled_matrix_long,
  #             aes(x=.data$Var1,
  #                 y=.data$Var2)) +
  #   geom_tile(aes(fill = I(panel_color)), col = "white") +
  #   scale_y_discrete(limits = rev(levels(waffled_matrix_long$Var2))) +
  #   theme_void() +
  #   scale_fill_manual(
  #     values = c("ReducedDimensionPlot" = "#3565AA")
  #     # values = iSEE_panel_colors
  #     # labels = names(iSEE_panel_colors)
  #   )


  p <- ggplot(na.omit(waffled_matrix_long),
              aes(x=.data$Var1,
                  y=.data$Var2)) +
    geom_tile(aes(fill = panel_type), col = "white") +
    scale_y_discrete(limits = rev(levels(waffled_matrix_long$Var2))) +
    theme_void() +
    scale_fill_manual(
      name = "iSEE Panel type",
      values = iSEE_panel_colors
    ) +
    theme(legend.position="bottom")

  return(p)
}






#' Title
#'
#' @param ...
#' @param remove_duplicate_panels
#'
#' @return
#' @export
#'
#' @examples
glue_configs <- function(...,
                         remove_duplicate_panels = TRUE,
                         verbose = TRUE,
                         custom_panels_allowed = NULL) {

  config_as_list <- list(...)

  allowed_panels <- names(iSEE_panel_colors)

  if (!is.null(custom_panels_allowed)) {
    stopifnot(is.character(custom_panels_allowed))
    allowed_panels <- c(allowed_panels, custom_panels_allowed)
  }

  # all things to be concatenated need to be lists ("initial" lists)
  if (!all(unlist(lapply(config_as_list, is.list))))
    stop("You need to provide a set of `initial` configuration lists for iSEE")

  nr_configs <- length(config_as_list)
  nr_panels <- lengths(config_as_list)

  if (verbose) {
    message(
      "Merging together ",
      nr_configs,
      " `initial` configuration objects...\n",
      "Combining sets of ",
      paste(nr_panels, collapse = ", "),
      " different panels."
    )
  }

  # checking that all the components are legit panel configurations
  concatenated_configs <- c(...)
  panel_types <- vapply(concatenated_configs, class, character(1))

  if (!all(panel_types %in% allowed_panels))
    stop("Some elements included in the provided input are not recognized as iSEE panels!")

  if (remove_duplicate_panels) {
    dupe_panels <- duplicated(concatenated_configs)
    glued_configs <- concatenated_configs[!dupe_panels]
    if (verbose)
      message("\nDropping ",
              sum(dupe_panels), " of the original list of ",
              length(concatenated_configs), " (detected as duplicated entries)"
      )
  } else {
    glued_configs <- concatenated_configs
  }

  if (verbose) {
    if (any(duplicated(names(glued_configs)))) {
      message("\nSome names of the panels were specified by the same name, ",
              "but this situation can be handled at runtime by iSEE\n",
              "(This is just a non-critical message)")
    }

    message("\nReturning an `initial` configuration including ",
            length(glued_configs),
            " different panels. Enjoy!\n",
            "If you want to obtain a preview of the panels configuration, ",
            "you can call `iSEEconfigviewer()` on the output of this function"
    )
  }

  return(glued_configs)
}


iSEE_panel_colors <- c(
  ReducedDimensionPlot = "#3565AA",
  FeatureAssayPlot = "#7BB854",
  SampleAssayPlot = "#07A274",
  ColumnDataPlot = "#DB0230",
  ColumnDataTable = "#B00258",
  RowDataPlot = "#F2B701",
  RowDataTable = "#E47E04",
  ComplexHeatmapPlot = "#440154FF",
  AggregatedDotPlot = "#703737FF",
  MarkdownBoard = "black",
  DynamicMarkerTable = "#B73CE4",
  DynamicReducedDimensionPlot = "#0F0F0F",
  FeatureSetTable = "#BB00FF",
  GeneSetTable = "#BB00FF",
  LogFCLogFCPlot = "#770055",
  MAPlot = "#666600",
  VolcanoPlot = "#DEAE10"
)
