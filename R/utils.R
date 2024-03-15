#' View an initial object as a set of tiles
#'
#' Previews the layout of the `initial` configuration object in a graphical form.
#'
#' Tiles are used to represent the panel types, and reflect the values of their
#' width.
#' This can be a compact visualization to obtain an overview for the configuration,
#' without the need of fully launching the app and loading the content of all
#' panels
#'
#' @details
#' This function is particularly useful with mid-to-large `initial` objects, as
#' they can be quickly generated in a programmatic manner via the `iSEEinit()`
#' provided in this package.
#'
#'
#' @param initial An `initial` list object, in the format that is required to
#' be passed as a parameter in the call to [iSEE::iSEE()].
#'
#' @return A `ggplot` object, representing a schematic view for the `initial`
#' object.
#'
#' @export
#'
#' @importFrom stats na.omit
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual scale_y_discrete
#' theme theme_void
#' @importFrom rlang .data
#'
#' @seealso [view_initial_network()]
#'
#' @examples
view_initial_tiles <- function(initial) {

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
              aes(x = .data$Var1,
                  y = .data$Var2)) +
    geom_tile(aes(fill = .data$panel_type), col = "white") +
    scale_y_discrete(limits = rev(levels(waffled_matrix_long$Var2))) +
    theme_void() +
    scale_fill_manual(
      name = "iSEE Panel type",
      values = iSEE_panel_colors
    ) +
    theme(legend.position="bottom")

  return(p)
}


#' View an initial object as a network
#'
#' Translates the layout of the `initial` configuration object as a networks,
#' representing panels as nodes and links between them as edges.
#'
#' Panels are the nodes, with color and names to identify them easily.
#' The connections among panels are represented through directed edges.
#' This can be a compact visualization to obtain an overview for the configuration,
#' without the need of fully launching the app and loading the content of all
#' panels
#'
#' @details
#' This function is particularly useful with mid-to-large `initial` objects, as
#' they can be quickly generated in a programmatic manner via the `iSEEinit()`
#' provided in this package.
#'
#' @param initial An `initial` list object, in the format that is required to
#' be passed as a parameter in the call to [iSEE::iSEE()].
#' @param plot_format Character string, one of `igraph`, `visNetwork`, or `none`.
#' Defaults to `igraph`. Determines the format of the visual representation
#' generated as a side effect of this function - it can be the output of the
#' `plot()` function for `igraph` objects, or an interactive widget created
#' via `visNetwork::visNetwork()`.
#'
#' @return An `igraph` object, underlying the visual representation provided.
#'
#' @export
#'
#' @importFrom igraph graph_from_data_frame graph.empty V V<-
#' @importFrom visNetwork visNetwork visEdges toVisNetworkData
#'
#' @seealso [view_initial_tiles()]
#'
#' @examples
view_initial_network <- function(initial,
                              plot_format = c("igraph", "visNetwork", "none")) {

  plot_format <- match.arg(plot_format, c("igraph", "visNetwork", "none"))

  panel_widths <- vapply(initial,
                         function(arg) {
                           arg@PanelWidth
                         },
                         FUN.VALUE = numeric(1))

  # check: max value should be 12 (but it is a given through iSEE)

  panel_types <- vapply(initial, class, character(1))

  # need to have SIMPLIFIED configs
  panel_ids <- names(initial)

  panel_links <- vapply(initial,
                        function(arg) {
                          arg@ColumnSelectionSource
                        },
                        FUN.VALUE = character(1))
  panel_edges <- panel_links[panel_links != "---"]

  graph_nodes_df <- data.frame(
    name = panel_ids
  )

  if (length(panel_edges) > 0) {
    graph_edges_df <- data.frame(
      from = names(panel_edges),
      to = panel_edges
    )
    g <- graph_from_data_frame(graph_edges_df, vertices = graph_nodes_df)
  } else {
    g <- graph.empty(n = length(panel_ids))
    V(g)$name <- panel_ids
  }

  V(g)$color <- iSEE_panel_colors[panel_types]

  if (plot_format == "igraph") {
    plot(g, vertex.label.family = "Helvetica")
  } else if (plot_format == "visNetwork") {
    gdata <- visNetwork::toVisNetworkData(g)
    print(visNetwork(nodes = gdata$nodes,
               edges = gdata$edges) |>
      visEdges(arrows = "to",
               smooth = TRUE))
  } else if (plot_format == "none") {
    message("Returning the graph object...")
  }
  return(g)
}




#' Title
#'
#' @param ...
#' @param remove_duplicate_panels
#' @param verbose
#' @param custom_panels_allowed
#'
#' @return
#' @export
#'
#' @examples
glue_initials <- function(...,
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
            "you can call `view_initial_tiles()` on the output of this function"
    )
  }

  return(glued_configs)
}



#' Constant values used throughout iSEEfier
#'
#' @name constants-iSEEfier
#'
#' @section Panel colors:
#' * color values (as string character or hex value) for the panels included by
#'   default in `iSEE` and `iSEEu`
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
