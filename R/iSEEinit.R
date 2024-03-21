#' iSEEinit: Create an initial state of an iSEE instance for gene expression visualization
#'
#' `iSEEinit()` defines the initial setup of an iSEE instance, recommending tailored visual elements to effortlessly illustrate the expression of a gene list in a single view.
#'
#' @param sce SingleCellExperiment object
#' @param features A character vector containing a list of genes
#' @param reddim_type A string vector containing the dimensionality reduction type
#' @param clusters A character string containing the name of the clusters/cell-type/state...(as listed in the colData of the sce)
#' @param groups A character string of the groups/conditions...(as it appears in the colData of the sce)
#' @param add_markdown_panel A logical indicating whether or not to include the MarkdownBoard panel in the initial configuration
#' @param add_dynamicTable_panel A logical indicating whether or not the DynamicMarkerTable and linked panels should be included in the initial configuration
#'
#' @return A list of "Panel" objects specifying the initial state of iSEE instance
#' @export
#' @importFrom methods new
#' @importFrom SummarizedExperiment colData
#' @importFrom SingleCellExperiment reducedDimNames
#' @importFrom methods is
#' @importClassesFrom iSEE ColumnDataPlot
#' @importClassesFrom iSEE ReducedDimensionPlot
#' @importClassesFrom iSEE FeatureAssayPlot
#' @importClassesFrom iSEE RowDataTable
#' @importClassesFrom iSEE ComplexHeatmapPlot
#' @importClassesFrom iSEEu MarkdownBoard
#'
#'
#' @examples
#' sce <- scRNAseq::RichardTCellData()
#' sce <- scuttle::logNormCounts(sce)
#' sce <- scater::runPCA(sce)
#' sce <- scater::runTSNE(sce)
#' gene_list <- c("ENSMUSG00000026581", "ENSMUSG00000005087", "ENSMUSG00000015437")
#' cluster <- "stimulus"
#' group <- "single cell quality"
#' initial <- iSEEinit(sce = sce, features = gene_list, clusters = cluster, groups = group)
iSEEinit <- function(sce,
                     features,
                     reddim_type = "TSNE",
                     clusters = colnames(colData(sce))[1],
                     groups = colnames(colData(sce))[1],
                     add_markdown_panel = FALSE,
                     add_dynamicTable_panel = FALSE) {

  ## Checks on arguments
  if (!is(sce, "SingleCellExperiment"))
    stop("Please provide a SingleCellExperiment as input!")

  stopifnot(is.character(features))
  stopifnot(length(features) > 0)

  stopifnot(is.character(reddim_type))
  stopifnot(length(reddim_type) == 1)

  stopifnot(is.character(clusters))
  stopifnot(length(clusters) == 1)

  stopifnot(is.character(groups))
  stopifnot(length(groups) == 1)

  stopifnot(is.logical(add_markdown_panel))
  stopifnot(length(add_markdown_panel) == 1)
  stopifnot(is.logical(add_dynamicTable_panel))
  stopifnot(length(add_dynamicTable_panel) == 1)

  if (!(reddim_type %in% reducedDimNames(sce))) {
    available_reddims <- reducedDimNames(sce)
    stop("The selected reduced dimensionality embedding is not available!\n",
         "Please select one of these: ",
         paste(available_reddims, collapse = ", "))
  }

  if (!all(features %in% rownames(sce))) {
    not_available_features <- features[!features %in% rownames(sce)]
    message("Some of the features specified are not available in the provided sce object!\n",
            "Feature(s) not found: ",
            paste(not_available_features, collapse = ", "))

    features <- features[features %in% rownames(sce)]
    if (length(features) == 0)
      stop("No features available!")
  }

  if (!(clusters %in% colnames(colData(sce)))) {
    if (ncol(colData(sce)) > 0) {
      fallback_clusters <- colnames(colData(sce))[1]
    } else {
      stop("No colData provided for the `clusters`!")
    }

    message("colData column not found for the `clusters` parameter, defaulting to ",
            fallback_clusters)
  }

  if (!(groups %in% colnames(colData(sce)))) {
    if (ncol(colData(sce)) > 0) {
      fallback_groups <- colnames(colData(sce))[1]
    } else {
      stop("No colData provided for the `groups`!")
    }

    message("colData column not found for the `groups` parameter, defaulting to ",
            fallback_groups)
  }




  initial <- list()
  
  

  for (j in features) {
    initial[[paste0("ReducedDimensionPlot", which(features == j))]] <- new(
      "ReducedDimensionPlot",
      Type = reddim_type,
      ColorBy = "Feature name",
      ColorByFeatureName = j,
      ColorByFeatureSource = paste0("RowDataTable", which(features == j)),
      ColumnSelectionSource = "ColumnDataPlot1",
      SelectionAlpha = 0.05
    )

    initial[[paste0("FeatureAssayPlot", which(features == j))]] <- new(
      "FeatureAssayPlot",
      XAxis = "Column data",
      XAxisColumnData = clusters,
      YAxisFeatureName = j,
      YAxisFeatureSource = paste0("RowDataTable", which(features == j)),
      ColorBy = "Column data",
      ColorByColumnData = clusters
    )

    initial[[paste0("RowDataTable", which(features == j))]] <- new(
      "RowDataTable",
      Selected = j,
      Search = j
    )
  }

  if (length(features) > 1) {
    initial[[paste0("FeatureAssayPlot", length(features) + 1)]] <- new(
      "FeatureAssayPlot",
      XAxis = "Feature name",
      XAxisFeatureName = features[[1]],
      YAxisFeatureName = features[[2]]
    )
  }

  initial[[paste0("ReducedDimensionPlot", length(features) + 1)]] <- new(
    "ReducedDimensionPlot",
    Type = reddim_type,
    ColorByColumnData = clusters,
    ColorBy = "Column data",
    ColumnSelectionSource = paste0("FeatureAssayPlot", length(features) + 1),
    FacetColumnBy = "Column data",
    FacetColumnByColData = groups,
    SelectionAlpha = 0.05
  )


  initial[["ComplexHeatmapPlot1"]] <- new(
    "ComplexHeatmapPlot",
    CustomRowsText = paste(features, collapse = "\n"),
    ColumnData = clusters
  )

  initial[["ColumnDataPlot1"]] <- new(
    "ColumnDataPlot",
    YAxis = clusters,
    ColorBy = "Column data",
    ColorByColumnData = clusters,
    PanelWidth = 6L
  )

  if (add_markdown_panel == TRUE) {
    initial[["MarkdownBoard1"]] <- new(
      "MarkdownBoard",
      Content = "# Placeholder\n\nFill me with text!",
      PanelWidth = 4L)
  }

  if(add_dynamicTable_panel == TRUE) {
    initial[[paste0("ReducedDimensionPlot",length(features)+2)]] <- new(
      "ReducedDimensionPlot",
      Type = reddim_type,
      ColorByColumnData = clusters,
      ColorBy = "Column data",
      SelectionAlpha = 0.05,
      ColumnSelectionSource = paste0("FeatureAssayPlot",length(features)+2))


    initial[["DynamicMarkerTable1"]] <- new(
      "DynamicMarkerTable",
      ColumnSelectionSource = paste0("ReducedDimensionPlot",length(features)+2))

    initial[[paste0("FeatureAssayPlot",length(features)+2)]] <- new(
      "FeatureAssayPlot",
      XAxis = "Column data",
      XAxisColumnData = groups,
      YAxisFeatureSource = "DynamicMarkerTable1")
  }

  return(initial)

}
