#' iSEEinit: Create an initial state of an iSEE instance for gene expression visualization
#'
#' `iSEEinit()` defines the initial setup of an iSEE instance, recommending tailored visual elements to effortlessly illustrate the expression of a gene list in a single view.
#'
#' @param sce SingleCellExperiment object
#' @param feature.list A character vector containing a list of genes
#' @param reddim.type A string vector containing the dimensionality reduction type
#' @param clusters A character string containing the name of the clusters/cell-type/state...(as listed in the colData of the sce)
#' @param groups A character string of the groups/conditions...(as it appears in the colData of the sce)
#' @param markdownboard A logical indicating whether or not to include the MarkdownBoard panel in the initial configuration
#' @param dynamicMarkerTable A logical indicating whether or not the DynamicMarkerTable and linked panels should be included in the initial configuration
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
#' initial <- iSEEinit(sce = sce, feature.list = gene_list, clusters = cluster, groups = group)
iSEEinit <- function(sce,
                     feature.list,
                     reddim.type = "TSNE",
                     clusters = colnames(colData(sce))[1],
                     groups = colnames(colData(sce))[1],
                     markdownboard = FALSE,
                     dynamicMarkerTable = FALSE) {

  ## Checks on arguments
  if (!is(sce, "SingleCellExperiment"))
    stop("Please provide a SingleCellExperiment as input!")

  stopifnot(is.character(feature.list))
  stopifnot(length(feature.list) > 0)

  stopifnot(is.character(reddim.type))
  stopifnot(length(reddim.type) == 1)

  stopifnot(is.character(clusters))
  stopifnot(length(clusters) == 1)

  stopifnot(is.character(groups))
  stopifnot(length(groups) == 1)

  stopifnot(is.logical(markdownboard))
  stopifnot(length(markdownboard) == 1)
  stopifnot(is.logical(dynamicMarkerTable))
  stopifnot(length(dynamicMarkerTable) == 1)

  if (!(reddim.type %in% reducedDimNames(sce))) {
    available_reddims <- reducedDimNames(sce)
    stop("The selected reduced dimensionality embedding is not available!\n",
         "Please select one of these: ",
         paste(available_reddims, collapse = ", "))
  }

  if (!all(feature.list %in% rownames(sce))) {
    not_available_features <- feature.list[!feature.list %in% rownames(sce)]
    message("Some of the features specified are not available in the provided sce object!\n",
            "Feature(s) not found: ",
            paste(not_available_features, collapse = ", "))

    feature.list <- feature.list[feature.list %in% rownames(sce)]
    if (length(feature.list) == 0)
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
  ## TODO: do we need this to be enforced as list?
  # feature.list <- as.list(feature.list)
  # clusters <- as.character(clusters)
  # groups <- as.character(groups)


  for (j in feature.list) {
    initial[[paste0("ReducedDimensionPlot", which(feature.list == j))]] <- new(
      "ReducedDimensionPlot",
      Type = reddim.type,
      ColorBy = "Feature name",
      ColorByFeatureName = j,
      ColorByFeatureSource = paste0("RowDataTable", which(feature.list == j)),
      ColumnSelectionSource = "ColumnDataPlot1",
      SelectionAlpha = 0.05
    )

    initial[[paste0("FeatureAssayPlot", which(feature.list == j))]] <- new(
      "FeatureAssayPlot",
      XAxis = "Column data",
      XAxisColumnData = clusters,
      YAxisFeatureName = j,
      YAxisFeatureSource = paste0("RowDataTable", which(feature.list == j)),
      ColorBy = "Column data",
      ColorByColumnData = clusters
    )

    initial[[paste0("RowDataTable", which(feature.list == j))]] <- new(
      "RowDataTable",
      Selected = j,
      Search = j
    )
  }

  if (length(feature.list) > 1) {
    initial[[paste0("FeatureAssayPlot", length(feature.list) + 1)]] <- new(
      "FeatureAssayPlot",
      XAxis = "Feature name",
      XAxisFeatureName = feature.list[[1]],
      YAxisFeatureName = feature.list[[2]]
    )
  }

  initial[[paste0("ReducedDimensionPlot", length(feature.list) + 1)]] <- new(
    "ReducedDimensionPlot",
    Type = reddim.type,
    ColorByColumnData = clusters,
    ColorBy = "Column data",
    ColumnSelectionSource = paste0("FeatureAssayPlot", length(feature.list) + 1),
    FacetColumnBy = "Column data",
    FacetColumnByColData = groups,
    SelectionAlpha = 0.05
  )


  initial[["ComplexHeatmapPlot1"]] <- new(
    "ComplexHeatmapPlot",
    CustomRowsText = paste(feature.list, collapse = "\n"),
    ColumnData = clusters
  )

  initial[["ColumnDataPlot1"]] <- new(
    "ColumnDataPlot",
    YAxis = clusters,
    ColorBy = "Column data",
    ColorByColumnData = clusters,
    PanelWidth = 6L
  )

  if (markdownboard == TRUE) {
    initial[["MarkdownBoard1"]] <- new(
      "MarkdownBoard",
      Content = "# Placeholder\n\nFill me with text!",
      PanelWidth = 4L)
  }

  if(dynamicMarkerTable == TRUE) {
    initial[[paste0("ReducedDimensionPlot",length(feature.list)+2)]] <- new(
      "ReducedDimensionPlot",
      Type = reddim.type,
      ColorByColumnData = clusters,
      ColorBy = "Column data",
      SelectionAlpha = 0.05,
      ColumnSelectionSource = paste0("FeatureAssayPlot",length(feature.list)+2))


    initial[["DynamicMarkerTable1"]] <- new(
      "DynamicMarkerTable",
      ColumnSelectionSource = paste0("ReducedDimensionPlot",length(feature.list)+2))

    initial[[paste0("FeatureAssayPlot",length(feature.list)+2)]] <- new(
      "FeatureAssayPlot",
      XAxis = "Column data",
      XAxisColumnData = groups,
      YAxisFeatureSource = "DynamicMarkerTable1")
  }

  return(initial)

}
