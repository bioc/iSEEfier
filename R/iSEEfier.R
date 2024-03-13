#' iSEEfier: Create an initial state of an iSEE instance for gene expression visualization
#' 
#' `iSEEfier()` defines the initial setup of an iSEE instance, recommending tailored visual elements to effortlessly illustrate the expression of a gene list in a single view.
#'
#' @param sce SingleCellExperiment object
#' @param feature.list A character vector containing a list of genes
#' @param reddim.type A string vector containing the dimensionality reduction type
#' @param clusters A character string containing the name of the clusters/cell-type/state...(as listed in the colData of the sce)
#' @param groups A character string of the groups/conditions...(as it appears in the colData of the sce)
#'
#' @return A list of "Panel" objects specifying the initial state of iSEE instance
#' @export iSEEfier
#' @importFrom methods new
#' @importFrom SummarizedExperiment colData
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
#' initial <- iSEEfier(sce = sce, feature.list = gene_list, clusters = cluster, groups = group)
iSEEfier <- function(sce,
                     feature.list,
                     reddim.type = "TSNE",
                     clusters = colnames(colData(sce))[1],
                     groups = colnames(colData(sce))[1]) {
  initial <- list()
  feature.list <- as.list(feature.list)
  clusters <- as.character(clusters)
  group <- as.character(groups)
  
  initial[["ColumnDataPlot1"]] <- new("ColumnDataPlot",
                                      YAxis = clusters,
                                      ColorBy = "Column data",
                                      ColorByColumnData = clusters
  )
  
  for (j in feature.list) {
    initial[[paste0("ReducedDimensionPlot", which(feature.list == j))]] <- new("ReducedDimensionPlot",
                                                                               Type = reddim.type,
                                                                               ColorBy = "Feature name",
                                                                               ColorByFeatureName = j,
                                                                               ColumnSelectionSource = "ColumnDataPlot1",
                                                                               SelectionAlpha = 0.05
    )
    
    initial[[paste0("FeatureAssayPlot", which(feature.list == j))]] <- new("FeatureAssayPlot",
                                                                           XAxis = "Column data",
                                                                           XAxisColumnData = clusters,
                                                                           YAxisFeatureName = j,
                                                                           ColorBy = "Column data",
                                                                           ColorByColumnData = clusters
    )
    
    initial[[paste0("RowDataTable", which(feature.list == j))]] <- new("RowDataTable",
                                                                       Selected = j,
                                                                       Search = j
    )
  }
  
  if (length(feature.list) > 1) {
    initial[[paste0("FeatureAssayPlot", length(feature.list) + 1)]] <- new("FeatureAssayPlot", 
                                                                           XAxis = "Feature name", 
                                                                           XAxisFeatureName = feature.list[[1]],
                                                                           YAxisFeatureName = feature.list[[2]]
    )
  }
  
  initial[[paste0("ReducedDimensionPlot", length(feature.list) + 1)]] <- new("ReducedDimensionPlot",
                                                                             Type = reddim.type,
                                                                             ColorByColumnData = clusters,
                                                                             ColorBy = "Column data",
                                                                             ColumnSelectionSource = paste0("FeatureAssayPlot", length(feature.list) + 1),
                                                                             FacetColumnBy = "Column data",
                                                                             FacetColumnByColData = group,
                                                                             SelectionAlpha = 0.05
  )
  
  
  initial[["ComplexHeatmapPlot1"]] <- new("ComplexHeatmapPlot",
                                          CustomRowsText = paste(feature.list, collapse = "\n"),
                                          ColumnData = clusters
  )
  
  initial[["MarkdownBoard1"]] <- new("MarkdownBoard", Content = "# Placeholder\n\nFill me with text!",
                                     PanelWidth = 3L)
  
  return(initial)
  
}