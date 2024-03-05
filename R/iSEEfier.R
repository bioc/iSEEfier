#' iSEEfier
#'
#' @param sce SingleCellExperiment object
#' @param feature.list A character vector containing a list of genes
#' @param reddim.type A string vector containing the dimensionality reduction type
#' @param clusters A character string containing the name of the clusters (as listed in the colData of the sce)
#' @param conditions A character string of the groups/conditions (as it appears in the colData of the sce)
#'
#' @return A list of "Panel" objects specifying the initial state of iSEE instance
#' @export
#'
#' @examples
#' sce <- 
#' gene_list <- c("IL7R", "CCR7", "B3GAT1", "PDCD1")
#' cluster <- "clusters"
#' condition <- "group"
#' initial <- iSEEfier(sce = sce, feature.list = gene_list, clusters = cluster, conditions = condition)
iSEEfier <- function(sce,
                     feature.list,
                     reddim.type = "TSNE",
                     clusters = colnames(colData(sce))[1],
                     conditions = colnames(colData(sce))[1]) {
  initial <- list()
  feature.list <- as.list(feature.list)
  clusters <- as.character(clusters)
  condition <- as.character(conditions)
  
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
                                                                               ColumnSelectionSource = "ColumnDataPlot1"
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
                                                                             FacetColumnByColData = condition
  )
  
  
  initial[["ComplexHeatmapPlot1"]] <- new("ComplexHeatmapPlot",
                                          CustomRowsText = paste(feature.list, collapse = "\n"),
                                          ColumnData = clusters
  )
  
  return(initial)
  
}