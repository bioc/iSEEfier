#' iSEEnrich
#'
#' @param sce 
#' @param collection 
#' @param organsim 
#'
#' @return
#' @export
#'
#' @examples
iSEEnrich <- function(sce, collection, organsim) {
  initial <- list()
  
  cmds <- createGeneSetCommands(collections= my_collection,
                                organism= my_organism,
                                identifier="SYMBOL")
  sce1 <- registerFeatureSetCommands(sce, cmds)
  
  initial[["FeatureSetTable1"]] <- FeatureSetTable()
  
  initial[["RowDataTable1"]] <- RowDataTable(RowSelectionSource="FeatureSetTable1")
  
  return(list(initial = initial, sce1 = sce1))
}