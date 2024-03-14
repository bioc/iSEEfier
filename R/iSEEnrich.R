#' iSEEnrich
#' 
#' `iSEEnrich()` creates an initial state of an iSEE instance for interactive exploration of feature sets extracted from GO and KEGG database,
#' displaying all associated genes in a `RowDataTable` panel.
#'
#' @param sce SingleCellExperiment object
#' @param collection A character vector specifying the gene set collections of interest (GO,KEGG)
#' @param organism A character string of the org.*.eg.db package to use to extract mappings of gene sets to gene IDs.
#' @param gene_identifer A character string specifying the identifier to use to extract gene IDs for the organism package
#'
#' @return A list of "Panel" objects specifying the initial state of iSEE instance
#' @export iSEEnrich
#' @importFrom iSEE RowDataTable
#' @importFrom iSEEu createGeneSetCommands
#' @importFrom iSEEu registerFeatureSetCommands
#' @importFrom iSEEu FeatureSetTable
#' 
#'
#' @examples
#' sce <- scRNAseq::BacherTCellData()
#' sce <- scuttle::logNormCounts(sce)
#' sce <- scater::runPCA(sce)
#' GO_collection <- "GO"
#' Hs_organism <- "org.Hs.eg.db"
#' gene_id <- "SYMBOL"
#' results <- iSEEnrich(sce = sce, collection = GO_collection,
#' organism = Hs_organism, gene_identifer = gene_id)
#' 
iSEEnrich <- function(sce, collection = c("GO", "KEGG"),
                      organism = "org.Hs.eg.db",
                      gene_identifer = "ENTREZID") {
  initial <- list()
  
  cmds <- createGeneSetCommands(collections = collection,
                                organism = organism,
                                identifier = gene_identifer)
  sce1 <- registerFeatureSetCommands(sce, cmds)
  
  initial[["FeatureSetTable1"]] <- FeatureSetTable()
  
  initial[["RowDataTable1"]] <- RowDataTable(RowSelectionSource="FeatureSetTable1")
  
  return(list(initial = initial, sce1 = sce1))
}