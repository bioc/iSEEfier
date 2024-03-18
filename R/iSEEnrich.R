#' iSEEnrich
#'
#' `iSEEnrich()` creates an initial state of an iSEE instance for interactive exploration of feature sets extracted from GO and KEGG database,
#' displaying all associated genes in a `RowDataTable` panel.
#'
#' @param sce SingleCellExperiment object
#' @param collection A character vector specifying the gene set collections of interest (GO,KEGG)
#' @param organism A character string of the org.*.eg.db package to use to extract mappings of gene sets to gene IDs.
#' @param gene_identifier A character string specifying the identifier to use to extract gene IDs for the organism package
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
#' sce <- scRNAseq::RichardTCellData()
#' sce <- scuttle::logNormCounts(sce)
#' sce <- scater::runPCA(sce)
#' GO_collection <- "GO"
#' Mm_organism <- "org.Mm.eg.db"
#' gene_id <- "SYMBOL"
#' results <- iSEEnrich(sce = sce,
#'                      collection = GO_collection,
#'                      organism = Mm_organism,
#'                      gene_identifier = gene_id)
#'
iSEEnrich <- function(sce,
                      collection = c("GO", "KEGG"),
                      organism = "org.Hs.eg.db",
                      gene_identifier = "ENTREZID") {

  ## Checks on arguments
  if (!is(sce, "SingleCellExperiment"))
    stop("Please provide a SingleCellExperiment as input!")

  stopifnot(is.character(collection))
  stopifnot(length(collection) == 1)

  collection <- match.arg(collection, c("GO", "KEGG"))

  stopifnot(is.character(organism))
  stopifnot(length(organism) == 1)

  stopifnot(is.character(gene_identifier))
  stopifnot(length(gene_identifier) == 1)


  initial <- list()

  cmds <- createGeneSetCommands(collections = collection,
                                organism = organism,
                                identifier = gene_identifier)
  sce1 <- registerFeatureSetCommands(sce, cmds)

  initial[["FeatureSetTable1"]] <- FeatureSetTable()

  initial[["RowDataTable1"]] <- RowDataTable(RowSelectionSource="FeatureSetTable1")

  return(
    list(
      initial = initial,
      sce = sce1
    )
  )
}
