## Preparing up the datasets to use for the test suite
message("--- Loading packages...")
suppressPackageStartupMessages({
  library("scRNAseq")
  library("scuttle")
  library("scater")
})
message("- Done!")

message("--- Preparing datasets...")
sce_allen <- scRNAseq::ReprocessedAllenData(assays = "tophat_counts")
sce_allen <- scuttle::logNormCounts(sce_allen, exprs_values="tophat_counts")
sce_allen <- scater::runPCA(sce_allen)
sce_allen <- scater::runTSNE(sce_allen)

# sce_bacher <- scRNAseq::BacherTCellData()
# sce_bacher <- scuttle::logNormCounts(sce_bacher)
# sce_bacher <- scater::runPCA(sce_bacher)

message("- Done!!")
