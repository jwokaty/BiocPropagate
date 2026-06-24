.filter_unsupported <- function(results, desc) {
    desc_field <- "Config/Bioconductor/UnsupportedPlatforms"
    fields <- colnames(desc)
    if (!desc_field %in% fields)
        return(results)
    unsupplat <- desc[, desc_field]
    plats <- strsplit(unsupplat, ",\\s+")[[1L]] |>
        gsub("macosx", "macos", x = _)
    unsupported <- lapply(
        plats, startsWith, x = results[["config"]]
    ) |>
        Reduce(`|`, x = _)
    results[!unsupported, , drop = FALSE]
}

.filter_other_checks <- function(results) {
    other_checks <- c("bioc-checks", "wasm-release")
    results[!results[["config"]] %in% other_checks, , drop = FALSE]
}

.filter_r_ver <- function(results) {
    rver <- BiocManager:::.version_field("R")
    rver[, 3L] <- 0L
    results[results[["r"]] == rver, , drop = FALSE]
}
