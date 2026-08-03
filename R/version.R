#' @noRd
#' @title Look up a version-map field for an explicit Bioconductor branch
#'
#' @param field `character(1)` Column to pull, e.g. `"R"`, `"Bioc"`, or
#'   `"BiocStatus"`.
#' @param branch `character(1)` A Bioc status tag (`"release"`, `"devel"`,
#'   `"out-of-date"`, `"future"`) or an explicit Bioc version (e.g.
#'   `"3.22"`).
#'
#' @return The value in `field` for the matched row.
#'
#' @examples
#' .version_field_for("R", "release")
#' .version_field_for("Bioc", "devel")
.version_field_for <- function(field, branch) {
    map <- BiocManager:::.version_map()
    if (identical(map, BiocManager:::.VERSION_MAP_SENTINEL))
        stop("could not build BiocManager's version map (check network access)")

    status_tags <- c("out-of-date", "release", "devel", "future")
    if (branch %in% status_tags)
        idx <- match(branch, map[["BiocStatus"]])
    else
        idx <- match(package_version(branch), map[["Bioc"]])

    if (is.na(idx))
        stop(glue::glue("branch '{branch}' not found in BiocManager's ",
            "version map"))

    map[idx, field]
}

#' @noRd
#' @title Get the major.minor R version paired with a Bioconductor branch
#'
#' @param branch `character(1)`
#'
#' @return A `package_version`, major.minor only (e.g. `4.6`).
#'
#' @examples
#' .branch_r_version("release")
.branch_r_version <- function(branch) {
    .major_minor(.version_field_for("R", branch))
}

#' @noRd
#' @title Resolve a branch tag or version to the Bioc version number
#'
#' @param branch `character(1)`
#'
#' @return A `package_version`, e.g. `3.24`.
#'
#' @examples
#' .branch_bioc_version("devel")
.branch_bioc_version <- function(branch) {
    package_version(.version_field_for("Bioc", branch))
}

#' @noRd
#' @title Truncate a version to its major.minor components
#'
#' @param v Anything coercible via `package_version()`, e.g. `"4.6.1"`.
#'
#' @return A `package_version` with only the first two components.
#'
#' @examples
#' .major_minor("4.6.1")
.major_minor <- function(v) {
    v <- package_version(v)
    package_version(paste(v[, 1L], v[, 2L], sep = "."))
}
