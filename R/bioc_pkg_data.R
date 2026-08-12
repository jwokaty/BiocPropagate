#' Repo-path segment for each Bioconductor package type, matching
#' `BiocManager::repositories()`'s own naming.
#' @noRd
.PACKAGE_TYPE_PATH <- c(
    software = "bioc",
    "data-annotation" = "data/annotation",
    "data-experiment" = "data/experiment",
    workflows = "workflows"
)

#' @noRd
#' @title Fetch current Bioconductor package version data for a branch
#'   and type
#'
#' @details Uses `available.packages()` -- the same mechanism
#'   `install.packages()`/`BiocManager::install()` use internally to read
#'   a repository's standard `PACKAGES` index. `filters = list()`
#'   disables the default R-version-compatibility filtering, which
#'   otherwise assumes the *running* R session's version rather than the
#'   branch's paired version we're actually asking about.
#'
#' @param branch `character(1)` A Bioc status tag or explicit version.
#' @param type `character(1)` One of `names(.PACKAGE_TYPE_PATH)`.
#'
#' @return A `data.frame` with `Package` and `Version` columns (plus
#'   whatever else `available.packages()` returns), one row per package.
#'
#' @examplesIf curl::has_internet()
#' pkg_data <- .get_bioc_pkg_data("release")
#' head(pkg_data[c("Package", "Version")])
.get_bioc_pkg_data <- function(branch, type = "software") {
    type <- match.arg(type, names(.PACKAGE_TYPE_PATH))
    bioc_ver <- .branch_bioc_version(branch)
    path <- .PACKAGE_TYPE_PATH[[type]]
    contrib_url <- glue::glue(
        "https://bioconductor.org/packages/{bioc_ver}/{path}/src/contrib"
    )
    as.data.frame(
        utils::available.packages(contriburl = contrib_url, filters = list()),
        stringsAsFactors = FALSE
    )
}

#' @noRd
#' @title Fetch Package/Version pairs for Bioconductor Books
#'
#' @param branch `character(1)` A Bioc status tag or explicit version.
#'
#' @return A `data.frame` with `Package` and `Version` columns.
#'
#' @examplesIf curl::has_internet()
#' books <- .get_books_packages("release")
#' head(books[c("Package", "Version")])
.get_books_packages <- function(branch) {
    bioc_ver <- .branch_bioc_version(branch)
    contrib_url <- glue::glue(
        "https://bioconductor.org/packages/{bioc_ver}/books/src/contrib"
    )
    as.data.frame(
        utils::available.packages(contriburl = contrib_url, filters = list()),
        stringsAsFactors = FALSE
    )
}

#' @noRd
#' @title Resolve the Bioc version immediately before a branch's
#'
#' @details Bioc versions increment y by 1 per release (e.g. 3.23 ->
#'   3.24); computed arithmetically rather than via
#'   `BiocManager:::.version_map()`'s row order, which isn't something
#'   this package has verified.
#'
#' @param branch `character(1)` A Bioc status tag or explicit version.
#'
#' @return `character(1)`, e.g. `"3.23"` given a branch resolving to
#'   `"3.24"`.
.previous_bioc_version <- function(branch) {
    bioc_ver <- .branch_bioc_version(branch)
    paste(as.integer(bioc_ver[, 1L]), as.integer(bioc_ver[, 2L]) - 1L, sep = ".")
}

#' macOS codename per R version and arch -- R-core's choice of which
#' macOS baseline each release's binaries target, not derivable from the
#' version number. Confirmed for R 4.6 from real URLs; add an entry each
#' time a new R version needs one.
#' @noRd
.MACOS_CODENAMES <- list(
    "4.7" = c(arm64 = "sonoma", x86_64 = "big-sur"),
    "4.6" = c(arm64 = "sonoma", x86_64 = "big-sur"),
    "4.5" = c(arm64 = "big-sur", x86_64 = "big-sur")
)

#' @noRd
#' @title Fetch Bioconductor's published version data for one binary
#'   platform
#'
#' @details Linux has no separate binary repo (source is what r-universe
#'   compares against there) -- only call this for windows/macos.
#'
#' @param os `"windows"` or `"macos"`.
#' @param arch Required for `"macos"`; ignored for `"windows"` (one
#'   shared binary, no arch split).
#' @param codenames Override for `.MACOS_CODENAMES`.
#'
#' @return A `data.frame` with `Package`/`Version` columns.
.get_bioc_platform_pkg_data <- function(
    branch, type = "software", os, arch = NULL, codenames = .MACOS_CODENAMES
) {
    type <- match.arg(type, names(.PACKAGE_TYPE_PATH))
    bioc_ver <- .branch_bioc_version(branch)
    path <- .PACKAGE_TYPE_PATH[[type]]
    rver <- as.character(.branch_r_version(branch))

    bin_path <- if (identical(os, "windows")) {
        glue::glue("bin/windows/contrib/{rver}")
    } else if (identical(os, "macos")) {
        codename <- codenames[[rver]][[arch]]
        if (is.null(codename))
            stop(sprintf(
                "no macOS codename known for R %s/%s -- update .MACOS_CODENAMES",
                rver, arch
            ))
        glue::glue("bin/macosx/{codename}-{arch}/contrib/{rver}")
    } else {
        stop(sprintf("unsupported os: %s", os))
    }

    contrib_url <- glue::glue(
        "https://bioconductor.org/packages/{bioc_ver}/{path}/{bin_path}"
    )
    as.data.frame(
        utils::available.packages(contriburl = contrib_url, filters = list()),
        stringsAsFactors = FALSE
    )
}

#' @noRd
#' @title Determine which Bioconductor repository type a package belongs
#'   to, by checking which one actually contains it
#'
#' @details No package-type input is available in
#'   `check_propagation_from_results()`'s context -- tries each type in
#'   turn (software first, since that's most packages) and returns the
#'   first whose data actually lists the package.
#'
#' @return `character(1)` type name, or `NA_character_` if not found in
#'   any of them (e.g. a genuinely new, not-yet-published package).
.detect_bioc_pkg_type <- function(branch, pkgName) {
    for (type in names(.PACKAGE_TYPE_PATH)) {
        data <- tryCatch(.get_bioc_pkg_data(branch, type), error = function(e) NULL)
        if (!is.null(data) && pkgName %in% data[["Package"]])
            return(type)
    }
    NA_character_
}

#' @noRd
#' @title Fetch all package data needed for propagation checks, once
#'
#' @details Eager, not lazy -- every default platform set needs
#'   windows/macos-arm64/macos-x86_64 anyway, so there's no case where
#'   deferring the fetch saves anything.
#'
#' @param type `character(1)` or `NULL` (default) to auto-detect via
#'   `.detect_bioc_pkg_type()`.
#'
#' @return `list(source = <data.frame>, platform = list(windows = ...,
#'   "macos-arm64" = ..., "macos-x86_64" = ...))`. If `type` can't be
#'   detected, `source` is `NULL` and `platform` is empty -- every
#'   criterion reading this then sees "nothing to compare against",
#'   handled the same way as a brand-new package already is.
.get_all_bioc_pkg_data <- function(branch, pkgName, type = NULL) {
    if (is.null(type))
        type <- .detect_bioc_pkg_type(branch, pkgName)
    if (is.na(type))
        return(list(source = NULL, platform = list()))

    list(
        source = .get_bioc_pkg_data(branch, type),
        platform = list(
            windows = .get_bioc_platform_pkg_data(branch, type, os = "windows"),
            "macos-arm64" = .get_bioc_platform_pkg_data(
                branch, type, os = "macos", arch = "arm64"
            ),
            "macos-x86_64" = .get_bioc_platform_pkg_data(
                branch, type, os = "macos", arch = "x86_64"
            )
        )
    )
}

#' @noRd
#' @title Look up a package's published version from package version data
#'
#' @param bioc_pkg_data A `data.frame` with `Package`/`Version` columns,
#'   or `NULL`.
#' @param pkgName `character(1)`
#'
#' @return `character(1)` The published version, or `NA` if
#'   `bioc_pkg_data` is `NULL` or the package isn't in it (e.g. a
#'   brand-new package).
#'
#' @examples
#' bioc_pkg_data <- data.frame(Package = "foo", Version = "1.2.0")
#' .lookup_bioc_pkg_version(bioc_pkg_data, "foo")
#' .lookup_bioc_pkg_version(bioc_pkg_data, "bar")
#' .lookup_bioc_pkg_version(NULL, "foo")
.lookup_bioc_pkg_version <- function(bioc_pkg_data, pkgName) {
    if (is.null(bioc_pkg_data))
        return(NA_character_)
    idx <- match(pkgName, bioc_pkg_data[["Package"]])
    if (is.na(idx))
        return(NA_character_)
    bioc_pkg_data[["Version"]][idx]
}
