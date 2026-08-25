#' @noRd
.BIOC_BASE_URL <- "https://bioconductor.org"

#' Repo-path segment for each Bioconductor package type, matching
#' `BiocManager::repositories()`'s own naming.
#' @noRd
.PACKAGE_TYPE_PATH <- c(
    software = "bioc",
    "data-annotation" = "data/annotation",
    "data-experiment" = "data/experiment",
    workflows = "workflows",
    books = "books"
)

#' @noRd
#' @title Fetch current Bioconductor package version data for a branch
#'   and type
#'
#' @param branch `character(1)` A Bioc status tag or explicit version.
#' @param type `character(1)` One of `names(.PACKAGE_TYPE_PATH)`.
#'
#' @return A `data.frame` with `Package` and `Version` columns one, row per
#'   package.
#'
#' @examplesIf curl::has_internet()
#' pkg_data <- .get_bioc_pkg_data("release")
#' head(pkg_data[c("Package", "Version")])
.get_bioc_pkg_data <- function(branch, type = "software") {
    type <- match.arg(type, names(.PACKAGE_TYPE_PATH))
    bioc_ver <- .branch_bioc_version(branch)
    path <- .PACKAGE_TYPE_PATH[[type]]
    contrib_url <- glue::glue(
        "{.BIOC_BASE_URL}/packages/{bioc_ver}/{path}/src/contrib"
    )
    as.data.frame(
        utils::available.packages(contriburl = contrib_url, filters = list()),
        stringsAsFactors = FALSE
    )
}

#' @noRd
#' @title Resolve the Bioc version immediately before a branch's
#'
#' @param branch `character(1)` A Bioc status tag or explicit version.
#'
#' @return `character(1)`, e.g. `"3.22"` given a branch resolving to
#'   `"3.23"`.
.previous_bioc_version <- function(branch) {
    bioc_ver <- .branch_bioc_version(branch)
    config <- yaml::read_yaml(glue::glue("{.BIOC_BASE_URL}/config.yaml"))
    if (branch == "devel")
        versions <- config$versions
    else
        versions <- names(config$release_dates)
 
    idx <- match(bioc_ver, versions)
    if (is.na(idx) || idx <= 1L)
        stop(glue::glue(
            "no previous Bioc version available in config.yaml's ",
            "'versions' list for {bioc_ver}"
        ))

    as.character(versions[idx - 1L])
}

#' r-universe universe name -> Bioc branch.
#' @noRd
.UNIVERSE_BRANCH_MAP <- c(
    "bioc" = "devel",
    "bioc-release" = "release"
)

#' @noRd
#' @title Resolve an r-universe name to a Bioconductor branch
#'
#' @param universe `character(1)`, e.g. `"bioc-release"`.
#' @param map Override for `.UNIVERSE_BRANCH_MAP`.
#'
#' @return `character(1)`, e.g. `"release"`.
.universe_to_branch <- function(universe, map = .UNIVERSE_BRANCH_MAP) {
    tryCatch(map[[universe]],
        error = function(e) {
            stop(glue::glue("unknown universe '{universe}' -- update",
                            ".UNIVERSE_BRANCH_MAP", .sep = " "))
    })
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
            stop(glue::glue("no macOS codename known for R {rver}/{arch} --",
                            "update .MACOS_CODENAMES", .sep = " "))
        glue::glue("bin/macosx/{codename}-{arch}/contrib/{rver}")
    } else {
        stop(glue::glue("unsupported os: {os}"))
    }

    contrib_url <- glue::glue(
        "{.BIOC_BASE_URL}/packages/{bioc_ver}/{path}/{bin_path}"
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
#' @details tries each type in names(.PACKAGE_TYPE_PATH)
#'
#' @return `character(1)` type name, or `NA_character_` if not found in
#'   any of them (e.g. a genuinely new, not-yet-published package).
.detect_bioc_pkg_type <- function(branch, pkg) {
    for (type in names(.PACKAGE_TYPE_PATH)) {
        data <- tryCatch(.get_bioc_pkg_data(branch, type),
                         error = function(e) NULL)
        if (!is.null(data) && pkg %in% data[["Package"]])
            return(type)
    }

    NA_character_
}

#' @noRd
#' @title Fetch all package data needed for propagation checks, once
#'
#' @param branch release or devel
#' @param pkg package name
#'
#' @return `list(source = <data.frame>, platform = list(windows = ...,
#'   "macos-arm64" = ..., "macos-x86_64" = ...))`. If `type` can't be
#'   detected, `source` is `NULL` and `platform` is empty, it is handled the
#'   same way as a brand-new package.
.get_all_bioc_pkg_data <- function(branch, pkg) {
    type <- .detect_bioc_pkg_type(branch, pkg)
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
