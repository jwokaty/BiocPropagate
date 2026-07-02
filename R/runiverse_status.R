.KNOWN_STATUSES <- c("ERROR", "FAIL", "CANCELLED", "WARNING", "NOTE", "OK")

#' @title Check if package builds/checks are successful in R-Universe
#'
#' @description This function checks the status of a package in the R-Universe.
#'   It returns `TRUE` if at least one build/check has a non-failure status
#'   (i.e., not `"ERROR"`, `"FAIL"`, or `"CANCELLED"`).
#'
#' @param pkgName `character(1)` The name of the package to query in the
#'   R-Universe.
#'
#' @return `logical(1)` `TRUE` if at least one build/check status is not
#'   `"ERROR"`, `"FAIL"`, or `"CANCELLED"`. Returns `FALSE` otherwise.
#'
#' @examplesIf interactive()
#' runiverse_ok("BiocCheck")
#' @export
runiverse_ok <- function(pkgName) {
    results <- runiverse_status(pkgName)

    statuses <- results[["check"]] |>
        unlist() |>
        unname()

    .validate_status(statuses)

    any(
        !statuses %in% c("ERROR", "FAIL", "CANCELLED")
    )
}

#' @noRd
#' @title Validate build/check statuses
#'
#' @description Check that the input statuses are recognized. Issues a warning
#'   if any unrecognized status is found.
#'
#' @param statuses `character` A vector of status names to validate.
#'
#' @return `NULL` (invisibly). Used for side-effects.
.validate_status <- function(statuses) {
    if (!all(statuses %in% .KNOWN_STATUSES))
        warning(
            "Invalid status found in r-universe checks: ",
            paste(statuses, collapse = ", "),
            call. = FALSE
        )
}

#' @title Retrieve R-Universe package job details
#'
#' @description Query the Bioconductor R-Universe API for the jobs associated
#'   with a package. The function retrieves the job list, filters it to match
#'   the current R version used by Bioconductor, removes auxiliary checks (e.g.,
#'   `"bioc-checks"`, `"wasm-release"`), and filters out platforms specified as
#'   unsupported in the package's `DESCRIPTION` file.
#'
#' @param pkgName `character(1)` The name of the package to query in the
#'   R-Universe.
#'
#' @return A `data.frame` of filtered jobs from R-Universe, representing the
#'   build and check statuses across different platforms.
#'
#' @examplesIf interactive()
#' runiverse_status("BiocCheck")
#' @export
runiverse_status <- function(pkgName) {
    rver <- BiocManager:::.version_field("R")
    rver[, 3L] <- 0L

    if (nzchar(system.file(package = pkgName)))
        desc <- .get_sys_desc(pkgName)
    else
        desc <- .get_gh_desc(pkgName)

    glue::glue(
        .BIOC_UNIVERSE_URL, "/{pkgName}"
    ) |>
        rjsoncons::j_pivot(
            path = "_jobs[]", as = "data.frame"
        ) |>
        .filter_r_ver() |>
        .filter_other_checks() |>
        .filter_unsupported(desc)
}
