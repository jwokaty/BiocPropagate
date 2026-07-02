.KNOWN_STATUSES <- c("ERROR", "FAIL", "CANCELLED", "WARNING", "NOTE", "OK")

#' @title Check the status of a package in the R-Universe
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

.validate_status <- function(statuses) {
    if (!all(statuses %in% .KNOWN_STATUSES))
        warning(
            "Invalid status found in r-universe checks: ",
            paste(statuses, collapse = ", "),
            call. = FALSE
        )
}

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
