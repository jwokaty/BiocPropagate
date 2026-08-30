#' @noRd
#' @title Report a check's result via message() on failure, returning
#'   whether it passed
#'
#' @param context `character(1)` The part of the failure message between
#'   the package name and the criterion's own message.
#' @param pkg_data The r-universe payload; source of the package name.
#' @param result `list(pass, message)` from a criterion function.
#'
#' @return `logical(1)` -- TRUE if `result$pass` was TRUE. FALSE
#'   otherwise, having already emitted a `message()` explaining why.
.report_check <- function(context, pkg_data, result) {
    if (isTRUE(result[["pass"]]))
        return(TRUE)

    message(glue::glue(
        "propagation check failed for {pkg_data[['Package']]} ",
        "{context}: {result[['message']]}"
    ))
    FALSE
}

#' @noRd
#' @title Run gates in order, stopping at the first failure
#'
#' @param gates Named list of gate functions (see criteria.R).
#' @param pkg_data,branch,bioc_pkg_data Passed through to each gate.
#' @param source_path `character(1)` Path to the extracted source
#'   package.
#'
#' @return `logical(1)` -- TRUE only if every gate passed.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' gates <- default_criteria()$gates["vignettes"]
#' .evaluate_gates(gates, pkg_data, "release", NULL, NULL)
.evaluate_gates <- function(gates, pkg_data, branch, bioc_pkg_data, source_path) {
    for (name in names(gates)) {
        result <- tryCatch(
            gates[[name]](pkg_data, branch, bioc_pkg_data, source_path),
            error = function(e) list(pass = FALSE, message = conditionMessage(e))
        )
        if (!.report_check(glue::glue("-- gate '{name}'"), pkg_data, result))
            return(FALSE)
    }
    TRUE
}

#' @noRd
#' @title Run platform criteria for one `_jobs` row, stopping at the first
#'   failure
#'
#' @param criteria Named list of platform-criterion functions (see
#'   criteria.R).
#' @param pkg_data,branch,bioc_pkg_data Passed through to each criterion.
#' @param row One row of `_jobs`.
#'
#' @return `logical(1)` -- TRUE only if every criterion passed.
.evaluate_platform <- function(criteria, pkg_data, branch, bioc_pkg_data, row) {
    for (name in names(criteria)) {
        result <- tryCatch(
            criteria[[name]](pkg_data, branch, bioc_pkg_data, row),
            error = function(e) list(pass = FALSE, message = conditionMessage(e))
        )
        context <- glue::glue("on {row[['config']]} -- '{name}'")
        if (!.report_check(context, pkg_data, result))
            return(FALSE)
    }
    TRUE
}

#' @title Check propagation status for a package
#'
#' @description Reads a package's `_jobs` build-check results plus
#'   its `DESCRIPTION` from `source_path`, and annotates each job with whether
#'   it's safe to propagate; if any gate fails, every row gets `deploy = FALSE`.
#'   Each row is checked independently against Bioconductor's
#'   currently-published data for that platform. Previously propagated
#'   artifacts will be FALSE on subsequent checks for that platform-version.
#'
#' @param args A named list:
#'   * `package` -- `character(1)` package name.
#'   * `universe` -- `character(1)` r-universe universe name		
#'   * `jobs` -- a data.frame with `config`, `r`, `check` columns
#'   * `source_path` -- `character(1)` path to the extracted source
#'   * `criteria` -- optional; a criteria list from [default_criteria()]
#'
#' @return `args$jobs`, with an added `deploy` column (`logical`) --
#'   `TRUE`/`FALSE` per row.
#'
#' @export
check_propagation <- function(args) {
    package <- args[["package"]]
    universe <- args[["universe"]]
    jobs <- args[["jobs"]]
    source_path <- args[["source_path"]]
    criteria <- args[["criteria"]]
    if (is.null(criteria))
        criteria <- default_criteria()
    branch <- .universe_to_branch(universe)
    # criteria <- .remove_criteria(package, branch, criteria)
    pkg_data <- as.list(read.dcf(file.path(source_path, "DESCRIPTION"))[1, ])
    pkg_data[["_jobs"]] <- jobs
    bioc_pkg_data <- .get_all_bioc_pkg_data(branch, package)

    gate_pass <- .evaluate_gates(
        criteria[["gates"]], pkg_data, branch, bioc_pkg_data, source_path
    )
    jobs[["propagate"]] <- vapply(seq_len(nrow(jobs)), function(i) {
        row <- jobs[i, ]
        parsed <- .parse_job_config(row[["config"]])
        if (is.na(parsed$os) || !parsed$os %in% .PROPAGATED_OS)
            return(NA)
        if (!identical(parsed$r_channel, branch))
            return(NA)
        if (!gate_pass)
            return(FALSE)
        .evaluate_platform(criteria[["platform"]], pkg_data, branch, bioc_pkg_data, row)
    }, logical(1L))
    jobs
}
