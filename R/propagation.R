#' @noRd
#' @title Report a check's result via message() on failure, returning
#'   whether it passed
#'
#' @param context `character(1)` The part of the failure message between
#'   the package name and the criterion's own message -- callers control
#'   this so gate and platform messages can each keep their own wording
#'   (e.g. `"-- gate 'vignettes'"` vs. `"on macos-arm64 -- 'build'"`).
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
#' @param pkg_data,branch,desc,views,repo Passed through to each gate.
#'
#' @return `logical(1)` -- TRUE only if every gate passed.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' gates <- default_criteria()$gates
#' .evaluate_gates(gates, pkg_data, "release", pkg_data, NULL, function() NULL)
.evaluate_gates <- function(gates, pkg_data, branch, desc, views, repo) {
    for (name in names(gates)) {
        result <- tryCatch(
            gates[[name]](pkg_data, branch, desc, views, repo),
            error = function(e) list(pass = FALSE, message = conditionMessage(e))
        )
        if (!.report_check(glue::glue("-- gate '{name}'"), pkg_data, result))
            return(FALSE)
    }
    TRUE
}

#' @noRd
#' @title Run platform criteria for one platform, stopping at the first
#'   failure
#'
#' @param criteria Named list of platform-criterion functions (see
#'   criteria.R).
#' @param pkg_data,branch,desc,views Passed through to each criterion.
#' @param platform `character(1)`, e.g. `"macos-arm64"`.
#'
#' @return `logical(1)` -- TRUE only if every criterion passed.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' criteria <- default_criteria()$platform
#' .evaluate_platform(criteria, pkg_data, "release", pkg_data, NULL, "macos-arm64")
.evaluate_platform <- function(criteria, pkg_data, branch, desc, views, platform) {
    for (name in names(criteria)) {
        result <- tryCatch(
            criteria[[name]](pkg_data, branch, desc, views, platform),
            error = function(e) list(pass = FALSE, message = conditionMessage(e))
        )
        context <- glue::glue("on {platform} -- '{name}'")
        if (!.report_check(context, pkg_data, result))
            return(FALSE)
    }
    TRUE
}

#' @title Check propagation status for a single package
#'
#' @description Queries the Bioconductor r-universe API for one package
#'   and reports, per requested OS-arch platform, whether it meets
#'   propagation criteria for a given branch. Registered gates (see
#'   [default_criteria()], [git_criteria()]) must all pass or every
#'   platform is `FALSE`. Evaluation is fail-fast (see
#'   `.evaluate_gates()`/`.evaluate_platform()`).
#'
#'   The return value is a plain `TRUE`/`FALSE` grid with no diagnostic.
#'   Maintainer must check GitHub Action log to diagnose.
#'
#' @param pkgName `character(1)` Package name.
#' @param branch `character(1)` A Bioc status tag (`"release"`, `"devel"`)
#'   or explicit version (e.g. `"3.22"`).
#' @param criteria A criteria list from [default_criteria()] (includes
#'   [git_criteria()]'s gates by default), optionally modified via
#'   [register_criterion()].
#' @param platforms `character()` OS-arch strings to check. The result's
#'   `platforms` element is built directly from this argument.
#' @param views A `data.frame` with `Package`/`Version` columns giving the
#'   currently-published Bioconductor version, or `NULL` (no VIEWS
#'   available).
#' @param repo_path `character(1)` or `NULL`. An already-cloned repo path.
#'   If `NULL`, cloned internally on first use by a git-based gate, if any
#'   is registered.
#'
#' @return `list(package, gates, platforms)`: `gates` is `logical(1)`;
#'   `platforms` is a named list of `logical(1)`, one per requested
#'   platform.
#'
#' @examplesIf interactive()
#' check_propagation("rtracklayer", branch = "devel", views = my_views)
#'
#' @export
check_propagation <- function(
    pkgName,
    branch = "release",
    criteria = default_criteria(),
    platforms = .DEFAULT_PLATFORMS,
    views = NULL,
    repo_path = NULL
) {
    pkg_data <- .get_runi_meta(pkgName)
    desc <- pkg_data
    repo <- .make_repo_accessor(pkg_data, branch, repo_path)

    gate_pass <- .evaluate_gates(
        criteria[["gates"]], pkg_data, branch, desc, views, repo
    )

    platform_results <- stats::setNames(
        lapply(platforms, function(p) {
            if (!gate_pass)
                FALSE
            else
                .evaluate_platform(
                    criteria[["platform"]], pkg_data, branch, desc, views, p
                )
        }),
        platforms
    )

    list(package = pkgName, gates = gate_pass, platforms = platform_results)
}
