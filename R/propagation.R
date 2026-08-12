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
#' @param pkg_data,branch,bioc_pkg_data,repo Passed through to each
#'   gate.
#'
#' @return `logical(1)` -- TRUE only if every gate passed.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' gates <- default_criteria()$gates
#' .evaluate_gates(gates, pkg_data, "release", NULL, function() NULL)
.evaluate_gates <- function(gates, pkg_data, branch, bioc_pkg_data, repo) {
    for (name in names(gates)) {
        result <- tryCatch(
            gates[[name]](pkg_data, branch, bioc_pkg_data, repo),
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
#' @param pkg_data,branch,bioc_pkg_data Passed through to each
#'   criterion.
#' @param platform `character(1)`, e.g. `"macos-arm64"`.
#'
#' @return `logical(1)` -- TRUE only if every criterion passed.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' criteria <- default_criteria()$platform
#' .evaluate_platform(criteria, pkg_data, "release", NULL, "macos-arm64")
.evaluate_platform <- function(criteria, pkg_data, branch, bioc_pkg_data, platform) {
    for (name in names(criteria)) {
        result <- tryCatch(
            criteria[[name]](pkg_data, branch, bioc_pkg_data, platform),
            error = function(e) list(pass = FALSE, message = conditionMessage(e))
        )
        context <- glue::glue("on {platform} -- '{name}'")
        if (!.report_check(context, pkg_data, result))
            return(FALSE)
    }
    TRUE
}

#' @noRd
#' @title Core propagation evaluation, given pkg_data already in hand
#'
#' @details Shared by [check_propagation()] and
#'   [check_propagation_from_results()].
#'
#' @return `list(package, gates, platforms)` -- see [check_propagation()].
.evaluate_propagation <- function(
    pkg, pkg_data, branch, criteria, platforms, bioc_pkg_data, repo_path
) {
    repo <- .make_repo_accessor(pkg_data, branch, repo_path)

    gate_pass <- .evaluate_gates(
        criteria[["gates"]], pkg_data, branch, bioc_pkg_data, repo
    )

    platform_results <- stats::setNames(
        lapply(platforms, function(p) {
            if (!gate_pass)
                FALSE
            else
                .evaluate_platform(
                    criteria[["platform"]], pkg_data, branch, bioc_pkg_data, p
                )
        }),
        platforms
    )

    list(package = pkg, gates = gate_pass, platforms = platform_results)
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
#' @param pkg `character(1)` Package name.
#' @param branch `character(1)` A Bioc status tag (`"release"`, `"devel"`)
#'   or explicit version (e.g. `"3.22"`).
#' @param criteria A criteria list from [default_criteria()] (includes
#'   [git_criteria()]'s gates by default), optionally modified via
#'   [register_criterion()].
#' @param platforms `character()` OS-arch strings to check. The result's
#'   `platforms` element is built directly from this argument.
#' @param bioc_pkg_data `list(source, platform)` -- see
#'   `.get_all_bioc_pkg_data()` -- or `NULL` (the default) to fetch it
#'   internally. A caller that already has this loaded (e.g.
#'   `biocPropagationPipe`) should pass it directly to skip the redundant
#'   fetch.
#' @param bioc_pkg_data_type `character(1)` or `NULL` (default) to
#'   auto-detect via [.detect_bioc_pkg_type()]. Passed to
#'   [.get_all_bioc_pkg_data()] when
#'   `bioc_pkg_data` isn't supplied.
#' @param repo_path `character(1)` or `NULL`. An already-cloned repo path.
#'   If `NULL`, cloned internally on first use by a git-based gate, if any
#'   is registered.
#'
#' @return `list(package, gates, platforms)`: `gates` is `logical(1)`;
#'   `platforms` is a named list of `logical(1)`, one per requested
#'   platform.
#'
#' @examplesIf interactive()
#' check_propagation("rtracklayer", branch = "devel")
#'
#' @export
check_propagation <- function(
    pkg,
    branch = "release",
    criteria = default_criteria(),
    platforms = .DEFAULT_PLATFORMS,
    bioc_pkg_data = NULL,
    bioc_pkg_data_type = NULL,
    repo_path = NULL
) {
    if (is.null(bioc_pkg_data))
        bioc_pkg_data <- if (identical(bioc_pkg_data_type, "books"))
            list(source = .get_books_packages(branch))
        else
            .get_all_bioc_pkg_data(branch, pkg, bioc_pkg_data_type)

    pkg_data <- .get_runi_meta(pkg)

    .evaluate_propagation(
        pkg, pkg_data, branch, criteria, platforms, bioc_pkg_data,
        repo_path
    )
}

#' @title Check propagation status from pre-supplied build results
#'
#' @description For same-run use inside a build workflow
#'
#' @param repo_path `character(1)` Path to the package's already
#'   checked-out source director.
#' @param jobs A data.frame with `config`, `r`, `check` columns -- one
#'   row for `"source"`, one per platform actually tested (e.g.
#'   `"linux-devel-x86_64"`).
#' @param universe `character(1)` r-universe universe name
#' @param binaries A data.frame with `os`/`arch` columns, or `NULL`
#'   (default) if unavailable. If supplied, `criteria` defaults to
#'   [default_criteria()] instead of [default_results_criteria()].
#' @param criteria A criteria list. Defaults to [default_results_criteria()]
#'   (no `binary` check) if `binaries` is `NULL`, or [default_criteria()]
#'   (includes it) if `binaries` is supplied.
#' @param platforms `character()` OS-arch strings to check.
#' @param bioc_pkg_data `list(source, platform)`, or `NULL` (the default)
#'   to fetch internally -- same as [check_propagation()].
#' @param bioc_pkg_data_type `character(1)` or `NULL` (default) to
#'   auto-detect via [.detect_bioc_pkg_type()]. Passed to
#'   [.get_all_bioc_pkg_data()] when `bioc_pkg_data` isn't supplied.
#'
#' @return `list(package, gates, platforms)` -- same shape as
#'   [check_propagation()].
#'
#' @examplesIf interactive()
#' # repo_path must contain a real DESCRIPTION file -- not runnable
#' # against the synthetic fixtures used elsewhere in this package's
#' # examples.
#' jobs <- .example_pkg_data(needs_compilation = TRUE)[["_jobs"]]
#' check_propagation_from_results(
#'     repo_path = "/path/to/checked-out/pkg",
#'     jobs = jobs,
#'     universe = "bioc-release"
#' )
#'
#' @export
check_propagation_from_results <- function(
    repo_path,
    jobs,
    universe,
    binaries = NULL,
    criteria = if (is.null(binaries)) default_results_criteria() else default_criteria(),
    platforms = .DEFAULT_PLATFORMS,
    bioc_pkg_data = NULL,
    bioc_pkg_data_type = NULL
) {
    pkg_data <- as.list(read.dcf(file.path(repo_path, "DESCRIPTION"))[1, ])
    pkg_data[["_jobs"]] <- jobs
    if (!is.null(binaries))
        pkg_data[["_binaries"]] <- binaries

    pkg <- pkg_data[["Package"]]
    branch <- .universe_to_branch(universe)

    if (is.null(bioc_pkg_data))
        if (identical(bioc_pkg_data_type, "books"))
            bioc_pkg_data <- list(source = .get_books_packages(branch))
        else
            bioc_pkg_data <- .get_all_bioc_pkg_data(branch, pkg, bioc_pkg_data_type)

    .evaluate_propagation(
        pkg, pkg_data, branch, criteria, platforms, bioc_pkg_data,
        repo_path
    )
}
