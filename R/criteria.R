#' Default os-arch columns evaluated when `platforms` isn't specified.
#' @noRd
.DEFAULT_PLATFORMS <- c(
    "windows-arm64", "windows-x86_64",
    "macos-arm64", "macos-x86_64",
    "linux-arm64", "linux-x86_64"
)

#' Build/check statuses that count as passing. Only ERROR/FAIL/CANCELLED
#' are failures.
#' @noRd
.PASSING_STATUSES <- c("OK", "WARNING", "NOTE")

#' r-universe's `_binaries[].os` uses different tokens than
#' `_jobs[].config` (e.g. "mac" vs "macos"). Maps platform-string os
#' tokens to the corresponding `_binaries[].os` value.
#' @noRd
.BINARY_OS_MAP <- c(macos = "mac", windows = "win", linux = "linux")

#' @noRd
#' @title Test-fixture helper: a minimal r-universe payload
#'
#' @param needs_compilation `logical(1)`.
#'
#' @return A named list shaped like a real r-universe package payload.
.example_pkg_data <- function(needs_compilation = FALSE) {
    r_ver <- paste0(as.character(.branch_r_version("release")), ".0")
    binaries <- data.frame(
        r = r_ver,
        os = c("mac", "win", "win", "linux"),
        status = "success",
        check = "OK",
        stringsAsFactors = FALSE
    )
    if (needs_compilation)
        binaries[["arch"]] <- c("aarch64", "x86_64", "aarch64", "x86_64")

    list(
        Package = "examplePkg",
        Version = "1.2.0",
        NeedsCompilation = if (needs_compilation) "yes" else "no",
        `_jobs` = data.frame(
            config = c(
                "source", "bioc-checks",
                "macos-release-arm64",
                "windows-release-x86_64", "windows-release-arm64",
                "linux-release-x86_64"
            ),
            r = r_ver,
            check = "OK",
            stringsAsFactors = FALSE
        ),
        `_binaries` = binaries
    )
}

#' @noRd
#' @title Split a platform string like "macos-x86_64" into os and arch
#'
#' @param platform `character(1)`, e.g. `"macos-x86_64"`.
#'
#' @return `list(os, arch)`.
#'
#' @examples
#' .parse_platform("macos-x86_64")
.parse_platform <- function(platform) {
    parts <- strsplit(platform, "-", fixed = TRUE)[[1L]]
    list(os = parts[1L], arch = paste(parts[-1L], collapse = "-"))
}

#' @noRd
#' @title Does this package need compilation?
#'
#' @details Uses `NeedsCompilation` to determine if package needs
#'   compilation. If not present, checks binaries.
#'
#' @param pkg_data The r-universe payload (`NeedsCompilation`, and
#'   `_binaries` as a fallback).
#'
#' @return `logical(1)`.
#'
#' @examples
#' .needs_compilation(list(NeedsCompilation = "yes"))
#' .needs_compilation(list(`_binaries` = data.frame(arch = "x86_64")))
#' .needs_compilation(list())
.needs_compilation <- function(pkg_data) {
    declared <- pkg_data[["NeedsCompilation"]]
    if (!is.null(declared) && !is.na(declared) && nzchar(declared))
        return(identical(declared, "yes"))

    binaries <- pkg_data[["_binaries"]]
    if (!is.null(binaries) && NROW(binaries))
        return("arch" %in% colnames(binaries))

    TRUE
}

#' Config values in `_jobs` that are gate-level checks, not platform
#' (OS-arch) builds.
#' @noRd
.GATE_JOB_CONFIGS <- c(
    vignettes   = "source",
    bioc_checks = "bioc-checks"
)

#' @noRd
#' @title Shared implementation for gates keyed on a single `_jobs` config
#'   value
#'
#' @details Not filtered by branch/R version: r-universe reports one job
#'   per gate config, run against whichever R version happened to build
#'   it. Treated as branch-independent.
#'
#' @param pkg_data The r-universe payload.
#' @param config_name `character(1)`, e.g. `"source"`.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' .check_gate_job(.example_pkg_data(), "source")
.check_gate_job <- function(pkg_data, config_name, branch = NULL) {
    jobs <- pkg_data[["_jobs"]]
    row <- jobs[jobs[["config"]] == config_name, ]

    if (!is.null(branch)) {
        rver <- .branch_r_version(branch)
        row <- row[.major_minor(row[["r"]]) == rver, ]
    }

    if (!nrow(row))
        return(list(
            pass = FALSE,
            message = glue::glue("no '{config_name}' job found")
        ))

    if (all(row[["check"]] %in% .PASSING_STATUSES))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "'{config_name}' job status: ",
        "{paste(unique(row[['check']]), collapse = ', ')}"
    ))
}

#' @noRd
#' @title Gate: did the vignette/source build ("source" job) pass?
#'
#' @param pkg_data,branch,bioc_pkg_data,repo See `.check_gate_job()`; only
#'   `pkg_data` is used.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' .check_vignettes(.example_pkg_data(), "release", NULL, function() NULL)
.check_vignettes <- function(pkg_data, branch, bioc_pkg_data, repo)
    .check_gate_job(pkg_data, .GATE_JOB_CONFIGS[["vignettes"]])

#' @noRd
#' @title Gate: BiocCheck compliance job ("bioc-checks") pass?
#'
#' @param pkg_data,branch,bioc_pkg_data,repo See `.check_gate_job()`; only
#'   `pkg_data` is used.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' .check_bioc_checks(.example_pkg_data(), "release", NULL, function() NULL)
.check_bioc_checks <- function(pkg_data, branch, bioc_pkg_data, repo)
    .check_gate_job(pkg_data, .GATE_JOB_CONFIGS[["bioc_checks"]], branch)

#' @noRd
#' @title Gate: is the package's version a valid propagation over what's
#'   currently published in Bioconductor?
#'
#' @details Valid `x.y.z` patterns, by who makes the change:
#'
#'   maintainer, any time:
#'   * z-bump: `x.y.z -> x.y.(z+n)`
#'   * major-change signal (devel only): `x.y.z -> x.99.z`
#'
#'   Bioconductor's own release process, twice a year:
#'   * devel's release-cycle bump: `x.y.z -> x.(y+2).0`
#'   * devel's major cutover (only if the *previous* `y` was `99`):
#'     `x.99.z -> (x+1).1.0`
#'   * release: gets a z-bump
#'
#' @param pkg_data The r-universe payload.
#' @param branch `character(1)` Bioc branch tag or version -- resolved to
#'   release/devel status, which determines which pattern set applies.
#' @param repo Unused.
#' @param bioc_pkg_data `list(source, platform)` -- see
#'   `.get_all_bioc_pkg_data()`. Uses `source` only.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' pkg_data[["Version"]] <- "1.2.5"
#' source_data <- data.frame(Package = "examplePkg", Version = "1.2.0")
#' bioc_pkg_data <- list(source = source_data)
#' .check_version_valid(pkg_data, "release", bioc_pkg_data, function() NULL)
.check_version_valid <- function(pkg_data, branch, bioc_pkg_data, repo) {
    current <- pkg_data[["Version"]]
    previous <- .lookup_bioc_pkg_version(bioc_pkg_data$source, pkg_data[["Package"]])

    if (is.null(current) || is.na(previous))
        return(list(pass = TRUE, message = NA_character_))

    cur <- package_version(current)
    prev <- package_version(previous)
    cur_x <- as.integer(cur[, 1L])
    cur_y <- as.integer(cur[, 2L])
    cur_z <- as.integer(cur[, 3L])

    prev_x <- as.integer(prev[, 1L])
    prev_y <- as.integer(prev[, 2L])
    prev_z <- as.integer(prev[, 3L])

    status <- .version_field_for("BiocStatus", branch)
    is_release <- identical(as.character(status), "release")

    same_x <- cur_x == prev_x
    same_xy <- same_x && cur_y == prev_y

    normal_bump <- same_xy && cur_z > prev_z

    if (is_release) {
        ok <- normal_bump
    } else {
        release_cycle_bump <- same_x && (cur_y == prev_y + 2L) && (cur_z == 0L)
        major_cutover_bump <- (prev_y == 99L) && (cur_x == prev_x + 1L) &&
            (cur_y == 1L) && (cur_z == 0L)
        signal_99 <- same_x && (cur_y == 99L)

        ok <- normal_bump || release_cycle_bump || major_cutover_bump || signal_99
    }

    if (ok)
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "version {current} is not a valid propagation over {previous}"
    ))
}

#' @noRd
#' @title Platform check: did R CMD check pass for this OS-arch, at the R
#'   version paired with `branch`?
#'
#' @param pkg_data The r-universe payload (also used for
#'   `.needs_compilation()`).
#' @param branch `character(1)` Bioc branch tag or version.
#' @param bioc_pkg_data Unused.
#' @param platform `character(1)`, e.g. `"macos-arm64"`.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' .check_build_status(pkg_data, "release", NULL, "macos-arm64")
.check_build_status <- function(pkg_data, branch, bioc_pkg_data, platform) {
    p <- .parse_platform(platform)
    jobs <- pkg_data[["_jobs"]]
    rver <- .branch_r_version(branch)
    matched_r <- jobs[.major_minor(jobs[["r"]]) == rver, ]

    row <- if (.needs_compilation(pkg_data)) {
        matched_r[
            startsWith(matched_r[["config"]], paste0(p$os, "-")) &
                endsWith(matched_r[["config"]], paste0("-", p$arch)),
        ]
    } else {
        # Pure-R packages get one job per OS (no arch split); any arch's
        # column maps onto that single job. Gate configs (e.g. "source",
        # "bioc-checks") and non-platform entries (e.g. "wasm-release")
        # never match an os-prefix like "macos-"/"windows-"/"linux-", so
        # they're naturally excluded here without needing an explicit
        # exclusion list.
        matched_r[startsWith(matched_r[["config"]], paste0(p$os, "-")), ]
    }

    if (!nrow(row))
        return(list(pass = FALSE, message = glue::glue(
            "no build job found for {platform} at R {rver}"
        )))

    if (all(row[["check"]] %in% .PASSING_STATUSES))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "{platform} build status: ",
        "{paste(unique(row[['check']]), collapse = ', ')}"
    ))
}

#' r-universe/BBS os tokens, normalized to the `platforms`-argument
#' vocabulary. Covers both full names and abbreviations
#' @noRd
.OS_ALIASES <- c(
    win = "windows", windows = "windows",
    mac = "macos", macos = "macos", macosx = "macos",
    linux = "linux"
)

#' @noRd
.ARCH_ALIASES <- c(
    x64 = "x86_64", x86_64 = "x86_64",
    arm64 = "arm64", aarch64 = "arm64",
    emscripten = "emscripten"
)

#' @noRd
#' @title Normalize a single os or arch token to the canonical vocabulary
#'
#' @param token `character(1)`, e.g. `"win"`.
#' @param aliases `.OS_ALIASES` or `.ARCH_ALIASES`.
#'
#' @return `character(1)`.
#'
#' @examples
#' .canonicalize_token("win", .OS_ALIASES)
.canonicalize_token <- function(token, aliases) {
    key <- tolower(token)
    if (key %in% names(aliases)) unname(aliases[[key]]) else key
}

#' @noRd
#' @title Parse one `Config/Bioconductor/UnsupportedPlatforms` entry
#'
#' @details Entries may be OS-only (`"win"` -- unsupported on every arch
#'   for that OS) or OS-arch (`"macosx-arm64"`). `arch = NA` means "any
#'   arch".
#'
#' @param entry `character(1)`, e.g. `"macosx-arm64"`.
#'
#' @return `list(os, arch)`.
#'
#' @examples
#' .parse_unsupported_entry("win")
#' .parse_unsupported_entry("macosx-arm64")
.parse_unsupported_entry <- function(entry) {
    parts <- strsplit(entry, "-", fixed = TRUE)[[1L]]
    os <- .canonicalize_token(parts[1L], .OS_ALIASES)
    if (length(parts) > 1L)
        arch <- .canonicalize_token(paste(parts[-1L], collapse = "-"),
                                    .ARCH_ALIASES)
    else
        arch <- NA_character_
    list(os = os, arch = arch)
}

#' @noRd
#' @title Platform check: is this platform NOT declared unsupported via
#'   `Config/Bioconductor/UnsupportedPlatforms`?
#'
#' @param pkg_data The r-universe payload; source of
#'   `Config/Bioconductor/UnsupportedPlatforms`.
#' @param branch Unused.
#' @param bioc_pkg_data Unused.
#' @param platform `character(1)`, e.g. `"windows-x86_64"`.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' pkg_data <- list(`Config/Bioconductor/UnsupportedPlatforms` = "win")
#' .check_supported_platform(pkg_data, "release", NULL, "windows-x86_64")
.check_supported_platform <- function(pkg_data, branch, bioc_pkg_data, platform) {
    desc_field <- "Config/Bioconductor/UnsupportedPlatforms"
    unsupplat <- pkg_data[[desc_field]]
    if (is.null(unsupplat) || is.na(unsupplat) || !nzchar(unsupplat))
        return(list(pass = TRUE, message = NA_character_))

    p <- .parse_platform(platform)
    p_os <- .canonicalize_token(p$os, .OS_ALIASES)
    p_arch <- .canonicalize_token(p$arch, .ARCH_ALIASES)

    entries <- strsplit(unsupplat, ",\\s*")[[1L]]
    matches <- vapply(entries, function(entry) {
        parsed <- .parse_unsupported_entry(entry)
        if (identical(parsed$os, "linux"))
            return(FALSE)  # linux is always supported
        identical(parsed$os, p_os) &&
            (is.na(parsed$arch) || identical(parsed$arch, p_arch))
    }, logical(1L))

    if (!any(matches))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "{platform} declared unsupported ({desc_field})"
    ))
}

#' @noRd
#' @title Platform check: was a binary produced and installable for this
#'   OS-arch, at the R version paired with `branch`?
#'
#' @details `arch` is present in every `_binaries` entry (wasm included --
#'   its one arch value is `"emscripten"`) when `NeedsCompilation` is
#'   `"yes"`, and absent from every entry (again including wasm) when it's
#'   `"no"`.
#'
#' @param pkg_data The r-universe payload (also used for
#'   `.needs_compilation()`).
#' @param branch `character(1)` Bioc branch tag or version.
#' @param bioc_pkg_data Unused.
#' @param platform `character(1)`, e.g. `"macos-arm64"`.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' .check_binary_status(pkg_data, "release", NULL, "macos-arm64")
.check_binary_status <- function(pkg_data, branch, bioc_pkg_data, platform) {
    p <- .parse_platform(platform)
    bin_os <- .BINARY_OS_MAP[[p$os]]
    binaries <- pkg_data[["_binaries"]]
    if (is.null(binaries) || !NROW(binaries))
        return(list(pass = FALSE, message = "no binaries reported"))

    rver <- .branch_r_version(branch)
    matched_r <- binaries[.major_minor(binaries[["r"]]) == rver, ]
    matched_os <- matched_r[matched_r[["os"]] == bin_os, ]

    row <- if (.needs_compilation(pkg_data)) {
        if (!"arch" %in% colnames(matched_os))
            return(list(pass = FALSE, message = glue::glue(
                "no {platform} binary found (missing arch column)"
            )))
        binary_arch <- vapply(
            matched_os[["arch"]], .canonicalize_token, character(1L),
            .ARCH_ALIASES
        )
        matched_os[binary_arch ==
            .canonicalize_token(p$arch, .ARCH_ALIASES), ]
    } else {
        # Pure-R packages omit `arch` entirely
        matched_os
    }

    if (!nrow(row))
        return(list(pass = FALSE, message = glue::glue(
            "no binary found for {platform} at R {rver}"
        )))

    status_ok <- all(row[["status"]] == "success")
    # wasm binaries have no `check` field; missing check is fine if
    # status is "success".
    check_ok <- all(is.na(row[["check"]]) | row[["check"]] %in% .PASSING_STATUSES)

    if (status_ok && check_ok)
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "{platform} binary status: ",
        "{paste(unique(row[['status']]), collapse = ', ')} ",
        "/ check: {paste(unique(row[['check']]), collapse = ', ')}"
    ))
}

#' @noRd
#' @title Platform check: does propagating this platform actually
#'   advance its published version?
#'
#' @details Strict `>`, `=` fails to prevent the same version of a package
#'   os-arch artifact from being propagated more than once. Falls back to
#'   the previous version if no current artifact found in Bioconductor.
#'
#' @param bioc_pkg_data `list(source, platform, previous)` -- `previous`
#'   optional, same shape as the top level (`list(source, platform)`).
#'   See `.get_all_bioc_pkg_data()`.
#'
#' @return `list(pass = logical(1), message = character(1))`.
#'
#' @examples
#' pkg_data <- .example_pkg_data()
#' bioc_pkg_data <- list(
#'     source = data.frame(Package = "examplePkg", Version = "1.1.0"),
#'     platform = list(windows = data.frame(Package = "examplePkg", Version = "1.1.0"))
#' )
#' .check_platform_version_valid(pkg_data, "release", bioc_pkg_data, "windows-x86_64")
.check_platform_version_valid <- function(pkg_data, branch, bioc_pkg_data, platform) {
    current <- pkg_data[["Version"]]
    if (is.null(current))
        return(list(pass = TRUE, message = NA_character_))

    p <- .parse_platform(platform)
    key <- if (identical(p$os, "windows")) "windows" else platform
    lookup_table <- if (identical(p$os, "linux"))
        bioc_pkg_data$source
    else
        bioc_pkg_data$platform[[key]]

    published <- .lookup_bioc_pkg_version(lookup_table, pkg_data[["Package"]])

    if (!is.na(published)) {
        if (package_version(current) > package_version(published))
            return(list(pass = TRUE, message = NA_character_))
        return(list(pass = FALSE, message = sprintf(
            "version %s is not greater than %s already published for %s",
            current, published, platform
        )))
    }

    prev_table <- if (!is.null(bioc_pkg_data$previous)) {
        if (identical(p$os, "linux"))
            bioc_pkg_data$previous$source
        else
            bioc_pkg_data$previous$platform[[key]]
    } else {
        prev_branch <- .previous_bioc_version(branch)
        if (identical(p$os, "linux"))
            .get_bioc_pkg_data(prev_branch)
        else if (identical(p$os, "windows"))
            .get_bioc_platform_pkg_data(prev_branch, os = "windows")
        else
            .get_bioc_platform_pkg_data(prev_branch, os = "macos", arch = p$arch)
    }

    prev_published <- .lookup_bioc_pkg_version(prev_table, pkg_data[["Package"]])
    if (is.na(prev_published))
        return(list(pass = TRUE, message = NA_character_))

    cur_y <- as.integer(package_version(current)[, 2L])
    prev_y <- as.integer(package_version(prev_published)[, 2L])

    if (cur_y > prev_y)
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = sprintf(
        "version %s does not show a valid y-increase over %s (previous branch) for %s",
        current, prev_published, platform
    ))
}

#' @title Default propagation criteria
#'
#' @description The default gate and platform criteria used by
#'   [check_propagation()]. To opt *out* of the git-based gates (e.g. to
#'   avoid the clone cost), remove them explicitly:
#'
#'   ```r
#'   criteria <- default_criteria()
#'   criteria$gates[names(git_criteria()$gates)] <- NULL
#'   ```
#'
#'   Copy and modify the returned list (via [register_criterion()]) to
#'   add, remove, or override criteria as propagation policy evolves.
#'
#' @return A list with two elements, `gates` and `platform`, each a named
#'   list of criterion functions.
#'
#' @examples
#' criteria <- default_criteria()
#' names(criteria$gates)
#' names(criteria$platform)
#'
#' @export
default_criteria <- function() {
    criteria <- list(
        gates = list(
            vignettes   = .check_vignettes,
            version     = .check_version_valid
        ),
        platform = list(
            build           = .check_build_status,
            binary          = .check_binary_status,
            unsupported     = .check_supported_platform,
            platform_version = .check_platform_version_valid
        )
    )
    criteria$gates <- c(criteria$gates, git_criteria()$gates)
    criteria
}

#' @title Default criteria for check_propagation_from_results()
#'
#' @description Identical to [default_criteria()], minus the `binary`
#'   platform check.
#'
#' @return A list with two elements, `gates` and `platform`, same shape
#'   as [default_criteria()].
#'
#' @examples
#' criteria <- default_results_criteria()
#' names(criteria$platform)
#' # "binary" is absent, unlike default_criteria()
#'
#' @export
default_results_criteria <- function() {
    criteria <- default_criteria()
    criteria$platform$binary <- NULL
    criteria
}

#' @title Register propagation criterion
#'
#' @description Add or replace a named criterion in a criteria list,
#'   without needing to know [default_criteria()]'s internal structure --
#'   the hook point for propagation policy that changes over time.
#'
#' @param criteria A criteria list, e.g. from [default_criteria()].
#' @param name `character(1)` Criterion name (replaces an existing one
#'   with the same name and `type`).
#' @param fun A function returning `list(pass = logical(1), message =
#'   character(1))`. For `type = "gates"`:
#'   `function(pkg_data, branch, bioc_pkg_data, repo)`. For
#'   `type = "platform"`: `function(pkg_data, branch, bioc_pkg_data,
#'   platform)`.
#' @param type `character(1)` Either `"gates"` or `"platform"`.
#'
#' @return The updated criteria list.
#'
#' @examples
#' criteria <- default_criteria()
#' criteria <- register_criterion(
#'     criteria, "always_pass",
#'     function(pkg_data, branch, bioc_pkg_data, repo)
#'         list(pass = TRUE, message = NA_character_),
#'     type = "gates"
#' )
#' names(criteria$gates)
#'
#' @export
register_criterion <- function(criteria, name, fun, type = c("gates", "platform")) {
    type <- match.arg(type)
    criteria[[type]][[name]] <- fun
    criteria
}
