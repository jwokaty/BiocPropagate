#' Individual file size limit
#' @noRd
.MAX_FILE_SIZE_BYTES <- 5 * 1024^2

#' @noRd
#' @title Gate: no file in the source package exceeds the 5MB per-file
#'   limit
#'
#' @details Scans `source_path`'s current files only
#'
#' @param pkg_data,branch,bioc_pkg_data Unused; present for gate-signature
#'   consistency (see criteria.R).
#' @param source_path `character(1)` Path to the extracted source
#'   package.
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
.check_no_large_files <- function(pkg_data, branch, bioc_pkg_data, source_path) {
    files <- list.files(source_path, recursive = TRUE, full.names = TRUE)
    if (!length(files))
        return(list(pass = TRUE, message = NA_character_))

    sizes <- file.size(files)
    over <- sizes > .MAX_FILE_SIZE_BYTES
    if (!any(over, na.rm = TRUE))
        return(list(pass = TRUE, message = NA_character_))

    offending <- files[over]
    head_offending <- paste(utils::head(offending, 5L), collapse = ", ")
    list(pass = FALSE, message = glue::glue(
        "{length(offending)} file(s) exceed ",
        "{.MAX_FILE_SIZE_BYTES / 1024^2}MB: {head_offending}"
    ))
}

#' @noRd
#' @title Gate: DESCRIPTION does not declare a `Remotes:` field
#'
#' @param pkg_data DESCRIPTION-equivalent metadata; source of `Remotes`.
#' @param branch,bioc_pkg_data,source_path Unused; present for
#'   gate-signature consistency (see criteria.R).
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
#'
#' @examples
#' .check_no_remotes(list(Remotes = NULL), "devel", NULL, NULL)
#' .check_no_remotes(list(Remotes = "github::user/pkg"), "devel", NULL, NULL)
#' .check_no_remotes(
#'     list(Remotes = c("github::user/pkg1", "gitlab::user/pkg2")),
#'     "devel", NULL, NULL
#' )
.check_no_remotes <- function(pkg_data, branch, bioc_pkg_data, source_path) {
    remotes <- tryCatch(pkg_data[["Remotes"]], error = function(e) NULL)

    if (is.null(remotes) || !length(remotes))
        return(list(pass = TRUE, message = NA_character_))

    remotes <- remotes[!is.na(remotes) & nzchar(remotes)]
    if (!length(remotes))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "DESCRIPTION declares Remotes: {paste(remotes, collapse = ', ')}"
    ))
}

#' @noRd
#' @title Gate: no unresolved merge-conflict markers in the source
#'   package's files
#'
#' @param pkg_data,branch,bioc_pkg_data Unused; present for gate-signature
#'   consistency.
#' @param source_path `character(1)` Path to the extracted source
#'   package.
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
.check_no_merge_conflicts <- function(pkg_data, branch, bioc_pkg_data, source_path) {
    files <- list.files(source_path, recursive = TRUE, full.names = TRUE)
    pattern <- "^(<{7}|={7}|>{7})( |$)"

    hits <- character(0)
    for (f in files) {
        lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
        if (any(grepl(pattern, lines)))
            hits <- c(hits, f)
    }

    if (!length(hits))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "{length(hits)} file(s) contain unresolved merge-conflict markers: ",
        "{paste(utils::head(hits, 5L), collapse = ', ')}"
    ))
}

#' Non-exhaustive, heuristic patterns for commonly-leaked secret formats
#' @noRd
.SECRET_PATTERNS <- c(
    aws_access_key_id  = "AKIA[0-9A-Z]{16}",
    aws_secret_key     = "(?i)aws_secret_access_key[[:space:]]*[:=][[:space:]]*['\"]?[A-Za-z0-9/+=]{40}",
    github_token       = "gh[pousr]_[A-Za-z0-9]{36,}",
    slack_token        = "xox[baprs]-[A-Za-z0-9-]{10,}",
    private_key_header = "-----BEGIN (RSA |EC |OPENSSH |DSA )?PRIVATE KEY-----",
    generic_api_key    = "(?i)(api[_-]?key|secret|password)[[:space:]]*[:=][[:space:]]*['\"][A-Za-z0-9_/+=-]{16,}['\"]"
)

#' @noRd
#' @title Gate: no file in the source package matches a known secret
#'   pattern
#'
#' @details Scans `source_path`'s current file contents only -- no git
#'   history involved. A secret added and later removed in a prior commit
#'   is no longer caught; only what's actually present now is checked.
#'
#' @param pkg_data,branch,bioc_pkg_data Unused; present for gate-signature
#'   consistency.
#' @param source_path `character(1)` Path to the extracted source
#'   package.
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
.check_no_secrets <- function(pkg_data, branch, bioc_pkg_data, source_path) {
    files <- list.files(source_path, recursive = TRUE, full.names = TRUE)

    hit_names <- character(0)
    for (f in files) {
        lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
        if (!length(lines))
            next
        matches <- names(.SECRET_PATTERNS)[vapply(
            .SECRET_PATTERNS,
            function(pattern) any(grepl(pattern, lines, perl = TRUE)),
            logical(1L)
        )]
        hit_names <- union(hit_names, matches)
    }

    if (!length(hit_names))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "possible secret(s) detected, matching pattern(s): ",
        "{paste(hit_names, collapse = ', ')} -- if real, rotate the ",
        "credential(s) immediately"
    ))
}

#' @title Source-package-based propagation gates
#'
#' @description Included in [default_criteria()] automatically. Call this
#'   directly only if you want these gates on their own.
#'
#' @return A list with one element, `gates`, a named list of criterion
#'   functions.
#'
#' @examples
#' names(source_criteria()$gates)
#'
#' @export
source_criteria <- function() {
    list(
        gates = list(
            no_large_files     = .check_no_large_files,
            no_remotes         = .check_no_remotes,
            no_secrets         = .check_no_secrets,
            no_merge_conflicts = .check_no_merge_conflicts
        )
    )
}
