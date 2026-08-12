#' Base repository path
#' @noRD
.BASE_REPO_PATH <- "https://github.com/bioc/"

#' Individual file size limit
#' @noRd
.MAX_FILE_SIZE_BYTES <- 5 * 1024^2

#' @noRd
#' @title Resolve a clone URL for a package's repository
#'
#' @param pkg_data named list 
#'
#' @return `character(1)` clone URL.
#'
#' @examples
#' .repo_clone_url(list(Package = "BiocCheck"))
.repo_clone_url <- function(pkg_data) {
    glue::glue("{.BASE_REPO_PATH}/{pkg_data[['Package']]}")
}

#' @noRd
#' @title Clone a repository to a temp directory
#'
#' @details Always full history, one branch (`--single-branch`): several
#'   checks here need real history (a shallow clone would miss a file
#'   committed once and later removed), so there's no case where a
#'   partial clone is usable by every check sharing it.
#'
#' @param url `character(1)` Clone URL.
#' @param branch `character(1)` or `NULL`. Clone only this branch. Without
#'   it, a clone falls back to the repo's default branch
#'
#' @return `character(1)` path to the cloned repo.
#'
#' @examplesIf curl::has_internet()
#' dest <- .clone_repo("https://git.bioconductor.org/packages/BiocCheck")
#' list.files(dest)
.clone_repo <- function(url, branch = NULL) {
    if (is.null(url))
        stop("no clone URL available")

    dest <- file.path(
        tempdir(),
        paste0("biocpropagate-", basename(url), "-", as.integer(Sys.time()))
    )
    args <- c("clone", "--quiet")
    if (!is.null(branch))
        args <- c(args, "--branch", branch, "--single-branch")
    args <- c(args, url, dest)

    status <- system2("git", args)
    if (!identical(status, 0L))
        stop(glue::glue("git clone failed for {url}"))

    dest
}

#' @noRd
#' @title Build a lazy, cached repo-path accessor
#'
#' @param pkg_data The r-universe package payload.
#' @param branch `character(1)` Target Bioconductor branch.
#' @param repo_path `character(1)` or `NULL`. An already-cloned path to
#'   use instead of cloning.
#'
#' @return A zero-argument function; calling it returns `character(1)`, a
#'   local repo path.
#'
#' @examplesIf curl::has_internet()
#' pkg_data <- list(Package = "BiocCheck")
#' repo <- .make_repo_accessor(pkg_data, branch = NULL)
#' repo()  # clones on first call
#' identical(repo(), repo())  # cached, no second clone
.make_repo_accessor <- function(pkg_data, branch, repo_path = NULL) {
    cache <- new.env(parent = emptyenv())
    cache$path <- repo_path
    function() {
        if (is.null(cache$path))
            cache$path <- .clone_repo(.repo_clone_url(pkg_data[["Package"]]), branch)
        cache$path
    }
}

#' @noRd
#' @title Gate: no individual file in git history exceeds the 5MB
#'   per-file limit
#'
#' @details Scans history (`git rev-list --objects` piped through
#'   `git cat-file --batch-check`), not just the working tree, to catch a
#'   large file that was later deleted but still bloats the repo.
#'
#' @param pkg_data,branch,bioc_pkg_data Unused; present for gate-signature
#'   consistency (see criteria.R).
#' @param repo Zero-arg accessor from `.make_repo_accessor()`.
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
#'
#' @examplesIf curl::has_internet()
#' pkg_data <- list(Package = "BiocCheck")
#' repo <- .make_repo_accessor(pkg_data, branch = NULL)
#' .check_no_large_files(list(), NULL, NULL, repo)
.check_no_large_files <- function(pkg_data, branch, bioc_pkg_data, repo) {
    path <- repo()

    batch_check <- shQuote("%(objecttype) %(objectname) %(objectsize) %(rest)")
    cmd <- glue::glue(
        "git -C {shQuote(path)} rev-list --objects HEAD ",
        "| git -C {shQuote(path)} cat-file --batch-check={batch_check}"
    )
    out <- tryCatch(
        system2("sh", c("-c", shQuote(cmd)), stdout = TRUE, stderr = FALSE),
        error = function(e) NULL
    )
    if (is.null(out) || !length(out))
        return(list(pass = FALSE, message = "failed to scan git object history"))

    parts <- strsplit(out, " ", fixed = TRUE)
    is_blob <- vapply(
        parts, function(x) length(x) >= 3L && x[1L] == "blob", logical(1L)
    )
    blobs <- parts[is_blob]
    if (!length(blobs))
        return(list(pass = TRUE, message = NA_character_))

    sizes <- vapply(blobs, function(x) as.numeric(x[3L]), numeric(1L))
    paths <- vapply(
        blobs,
        function(x) if (length(x) >= 4L) paste(x[-(1:3)], collapse = " ") else NA_character_,
        character(1L)
    )

    over <- sizes > .MAX_FILE_SIZE_BYTES
    if (!any(over))
        return(list(pass = TRUE, message = NA_character_))

    offending <- unique(stats::na.omit(paths[over]))
    head_offending <- paste(utils::head(offending, 5L), collapse = ", ")
    list(pass = FALSE, message = glue::glue(
        "{length(offending)} file(s) in git history exceed ",
        "{.MAX_FILE_SIZE_BYTES / 1024^2}MB: {head_offending}"
    ))
}

#' LFS pointer files are small, fixed-format text blobs, always starting
#' with this exact line. Real pointer files are ~130 bytes; the size
#' cutoff below is a generous margin, used only to cheaply narrow which
#' blobs' content is worth inspecting.
#' @noRd
.LFS_POINTER_SIGNATURE <- "version https://git-lfs.github.com/spec/v1"
.LFS_POINTER_MAX_SIZE <- 200

#' @noRd
#' @title Gate: repository does not declare git-lfs filters or contain
#'   orphaned LFS pointer blobs in history
#'
#' @details Two checks: (1) cheap -- `.gitattributes` currently declaring
#'   `filter=lfs`; (2) if that's clean, scans history for the pointer-file
#'   signature itself, to catch LFS usage where `.gitattributes` was later
#'   removed but the pointer blobs are still in history.
#'
#' @param pkg_data,branch,bioc_pkg_data Unused; present for gate-signature
#'   consistency (see criteria.R).
#' @param repo accessor from `.make_repo_accessor()`.
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
#'
#' @examplesIf curl::has_internet()
#' pkg_data <- list(Package= "BiocCheck")
#' repo <- .make_repo_accessor(pkg_data, branch = NULL)
#' .check_no_git_lfs(list(), NULL, NULL, repo)
.check_no_git_lfs <- function(pkg_data, branch, bioc_pkg_data, repo) {
    path <- repo()
    attrs_file <- file.path(path, ".gitattributes")

    if (file.exists(attrs_file)) {
        lines <- readLines(attrs_file, warn = FALSE)
        if (any(grepl("filter=lfs", lines, fixed = TRUE)))
            return(list(
                pass = FALSE,
                message = ".gitattributes declares git-lfs filters"
            ))
    }

    # %(rest) is required, not optional: rev-list --objects emits "<sha>
    # <path>" lines, and without %(rest) in the batch-check format, git
    # tries to resolve the *whole line* (sha and path together) as one
    # object spec, fails, and reports every object as "missing".
    batch_check <- shQuote("%(objecttype) %(objectname) %(objectsize) %(rest)")
    check_cmd <- glue::glue(
        "git -C {shQuote(path)} rev-list --objects HEAD ",
        "| git -C {shQuote(path)} cat-file --batch-check={batch_check}"
    )
    out <- tryCatch(
        system2("sh", c("-c", shQuote(check_cmd)), stdout = TRUE, stderr = FALSE),
        error = function(e) NULL
    )
    if (is.null(out) || !length(out))
        return(list(pass = FALSE, message = "failed to scan git object history"))

    parts <- strsplit(out, " ", fixed = TRUE)
    is_candidate <- vapply(parts, function(x)
        length(x) >= 3L && x[1L] == "blob" &&
            as.numeric(x[3L]) <= .LFS_POINTER_MAX_SIZE,
        logical(1L)
    )
    candidate_shas <- vapply(parts[is_candidate], `[`, character(1L), 2L)
    if (!length(candidate_shas))
        return(list(pass = TRUE, message = NA_character_))

    for (sha in candidate_shas) {
        content <- tryCatch(
            system2(
                "git", c("-C", shQuote(path), "cat-file", "-p", sha),
                stdout = TRUE, stderr = TRUE
            ),
            error = function(e) NULL
        )
        if (length(content) && startsWith(content[1L], .LFS_POINTER_SIGNATURE))
            return(list(
                pass = FALSE,
                message = "git-lfs pointer blob(s) found in git history"
            ))
    }

    list(pass = TRUE, message = NA_character_)
}

#' @noRd
#' @title Gate: DESCRIPTION does not declare a `Remotes:` field
#'
#' @param pkg_data DESCRIPTION-equivalent metadata; source of `Remotes`.
#' @param branch,bioc_pkg_data,repo Unused; present for gate-signature
#'   consistency (see criteria.R).
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
#'
#' @examples
#' .check_no_remotes(list(Remotes = NULL), "devel", NULL, function() NULL)
#' .check_no_remotes(list(Remotes = "github::user/pkg"), "devel", NULL,
#'   function() NULL)
#' .check_no_remotes(
#'     list(Remotes = c("github::user/pkg1", "gitlab::user/pkg2")),
#'     "devel", NULL, function() NULL
#' )
.check_no_remotes <- function(pkg_data, branch, bioc_pkg_data, repo) {
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
#' @title Gate: no unresolved merge-conflict markers committed to tracked
#'   files
#'
#' @param pkg_data,branch,bioc_pkg_data Unused; present for gate-signature
#'   consistency.
#' @param repo accessor from `.make_repo_accessor()`.
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
#'
#' @examplesIf curl::has_internet()
#' pkg_data <- list(Package = "BiocCheck")
#' repo <- .make_repo_accessor(pkg_data, branch = NULL)
#' .check_no_merge_conflicts(list(), NULL, NULL, repo)
.check_no_merge_conflicts <- function(pkg_data, branch, bioc_pkg_data, repo) {
    path <- repo()
    pattern <- "^(<{7}|={7}|>{7})( |$)"

    result <- suppressWarnings(system2(
        "git",
        c("-C", shQuote(path), "grep", "-lE", shQuote(pattern)),
        stdout = TRUE, stderr = TRUE
    ))
    status <- attr(result, "status")
    status <- if (is.null(status)) 0L else status

    if (status > 1L)
        return(list(
            pass = FALSE,
            message = "git grep failed while scanning for conflict markers"
        ))

    if (status == 1L || !length(result))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "{length(result)} file(s) contain unresolved merge-conflict markers: ",
        "{paste(utils::head(result, 5L), collapse = ', ')}"
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
#' @title Gate: no commit adds a line matching a known secret pattern
#'
#' @param pkg_data,branch,bioc_pkg_data Unused; present for gate-signature
#'   consistency.
#' @param repo accessor from `.make_repo_accessor()`.
#'
#' @return `list(pass = logical(1), message = character(1))`; `message`
#'   is `NA_character_` on pass.
#'
#' @examplesIf curl::has_internet()
#' pkg_data <- list(Package = "BiocCheck")
#' repo <- .make_repo_accessor(pkg_data, branch = NULL)
#' .check_no_secrets(list(), NULL, NULL, repo)
.check_no_secrets <- function(pkg_data, branch, bioc_pkg_data, repo) {
    path <- repo()

    out <- tryCatch(
        system2(
            "git", c("-C", shQuote(path), "log", "HEAD", "-p"),
            stdout = TRUE, stderr = FALSE
        ),
        error = function(e) NULL
    )
    if (is.null(out))
        return(list(pass = FALSE,
                    message = "failed to scan commit diffs for secrets"))

    added_lines <- out[startsWith(out, "+") & !startsWith(out, "+++")]
    if (!length(added_lines))
        return(list(pass = TRUE, message = NA_character_))

    hit_names <- names(.SECRET_PATTERNS)[vapply(
        .SECRET_PATTERNS,
        function(pattern) any(grepl(pattern, added_lines, perl = TRUE)),
        logical(1L)
    )]

    if (!length(hit_names))
        return(list(pass = TRUE, message = NA_character_))

    list(pass = FALSE, message = glue::glue(
        "possible secret(s) detected in commit history, matching pattern(s): ",
        "{paste(hit_names, collapse = ', ')} -- if real, rotate the ",
        "  credential(s) immediately and scrub git history"
    ))
}

#' @title Git-history-based propagation gates
#'
#' @description Included in [default_criteria()] automatically. Call this
#'   directly only if you want the git-based gates on their own, or to
#'   opt out of them elsewhere:
#'
#'   ```r
#'   criteria <- default_criteria()
#'   criteria$gates[names(git_criteria()$gates)] <- NULL
#'   ```
#'
#' @return A list with one element, `gates`, a named list of criterion
#'   functions.
#'
#' @examples
#' names(git_criteria()$gates)
#'
#' @export
git_criteria <- function() {
    list(
        gates = list(
            no_large_files     = .check_no_large_files,
            no_git_lfs         = .check_no_git_lfs,
            no_remotes         = .check_no_remotes,
            no_secrets         = .check_no_secrets,
            no_merge_conflicts = .check_no_merge_conflicts
        )
    )
}
