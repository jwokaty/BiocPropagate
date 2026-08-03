#' A minimal local git repo with one commit, for tests that need
#' controllable content (a real Bioconductor repo won't have a
#' deliberately oversized file, a leaked secret, or a conflict marker
#' sitting in it -- see test-git_checks.R).
#'
#' @param files Named character(); names are file paths (relative),
#'   values are file contents.
#'
#' @return character(1) path to the repo.
.example_repo <- function(files = c(a.R = "x <- 1")) {
    path <- tempfile("repo-")
    dir.create(path)
    owd <- setwd(path)
    on.exit(setwd(owd))
    system2("git", c("init", "--quiet", "-b", "main"))
    system2("git", c("config", "user.email", "a@example.com"))
    system2("git", c("config", "user.name", "a"))
    for (f in names(files)) {
        dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
        writeLines(files[[f]], f)
    }
    system2("git", c("add", "."))
    system2("git", c("commit", "--quiet", "-m", "init"))
    path
}
