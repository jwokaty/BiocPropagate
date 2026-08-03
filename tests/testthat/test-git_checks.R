test_that(".repo_clone_url reads RemoteUrl", {
    expect_equal(
        .repo_clone_url(list(RemoteUrl = "https://example.com/x")),
        "https://example.com/x"
    )
    expect_null(.repo_clone_url(list()))
})

test_that(".clone_repo clones a local repo to a real, listable path", {
    src <- tempfile("src-")
    dir.create(src)
    owd <- setwd(src)
    on.exit(setwd(owd))
    system2("git", c("init", "--quiet", "-b", "main"))
    system2("git", c("config", "user.email", "a@example.com"))
    system2("git", c("config", "user.name", "a"))
    writeLines("x <- 1", "a.R")
    system2("git", c("add", "a.R"))
    system2("git", c("commit", "--quiet", "-m", "init"))
    setwd(owd)

    dest <- .clone_repo(src, branch = "main")
    expect_true(file.exists(file.path(dest, "a.R")))
})

test_that(".clone_repo errors when url is NULL", {
    expect_error(.clone_repo(NULL))
})

test_that(".make_repo_accessor returns a supplied path without cloning", {
    repo <- .make_repo_accessor(list(), "main", repo_path = tempdir())
    expect_equal(repo(), tempdir())
})

test_that(".make_repo_accessor caches across repeated calls", {
    repo <- .make_repo_accessor(list(), "main", repo_path = tempdir())
    expect_identical(repo(), repo())
})

test_that(".check_no_large_files passes a repo with only small files", {
    path <- .example_repo()
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_large_files(list(), "main", list(), NULL, repo)
    expect_true(result$pass)
})

test_that(".check_no_large_files fails a repo with a file over the limit", {
    big <- paste(rep("x", .MAX_FILE_SIZE_BYTES + 1L), collapse = "")
    path <- .example_repo(c(a.R = "x <- 1", "big.txt" = big))
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_large_files(list(), "main", list(), NULL, repo)
    expect_false(result$pass)
    expect_match(result$message, "big.txt")
})

test_that(".check_no_large_files fails closed when the scan itself fails", {
    not_a_repo <- tempfile("not-a-repo-")
    dir.create(not_a_repo)
    repo <- .make_repo_accessor(list(), "main", repo_path = not_a_repo)
    expect_warning(
        result <- .check_no_large_files(list(), "main", list(), NULL, repo),
        "had status"
    )
    expect_false(result$pass)
    expect_match(result$message, "failed to scan")
})

test_that(".check_no_git_lfs passes a repo without .gitattributes", {
    path <- .example_repo()
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_git_lfs(list(), "main", list(), NULL, repo)
    expect_true(result$pass)
})

test_that(".check_no_git_lfs fails a repo declaring filter=lfs", {
    path <- .example_repo(c(
        a.R = "x <- 1",
        ".gitattributes" = "*.bin filter=lfs diff=lfs merge=lfs -text"
    ))
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_git_lfs(list(), "main", list(), NULL, repo)
    expect_false(result$pass)
})

test_that(".check_no_git_lfs fails a repo with an orphaned pointer blob (no .gitattributes)", {
    pointer <- paste(
        "version https://git-lfs.github.com/spec/v1",
        "oid sha256:4d7a214614ab2935c943f9e0ff69d22eadbb8f32b1258daaa5e2ca24d17e2393",
        "size 12345",
        sep = "\n"
    )
    path <- .example_repo(c(a.R = "x <- 1", "data.bin" = pointer))
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_git_lfs(list(), "main", list(), NULL, repo)
    expect_false(result$pass)
    expect_match(result$message, "pointer blob")
})

test_that(".check_no_git_lfs ignores small non-pointer files", {
    path <- .example_repo(c(a.R = "x <- 1", "tiny.txt" = "ok"))
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_git_lfs(list(), "main", list(), NULL, repo)
    expect_true(result$pass)
})

test_that(".check_no_git_lfs fails closed when the scan itself fails", {
    not_a_repo <- tempfile("not-a-repo-")
    dir.create(not_a_repo)
    repo <- .make_repo_accessor(list(), "main", repo_path = not_a_repo)
    expect_warning(
        result <- .check_no_git_lfs(list(), "main", list(), NULL, repo),
        "had status"
    )
    expect_false(result$pass)
    expect_match(result$message, "failed to scan")
})

test_that(".check_no_remotes passes when Remotes is absent", {
    result <- .check_no_remotes(list(), "release", list(Remotes = NULL), NULL, function() NULL)
    expect_true(result$pass)
})

test_that(".check_no_remotes fails when Remotes is declared", {
    desc <- list(Remotes = "github::user/pkg")
    result <- .check_no_remotes(list(), "release", desc, NULL, function() NULL)
    expect_false(result$pass)
    expect_match(result$message, "github::user/pkg")
})

test_that(".check_no_merge_conflicts passes a clean repo", {
    path <- .example_repo()
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_merge_conflicts(list(), "main", list(), NULL, repo)
    expect_true(result$pass)
})

test_that(".check_no_merge_conflicts fails a repo with committed conflict markers", {
    path <- .example_repo(c(a.R = "<<<<<<< HEAD\nx <- 1\n=======\nx <- 2\n>>>>>>> other"))
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_merge_conflicts(list(), "main", list(), NULL, repo)
    expect_false(result$pass)
})

test_that(".check_no_secrets passes a repo without matching patterns", {
    path <- .example_repo()
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_secrets(list(), "main", list(), NULL, repo)
    expect_true(result$pass)
})

test_that(".check_no_secrets fails a repo with a matching pattern, and never leaks the match", {
    path <- .example_repo(c(a.R = 'token <- "AKIAABCDEFGHIJKLMNOP"'))
    repo <- .make_repo_accessor(list(), "main", repo_path = path)
    result <- .check_no_secrets(list(), "main", list(), NULL, repo)
    expect_false(result$pass)
    expect_match(result$message, "aws_access_key_id")
    expect_false(grepl("AKIAABCDEFGHIJKLMNOP", result$message, fixed = TRUE))
})

test_that("git_criteria returns the expected gate names", {
    criteria <- git_criteria()
    expect_named(criteria, "gates")
    expect_setequal(
        names(criteria$gates),
        c("no_large_files", "no_git_lfs", "no_remotes", "no_secrets", "no_merge_conflicts")
    )
})
