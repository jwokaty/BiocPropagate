.make_source_dir <- function(files = list()) {
    dir <- tempfile("source-")
    dir.create(dir)
    for (name in names(files))
        writeLines(files[[name]], file.path(dir, name))
    dir
}

test_that(".check_no_large_files passes for small files", {
    dir <- .make_source_dir(list("a.R" = "x <- 1"))
    result <- .check_no_large_files(list(), NULL, NULL, dir)
    expect_true(result$pass)
})

test_that(".check_no_large_files fails when a file exceeds the limit", {
    dir <- .make_source_dir()
    big <- file.path(dir, "big.bin")
    writeBin(raw(.MAX_FILE_SIZE_BYTES + 1L), big)
    result <- .check_no_large_files(list(), NULL, NULL, dir)
    expect_false(result$pass)
    expect_match(result$message, "exceed")
})

test_that(".check_no_large_files passes for an empty directory", {
    dir <- .make_source_dir()
    result <- .check_no_large_files(list(), NULL, NULL, dir)
    expect_true(result$pass)
})

test_that(".check_no_remotes passes when Remotes is absent", {
    result <- .check_no_remotes(list(Remotes = NULL), "devel", NULL, NULL)
    expect_true(result$pass)
})

test_that(".check_no_remotes fails when Remotes is declared", {
    result <- .check_no_remotes(list(Remotes = "github::user/pkg"), "devel", NULL, NULL)
    expect_false(result$pass)
    expect_match(result$message, "github::user/pkg")
})

test_that(".check_no_remotes fails and lists all entries for multiple Remotes", {
    result <- .check_no_remotes(
        list(Remotes = c("github::user/pkg1", "gitlab::user/pkg2")), "devel", NULL, NULL
    )
    expect_false(result$pass)
    expect_match(result$message, "pkg1")
    expect_match(result$message, "pkg2")
})

test_that(".check_no_merge_conflicts passes for clean files", {
    dir <- .make_source_dir(list("a.R" = "x <- 1"))
    result <- .check_no_merge_conflicts(list(), NULL, NULL, dir)
    expect_true(result$pass)
})

test_that(".check_no_merge_conflicts fails when conflict markers are present", {
    dir <- .make_source_dir(list(
        "a.R" = c("x <- 1", "<<<<<<< HEAD", "y <- 2", "=======", "y <- 3", ">>>>>>> branch")
    ))
    result <- .check_no_merge_conflicts(list(), NULL, NULL, dir)
    expect_false(result$pass)
    expect_match(result$message, "a.R")
})

test_that(".check_no_secrets passes for clean files", {
    dir <- .make_source_dir(list("a.R" = "x <- 1"))
    result <- .check_no_secrets(list(), NULL, NULL, dir)
    expect_true(result$pass)
})

test_that(".check_no_secrets fails for an AWS access key pattern", {
    dir <- .make_source_dir(list("a.R" = "key <- 'AKIAABCDEFGHIJKLMNOP'"))
    result <- .check_no_secrets(list(), NULL, NULL, dir)
    expect_false(result$pass)
    expect_match(result$message, "aws_access_key_id")
})

test_that(".check_no_secrets fails for a private key header", {
    dir <- .make_source_dir(list("id_rsa" = "-----BEGIN RSA PRIVATE KEY-----"))
    result <- .check_no_secrets(list(), NULL, NULL, dir)
    expect_false(result$pass)
    expect_match(result$message, "private_key_header")
})

test_that("source_criteria returns the expected gate names", {
    expect_setequal(
        names(source_criteria()$gates),
        c("no_large_files", "no_remotes", "no_secrets", "no_merge_conflicts")
    )
})
