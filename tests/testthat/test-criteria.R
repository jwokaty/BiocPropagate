test_that(".parse_platform splits os and arch", {
    expect_equal(.parse_platform("macos-x86_64"), list(os = "macos", arch = "x86_64"))
    expect_equal(.parse_platform("linux-arm64"), list(os = "linux", arch = "arm64"))
})

test_that(".needs_compilation reads NeedsCompilation", {
    expect_true(.needs_compilation(list(NeedsCompilation = "yes")))
    expect_false(.needs_compilation(list(NeedsCompilation = "no")))
})

test_that(".check_gate_job passes when the named job is OK", {
    pkg_data <- .example_pkg_data()
    result <- .check_gate_job(pkg_data, "source")
    expect_true(result$pass)
    expect_true(is.na(result$message))
})

test_that(".check_gate_job fails when the named job is missing", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]] <- pkg_data[["_jobs"]][
        pkg_data[["_jobs"]][["config"]] != "source",
    ]
    result <- .check_gate_job(pkg_data, "source")
    expect_false(result$pass)
    expect_match(result$message, "no 'source' job found")
})

test_that(".check_gate_job fails when the named job's check is a failure status", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "source", "check"] <- "ERROR"
    result <- .check_gate_job(pkg_data, "source")
    expect_false(result$pass)
})

test_that(".check_vignettes and .check_bioc_checks pass on a clean fixture", {
    pkg_data <- .example_pkg_data()
    expect_true(.check_vignettes(pkg_data, "release", pkg_data, NULL, function() NULL)$pass)
    expect_true(.check_bioc_checks(pkg_data, "release", pkg_data, NULL, function() NULL)$pass)
})

test_that(".check_vignettes ignores R version mismatches (the deliberate exception)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "source", "r"] <- "1.2.3"
    result <- .check_vignettes(pkg_data, "release", pkg_data, NULL, function() NULL)
    expect_true(result$pass)
})

test_that(".check_bioc_checks fails when the job's R version doesn't match the branch", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "bioc-checks", "r"] <- "1.2.3"
    result <- .check_bioc_checks(pkg_data, "release", pkg_data, NULL, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid passes when views has no prior version", {
    pkg_data <- .example_pkg_data()
    result <- .check_version_valid(pkg_data, "release", pkg_data, NULL, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid passes for a same-x.y z-increment", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.5"
    views <- data.frame(Package = "examplePkg", Version = "1.2.0")
    result <- .check_version_valid(pkg_data, "release", pkg_data, views, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid fails for an invalid decrement", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    views <- data.frame(Package = "examplePkg", Version = "2.0.0")
    result <- .check_version_valid(pkg_data, "release", pkg_data, views, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid fails when the version is unchanged", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    views <- data.frame(Package = "examplePkg", Version = "1.2.0")
    result <- .check_version_valid(pkg_data, "release", pkg_data, views, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid fails for a maintainer-driven y-bump (1.4.3 -> 1.5.0)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    views <- data.frame(Package = "examplePkg", Version = "1.4.3")
    result <- .check_version_valid(pkg_data, "release", pkg_data, views, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid fails for a maintainer-driven major bump (1.4.3 -> 2.0.0)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.0.0"
    views <- data.frame(Package = "examplePkg", Version = "1.4.3")
    result <- .check_version_valid(pkg_data, "release", pkg_data, views, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid passes for a large z-skip within the same x.y (not exactly +1)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.50"
    views <- data.frame(Package = "examplePkg", Version = "1.2.0")
    result <- .check_version_valid(pkg_data, "release", pkg_data, views, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid passes a release y/x-bump when views is NULL (brand-new release branch)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    result <- .check_version_valid(pkg_data, "release", pkg_data, NULL, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (devel) passes the release-cycle y+2 bump", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    views <- data.frame(Package = "examplePkg", Version = "1.3.7")
    result <- .check_version_valid(pkg_data, "devel", pkg_data, views, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (devel) fails a y-bump by the wrong amount (not +2)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.4.0"
    views <- data.frame(Package = "examplePkg", Version = "1.3.7")
    result <- .check_version_valid(pkg_data, "devel", pkg_data, views, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid (devel) passes the maintainer's y=99 signal", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.99.3"
    views <- data.frame(Package = "examplePkg", Version = "1.5.12")
    result <- .check_version_valid(pkg_data, "devel", pkg_data, views, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (release) rejects the y=99 signal", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.99.0"
    views <- data.frame(Package = "examplePkg", Version = "1.4.3")
    result <- .check_version_valid(pkg_data, "release", pkg_data, views, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid (devel) passes the major cutover when previous y was really 99", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.1.0"
    views <- data.frame(Package = "examplePkg", Version = "1.99.5")
    result <- .check_version_valid(pkg_data, "devel", pkg_data, views, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (devel) rejects a major-cutover-shaped bump when previous y wasn't 99", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.1.0"
    views <- data.frame(Package = "examplePkg", Version = "1.5.12")
    result <- .check_version_valid(pkg_data, "devel", pkg_data, views, function() NULL)
    expect_false(result$pass)
})

test_that(".check_build_status passes for a pure-R package's platform", {
    pkg_data <- .example_pkg_data(needs_compilation = FALSE)
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_true(result$pass)
})

test_that(".check_build_status fails when no job matches the exact arch (compiled)", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "linux-arm64")
    expect_false(result$pass)
})

test_that(".canonicalize_token maps known aliases", {
    expect_equal(.canonicalize_token("win", .OS_ALIASES), "windows")
    expect_equal(.canonicalize_token("aarch64", .ARCH_ALIASES), "arm64")
    expect_equal(.canonicalize_token("unknown-os", .OS_ALIASES), "unknown-os")
})

test_that(".parse_unsupported_entry handles OS-only and OS-arch entries", {
    expect_equal(.parse_unsupported_entry("win"), list(os = "windows", arch = NA_character_))
    expect_equal(
        .parse_unsupported_entry("macosx-arm64"),
        list(os = "macos", arch = "arm64")
    )
})

test_that(".check_supported_platform passes when nothing is declared unsupported", {
    desc <- list(`Config/Bioconductor/UnsupportedPlatforms` = NULL)
    result <- .check_supported_platform(list(), "release", desc, NULL, "windows-x86_64")
    expect_true(result$pass)
})

test_that(".check_supported_platform fails for an OS-only unsupported declaration", {
    desc <- list(`Config/Bioconductor/UnsupportedPlatforms` = "win")
    result <- .check_supported_platform(list(), "release", desc, NULL, "windows-x86_64")
    expect_false(result$pass)
})

test_that(".check_supported_platform never fails for linux", {
    desc <- list(`Config/Bioconductor/UnsupportedPlatforms` = "linux")
    result <- .check_supported_platform(list(), "release", desc, NULL, "linux-x86_64")
    expect_true(result$pass)
})

test_that(".check_binary_status passes for a pure-R package's binary", {
    pkg_data <- .example_pkg_data(needs_compilation = FALSE)
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_true(result$pass)
})

test_that(".check_binary_status passes for a compiled package's matching arch", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_true(result$pass)
})

test_that(".check_binary_status passes for windows-arm64, despite _binaries using aarch64", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "windows-arm64")
    expect_true(result$pass)
})

test_that(".check_build_status passes for windows-arm64 (_jobs uses arm64, not aarch64)", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "windows-arm64")
    expect_true(result$pass)
})

test_that(".DEFAULT_PLATFORMS includes windows-arm64", {
    expect_true("windows-arm64" %in% .DEFAULT_PLATFORMS)
})

test_that(".check_binary_status fails for a compiled package with a malformed (arch-less) binaries table", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    pkg_data[["_binaries"]][["arch"]] <- NULL
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_false(result$pass)
    expect_match(result$message, "missing arch column")
})

test_that("default_criteria returns the expected gate and platform names", {
    criteria <- default_criteria()
    expect_named(criteria, c("gates", "platform"))
    expect_setequal(names(criteria$gates), c(
        "vignettes", "bioc_checks", "version",
        "no_large_files", "no_git_lfs", "no_remotes",
        "no_secrets", "no_merge_conflicts"
    ))
    expect_setequal(names(criteria$platform), c("build", "binary", "unsupported"))
})

test_that("default_criteria's git gates can be removed to opt out of clone cost", {
    criteria <- default_criteria()
    criteria$gates[names(git_criteria()$gates)] <- NULL
    expect_setequal(names(criteria$gates), c("vignettes", "bioc_checks", "version"))
})

test_that("register_criterion adds a new gate without disturbing existing ones", {
    criteria <- default_criteria()
    always_pass <- function(pkg_data, branch, desc, views, repo)
        list(pass = TRUE, message = NA_character_)
    criteria <- register_criterion(criteria, "always_pass", always_pass, type = "gates")
    expect_true("always_pass" %in% names(criteria$gates))
    expect_true("vignettes" %in% names(criteria$gates))
})

test_that("register_criterion replaces an existing criterion of the same name", {
    criteria <- default_criteria()
    always_fail <- function(pkg_data, branch, desc, views, repo)
        list(pass = FALSE, message = "nope")
    criteria <- register_criterion(criteria, "vignettes", always_fail, type = "gates")
    expect_false(criteria$gates$vignettes(list(), "release", list(), NULL, function() NULL)$pass)
})
