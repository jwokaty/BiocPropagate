test_that(".parse_platform splits os and arch", {
    expect_equal(.parse_platform("macos-x86_64"), list(os = "macos", arch = "x86_64"))
    expect_equal(.parse_platform("linux-arm64"), list(os = "linux", arch = "arm64"))
})

test_that(".parse_job_config splits a real job config", {
    expect_equal(
        .parse_job_config("linux-devel-x86_64"),
        list(os = "linux", r_channel = "devel", arch = "x86_64")
    )
})

test_that(".parse_job_config returns NA fields for non-platform rows", {
    expect_true(is.na(.parse_job_config("source")$os))
    expect_true(is.na(.parse_job_config("bioc-checks")$os))
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
    expect_true(.check_vignettes(pkg_data, "release", NULL, NULL)$pass)
    expect_true(.check_bioc_checks(pkg_data, "release", NULL, NULL)$pass)
})

test_that(".check_vignettes ignores R version mismatches (the deliberate exception)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "source", "r"] <- "1.2.3"
    result <- .check_vignettes(pkg_data, "release", NULL, NULL)
    expect_true(result$pass)
})

test_that(".check_bioc_checks fails when the job's R version doesn't match the branch", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "bioc-checks", "r"] <- "1.2.3"
    result <- .check_bioc_checks(pkg_data, "release", NULL, NULL)
    expect_false(result$pass)
})

test_that(".check_package_version passes when bioc_pkg_data has no prior version", {
    pkg_data <- .example_pkg_data()
    result <- .check_package_version(pkg_data, "release", NULL, NULL)
    expect_true(result$pass)
})

test_that(".check_package_version passes for a same-x.y z-increment", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.5"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.2.0"))
    result <- .check_package_version(pkg_data, "release", bioc_pkg_data, NULL)
    expect_true(result$pass)
})

test_that(".check_package_version fails for an invalid decrement", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "2.0.0"))
    result <- .check_package_version(pkg_data, "release", bioc_pkg_data, NULL)
    expect_false(result$pass)
})

test_that(".check_package_version fails when the version is unchanged", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.2.0"))
    result <- .check_package_version(pkg_data, "release", bioc_pkg_data, NULL)
    expect_false(result$pass)
})

test_that(".check_package_version fails for a maintainer-driven y-bump (1.4.3 -> 1.5.0)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.4.3"))
    result <- .check_package_version(pkg_data, "release", bioc_pkg_data, NULL)
    expect_false(result$pass)
})

test_that(".check_package_version fails for a maintainer-driven major bump (1.4.3 -> 2.0.0)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.0.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.4.3"))
    result <- .check_package_version(pkg_data, "release", bioc_pkg_data, NULL)
    expect_false(result$pass)
})

test_that(".check_package_version passes for a large z-skip within the same x.y", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.50"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.2.0"))
    result <- .check_package_version(pkg_data, "release", bioc_pkg_data, NULL)
    expect_true(result$pass)
})

test_that(".check_package_version passes a release y/x-bump when bioc_pkg_data is NULL", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    result <- .check_package_version(pkg_data, "release", NULL, NULL)
    expect_true(result$pass)
})

test_that(".check_package_version (devel) passes the release-cycle y+2 bump", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.3.7"))
    result <- .check_package_version(pkg_data, "devel", bioc_pkg_data, NULL)
    expect_true(result$pass)
})

test_that(".check_package_version (devel) fails a y-bump by the wrong amount", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.4.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.3.7"))
    result <- .check_package_version(pkg_data, "devel", bioc_pkg_data, NULL)
    expect_false(result$pass)
})

test_that(".check_package_version (devel) passes the maintainer's y=99 signal", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.99.3"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.5.12"))
    result <- .check_package_version(pkg_data, "devel", bioc_pkg_data, NULL)
    expect_true(result$pass)
})

test_that(".check_package_version (release) rejects the y=99 signal", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.99.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.4.3"))
    result <- .check_package_version(pkg_data, "release", bioc_pkg_data, NULL)
    expect_false(result$pass)
})

test_that(".check_package_version (devel) passes the major cutover when previous y was 99", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.1.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.99.5"))
    result <- .check_package_version(pkg_data, "devel", bioc_pkg_data, NULL)
    expect_true(result$pass)
})

test_that(".check_package_version (devel) rejects a major-cutover-shaped bump when previous y wasn't 99", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.1.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.5.12"))
    result <- .check_package_version(pkg_data, "devel", bioc_pkg_data, NULL)
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

test_that(".is_supported passes when nothing is declared unsupported", {
    pkg_data <- list(`Config/Bioconductor/UnsupportedPlatforms` = NULL)
    result <- .is_supported(pkg_data, "release", NULL, "windows-x86_64")
    expect_true(result$pass)
})

test_that(".is_supported fails for an OS-only unsupported declaration", {
    pkg_data <- list(`Config/Bioconductor/UnsupportedPlatforms` = "win")
    result <- .is_supported(pkg_data, "release", NULL, "windows-x86_64")
    expect_false(result$pass)
})

test_that(".is_supported never fails for linux", {
    pkg_data <- list(`Config/Bioconductor/UnsupportedPlatforms` = "linux")
    result <- .is_supported(pkg_data, "release", NULL, "linux-x86_64")
    expect_true(result$pass)
})

test_that(".is_valid_version passes windows when current exceeds the shared windows version", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.1.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.1.0"))
    )
    result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "windows-x86_64")
    expect_true(result$pass)
})

test_that(".is_valid_version fails windows when current equals the published version", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.2.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.2.0"))
    )
    result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "windows-x86_64")
    expect_false(result$pass)
})

test_that(".is_valid_version fails windows when current is behind the published version", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.9.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.9.0"))
    )
    result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "windows-x86_64")
    expect_false(result$pass)
})

test_that(".is_valid_version uses the same windows entry for both arches", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.1.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.1.0"))
    )
    r1 <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "windows-arm64")
    r2 <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "windows-x86_64")
    expect_true(r1$pass)
    expect_true(r2$pass)
})

test_that(".is_valid_version uses macos-arm64's own entry, distinct from macos-x86_64", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.0.0"),
        platform = list(
            "macos-arm64" = data.frame(Package = "examplePkg", Version = "1.1.0"),
            "macos-x86_64" = data.frame(Package = "examplePkg", Version = "1.9.0")
        )
    )
    arm_result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "macos-arm64")
    x86_result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "macos-x86_64")
    expect_true(arm_result$pass)
    expect_false(x86_result$pass)
})

test_that(".is_valid_version falls back to source data for linux", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.1.0"),
        platform = list()
    )
    result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "linux-x86_64")
    expect_true(result$pass)
})

test_that(".is_valid_version falls back to previous-branch data when current branch has none", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0", y = 2
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),
        platform = list(),
        previous = list(source = data.frame(Package = "examplePkg", Version = "1.1.0"))
    )
    result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "linux-x86_64")
    expect_true(result$pass)
})

test_that(".is_valid_version fails the fallback when y did not increase", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0", y = 2
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),
        platform = list(),
        previous = list(source = data.frame(Package = "examplePkg", Version = "1.5.0"))
    )
    result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "linux-x86_64")
    expect_false(result$pass)
    expect_match(result$message, "y-increase")
})

test_that(".is_valid_version passes vacuously when neither branch has an entry", {
    pkg_data <- .example_pkg_data()
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),
        platform = list(),
        previous = list(source = data.frame(Package = "otherPkg", Version = "9.9.9"))
    )
    result <- .is_valid_version(pkg_data, "release", bioc_pkg_data, "linux-x86_64")
    expect_true(result$pass)
})

test_that(".check_platform_status passes for a matching, passing row", {
    pkg_data <- .example_pkg_data()
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "linux-release-x86_64", ]
    result <- .check_platform_status(pkg_data, "release", NULL, row)
    expect_true(result$pass)
})

test_that(".check_platform_status fails when the row's R version doesn't match the branch", {
    pkg_data <- .example_pkg_data()
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "linux-release-x86_64", ]
    row[["r"]] <- "1.2.3"
    result <- .check_platform_status(pkg_data, "release", NULL, row)
    expect_false(result$pass)
    expect_match(result$message, "does not match")
})

test_that(".check_platform_status fails for a failing check status", {
    pkg_data <- .example_pkg_data()
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "linux-release-x86_64", ]
    row[["check"]] <- "ERROR"
    result <- .check_platform_status(pkg_data, "release", NULL, row)
    expect_false(result$pass)
})

test_that(".check_supported_platform passes automatically for non-platform rows", {
    pkg_data <- .example_pkg_data()
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "source", ]
    result <- .check_supported_platform(pkg_data, "release", NULL, row)
    expect_true(result$pass)
})

test_that(".check_supported_platform derives os-arch and delegates correctly", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Config/Bioconductor/UnsupportedPlatforms"]] <- "win"
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "windows-release-x86_64", ]
    result <- .check_supported_platform(pkg_data, "release", NULL, row)
    expect_false(result$pass)
})

test_that(".check_platform_version passes automatically for non-platform rows", {
    pkg_data <- .example_pkg_data()
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "bioc-checks", ]
    result <- .check_platform_version(pkg_data, "release", NULL, row)
    expect_true(result$pass)
})

test_that(".check_platform_version derives os-arch and delegates correctly", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "9.9.9"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "9.9.9"))
    )
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "windows-release-x86_64", ]
    result <- .check_platform_version(pkg_data, "release", bioc_pkg_data, row)
    expect_false(result$pass)
})

test_that("default_criteria returns the expected gate and row names", {
    criteria <- default_criteria()
    expect_named(criteria, c("gates", "platform"))
    expect_setequal(names(criteria$gates), c(
        "vignettes", "package_version",
        "no_large_files", "no_remotes", "no_secrets", "no_merge_conflicts"
    ))
    expect_setequal(names(criteria$platform), c("status", "unsupported", "platform_version"))
})

test_that("default_criteria's source gates can be removed", {
    criteria <- default_criteria()
    criteria$gates[names(source_criteria()$gates)] <- NULL
    expect_setequal(names(criteria$gates), c("vignettes", "package_version"))
})

test_that("register_criterion adds a new gate without disturbing existing ones", {
    criteria <- default_criteria()
    always_pass <- function(pkg_data, branch, bioc_pkg_data, source_path)
        list(pass = TRUE, message = NA_character_)
    criteria <- register_criterion(criteria, "always_pass", always_pass, type = "gates")
    expect_true("always_pass" %in% names(criteria$gates))
    expect_true("vignettes" %in% names(criteria$gates))
})

test_that("register_criterion replaces an existing criterion of the same name", {
    criteria <- default_criteria()
    always_fail <- function(pkg_data, branch, bioc_pkg_data, source_path)
        list(pass = FALSE, message = "nope")
    criteria <- register_criterion(criteria, "vignettes", always_fail, type = "gates")
    expect_false(criteria$gates$vignettes(list(), "release", NULL, NULL)$pass)
})

test_that("register_criterion works for row-type criteria too", {
    criteria <- default_criteria()
    always_fail <- function(pkg_data, branch, bioc_pkg_data, row)
        list(pass = FALSE, message = "nope")
    criteria <- register_criterion(criteria, "status", always_fail, type = "platform")
    expect_false(criteria$platform$status(list(), "release", NULL, data.frame())$pass)
})

test_that("unregister_gates removes gates from list", {
    criteria <- default_criteria()
    criteria$gates[c("no_large_files", "no_remotes")] <- NULL
    expect_equal(criteria,
                 unregister_gates(default_criteria(),
                                  c("no_large_files", "no_remotes")))
})

test_that("unregister_gates removes nothing if gates do not exist", {
    criteria <- default_criteria()
    criteria$gates[c("nonexistant_gate")] <- NULL
    expect_equal(criteria,
                 unregister_gates(default_criteria(), c("nonexistant_gate")))
})

test_that("unregister_gates removes given gates", {
    criteria <- default_criteria()
    criteria$gates[c("no_large_files", "no_remotes")] <- NULL
    expect_equal(criteria,
                 unregister_gates(default_criteria(),
                                  c("no_large_files", "no_remotes")))
})

manifest <- "http://github.com/jwokaty/manifest"

test_that("remove_criteria removes criteria if package is exempt", {
    criteria <- default_criteria()
    criteria$gates[c("no_large_files", "no_secrets", "no_merge_conflicts")] <- NULL
    expect_equal(criteria,
                 remove_criteria("package1", "devel", default_criteria(),
                                 manifest))
})

test_that("remove_criteria removes no criteria if no exemptions", {
    criteria <- default_criteria()
    expect_equal(criteria,
                 remove_criteria("package3", "devel", default_criteria(),
                                 manifest))
})
