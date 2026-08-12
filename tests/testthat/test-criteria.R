test_that(".parse_platform splits os and arch", {
    expect_equal(.parse_platform("macos-x86_64"), list(os = "macos", arch = "x86_64"))
    expect_equal(.parse_platform("linux-arm64"), list(os = "linux", arch = "arm64"))
})

# ---- .needs_compilation ---------------------------------------------------

test_that(".needs_compilation trusts a declared NeedsCompilation first", {
    expect_true(.needs_compilation(list(NeedsCompilation = "yes"), list()))
    expect_false(.needs_compilation(list(NeedsCompilation = "no"), list()))
})

test_that(".needs_compilation falls back to _binaries' arch column when NeedsCompilation is missing", {
    # The rtracklayer case: DESCRIPTION omits NeedsCompilation even though
    # the package genuinely needs compilation.
    pkg_data <- list(`_binaries` = data.frame(arch = c("aarch64", "x86_64")))
    expect_true(.needs_compilation(list(), pkg_data))
})

test_that(".needs_compilation falls back to _binaries lacking an arch column (pure-R)", {
    pkg_data <- list(`_binaries` = data.frame(os = c("mac", "win")))
    expect_false(.needs_compilation(list(), pkg_data))
})

test_that(".needs_compilation falls back when NeedsCompilation is present but empty/NA", {
    pkg_data <- list(`_binaries` = data.frame(arch = "x86_64"))
    expect_true(.needs_compilation(list(NeedsCompilation = ""), pkg_data))
    expect_true(.needs_compilation(list(NeedsCompilation = NA), pkg_data))
})

test_that(".needs_compilation defaults to TRUE (the stricter assumption) when neither signal is available", {
    expect_true(.needs_compilation(list(), list()))
    expect_true(.needs_compilation(list(), list(`_binaries` = data.frame())))
})

test_that(".needs_compilation's pkg_data defaults to desc when not supplied separately", {
    # Matches .check_build_status()/.check_binary_status()'s real call
    # sites, which only ever pass one argument (desc), relying on the
    # pkg_data = desc default.
    desc <- list(`_binaries` = data.frame(arch = "arm64"))
    expect_true(.needs_compilation(desc))
})

# ---- .check_gate_job / .check_vignettes / .check_bioc_checks --------------

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

# ---- .check_version_valid ---------------------------------------------
# NOTE: bioc_pkg_data is now list(source = <data.frame>), not a bare
# data.frame -- .check_version_valid() reads bioc_pkg_data$source
# internally. A bare data.frame's $source is silently NULL, which would
# make every one of these tests exercise the vacuous-pass path
# regardless of what's being tested.

test_that(".check_version_valid passes when bioc_pkg_data has no prior version", {
    pkg_data <- .example_pkg_data()
    result <- .check_version_valid(pkg_data, "release", pkg_data, NULL, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid passes for a same-x.y z-increment", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.5"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.2.0"))
    result <- .check_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid fails for an invalid decrement", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "2.0.0"))
    result <- .check_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid fails when the version is unchanged", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.2.0"))
    result <- .check_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid fails for a maintainer-driven y-bump (1.4.3 -> 1.5.0)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.4.3"))
    result <- .check_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid fails for a maintainer-driven major bump (1.4.3 -> 2.0.0)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.0.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.4.3"))
    result <- .check_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid passes for a large z-skip within the same x.y (not exactly +1)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.50"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.2.0"))
    result <- .check_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid passes a release y/x-bump when bioc_pkg_data is NULL (brand-new release branch)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    result <- .check_version_valid(pkg_data, "release", pkg_data, NULL, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (devel) passes the release-cycle y+2 bump", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.5.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.3.7"))
    result <- .check_version_valid(pkg_data, "devel", pkg_data, bioc_pkg_data, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (devel) fails a y-bump by the wrong amount (not +2)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.4.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.3.7"))
    result <- .check_version_valid(pkg_data, "devel", pkg_data, bioc_pkg_data, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid (devel) passes the maintainer's y=99 signal", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.99.3"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.5.12"))
    result <- .check_version_valid(pkg_data, "devel", pkg_data, bioc_pkg_data, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (release) rejects the y=99 signal", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.99.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.4.3"))
    result <- .check_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, function() NULL)
    expect_false(result$pass)
})

test_that(".check_version_valid (devel) passes the major cutover when previous y was really 99", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.1.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.99.5"))
    result <- .check_version_valid(pkg_data, "devel", pkg_data, bioc_pkg_data, function() NULL)
    expect_true(result$pass)
})

test_that(".check_version_valid (devel) rejects a major-cutover-shaped bump when previous y wasn't 99", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "2.1.0"
    bioc_pkg_data <- list(source = data.frame(Package = "examplePkg", Version = "1.5.12"))
    result <- .check_version_valid(pkg_data, "devel", pkg_data, bioc_pkg_data, function() NULL)
    expect_false(result$pass)
})

# ---- .check_build_status: pure-R vs. compiled, and missing os-arch --------

test_that(".check_build_status passes for a pure-R package's platform (OS-prefix match, arch ignored)", {
    pkg_data <- .example_pkg_data(needs_compilation = FALSE)
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_true(result$pass)
})

test_that(".check_build_status passes for a compiled package's exact arch match", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_true(result$pass)
})

test_that(".check_build_status fails for a missing os-arch (compiled package, no job at all for that OS)", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    # Fixture has no "linux-*-arm64" job at all (only linux-*-x86_64).
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "linux-arm64")
    expect_false(result$pass)
    expect_match(result$message, "no build job found")
})

test_that(".check_build_status fails for a missing os entirely (no job for that OS at all)", {
    pkg_data <- .example_pkg_data(needs_compilation = FALSE)
    pkg_data[["_jobs"]] <- pkg_data[["_jobs"]][
        !startsWith(pkg_data[["_jobs"]][["config"]], "linux-"),
    ]
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "linux-x86_64")
    expect_false(result$pass)
    expect_match(result$message, "no build job found")
})

test_that(".check_build_status passes for windows-arm64 (_jobs uses arm64, not aarch64)", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    result <- .check_build_status(pkg_data, "release", pkg_data, NULL, "windows-arm64")
    expect_true(result$pass)
})

test_that(".DEFAULT_PLATFORMS includes windows-arm64", {
    expect_true("windows-arm64" %in% .DEFAULT_PLATFORMS)
})

# ---- .canonicalize_token / .parse_unsupported_entry / .check_supported_platform

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

# ---- .check_binary_status: pure-R vs. compiled, and missing os-arch -------

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

test_that(".check_binary_status fails for a missing os-arch (compiled, no binary row for that arch)", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    # Fixture's mac binary is only arm64 -- no macos-x86_64 row exists.
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "macos-x86_64")
    expect_false(result$pass)
    expect_match(result$message, "no binary found")
})

test_that(".check_binary_status fails for a missing os entirely (no binaries for that OS)", {
    pkg_data <- .example_pkg_data(needs_compilation = FALSE)
    pkg_data[["_binaries"]] <- pkg_data[["_binaries"]][
        pkg_data[["_binaries"]][["os"]] != "linux",
    ]
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "linux-x86_64")
    expect_false(result$pass)
    expect_match(result$message, "no binary found")
})

test_that(".check_binary_status fails when _binaries is entirely absent", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_binaries"]] <- NULL
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_false(result$pass)
    expect_match(result$message, "no binaries reported")
})

test_that(".check_binary_status fails for a compiled package with a malformed (arch-less) binaries table", {
    pkg_data <- .example_pkg_data(needs_compilation = TRUE)
    pkg_data[["_binaries"]][["arch"]] <- NULL
    result <- .check_binary_status(pkg_data, "release", pkg_data, NULL, "macos-arm64")
    expect_false(result$pass)
    expect_match(result$message, "missing arch column")
})

# ---- .check_platform_version_valid (new) -----------------------------------

test_that(".check_platform_version_valid passes windows when current exceeds the shared windows version", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.1.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.1.0"))
    )
    result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "windows-x86_64"
    )
    expect_true(result$pass)
})

test_that(".check_platform_version_valid fails windows when current equals the published version (strict >, not >=)", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.2.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.2.0"))
    )
    result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "windows-x86_64"
    )
    expect_false(result$pass)
})

test_that(".check_platform_version_valid fails windows when current is behind the published version", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.9.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.9.0"))
    )
    result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "windows-x86_64"
    )
    expect_false(result$pass)
})

test_that(".check_platform_version_valid uses the SAME windows entry for both windows-arm64 and windows-x86_64", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.1.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.1.0"))
    )
    r1 <- .check_platform_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, "windows-arm64")
    r2 <- .check_platform_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, "windows-x86_64")
    expect_true(r1$pass)
    expect_true(r2$pass)
})

test_that(".check_platform_version_valid uses macos-arm64's OWN entry, distinct from macos-x86_64", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.0.0"),
        platform = list(
            "macos-arm64" = data.frame(Package = "examplePkg", Version = "1.1.0"),   # behind current
            "macos-x86_64" = data.frame(Package = "examplePkg", Version = "1.9.0")   # ahead of current
        )
    )
    arm_result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "macos-arm64"
    )
    x86_result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "macos-x86_64"
    )
    expect_true(arm_result$pass)
    expect_false(x86_result$pass)
})

test_that(".check_platform_version_valid falls back to source data for linux (no separate linux binary repo)", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0"
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.1.0"),
        platform = list()  # deliberately empty -- linux must not look here
    )
    result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "linux-x86_64"
    )
    expect_true(result$pass)
})

test_that(".check_platform_version_valid falls back to previous-branch data when current branch has none", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0", y = 2
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),  # no entry for examplePkg
        platform = list(),
        previous = list(
            source = data.frame(Package = "examplePkg", Version = "1.1.0")  # y = 1, less than current's 2
        )
    )
    result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "linux-x86_64"
    )
    expect_true(result$pass)
})

test_that(".check_platform_version_valid fails the fallback when y did not increase over the previous branch", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0", y = 2
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),
        platform = list(),
        previous = list(
            source = data.frame(Package = "examplePkg", Version = "1.5.0")  # y = 5, greater than current's 2
        )
    )
    result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "linux-x86_64"
    )
    expect_false(result$pass)
    expect_match(result$message, "y-increase")
})

test_that(".check_platform_version_valid passes vacuously when neither current nor previous branch has an entry", {
    pkg_data <- .example_pkg_data()
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),
        platform = list(),
        previous = list(source = data.frame(Package = "otherPkg", Version = "9.9.9"))
    )
    result <- .check_platform_version_valid(
        pkg_data, "release", pkg_data, bioc_pkg_data, "linux-x86_64"
    )
    expect_true(result$pass)
})

test_that(".check_platform_version_valid's fallback uses windows' shared entry, same as the primary lookup", {
    pkg_data <- .example_pkg_data()  # Version "1.2.0", y = 2
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),
        platform = list(),
        previous = list(
            platform = list(windows = data.frame(Package = "examplePkg", Version = "1.1.0"))
        )
    )
    r1 <- .check_platform_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, "windows-arm64")
    r2 <- .check_platform_version_valid(pkg_data, "release", pkg_data, bioc_pkg_data, "windows-x86_64")
    expect_true(r1$pass)
    expect_true(r2$pass)
})

# ---- default_criteria / default_results_criteria / register_criterion ----

test_that("default_criteria returns the expected gate and platform names", {
    criteria <- default_criteria()
    expect_named(criteria, c("gates", "platform"))
    expect_setequal(names(criteria$gates), c(
        "vignettes", "version",
        "no_large_files", "no_git_lfs", "no_remotes",
        "no_secrets", "no_merge_conflicts"
    ))
    expect_setequal(
        names(criteria$platform),
        c("build", "binary", "unsupported", "platform_version")
    )
})

test_that("default_criteria's git gates can be removed to opt out of clone cost", {
    criteria <- default_criteria()
    criteria$gates[names(git_criteria()$gates)] <- NULL
    expect_setequal(names(criteria$gates), c("vignettes", "version"))
})

test_that("default_results_criteria excludes binary but keeps platform_version", {
    criteria <- default_results_criteria()
    expect_false("binary" %in% names(criteria$platform))
    expect_true("platform_version" %in% names(criteria$platform))
    expect_setequal(names(criteria$platform), c("build", "unsupported", "platform_version"))
})

test_that("register_criterion adds a new gate without disturbing existing ones", {
    criteria <- default_criteria()
    always_pass <- function(pkg_data, branch, desc, bioc_pkg_data, repo)
        list(pass = TRUE, message = NA_character_)
    criteria <- register_criterion(criteria, "always_pass", always_pass, type = "gates")
    expect_true("always_pass" %in% names(criteria$gates))
    expect_true("vignettes" %in% names(criteria$gates))
})

test_that("register_criterion replaces an existing criterion of the same name", {
    criteria <- default_criteria()
    always_fail <- function(pkg_data, branch, desc, bioc_pkg_data, repo)
        list(pass = FALSE, message = "nope")
    criteria <- register_criterion(criteria, "vignettes", always_fail, type = "gates")
    expect_false(criteria$gates$vignettes(list(), "release", list(), NULL, function() NULL)$pass)
})
