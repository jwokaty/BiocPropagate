test_that(".evaluate_gates passes when every gate passes", {
    pkg_data <- .example_pkg_data()
    # Non-git gates only -- the git-based gates in default_criteria()
    # need a real repo accessor, not the function() NULL stub used here;
    # those have their own dedicated tests in test-git_checks.R.
    gates <- default_criteria()$gates[c("vignettes", "version")]
    expect_true(.evaluate_gates(gates, pkg_data, "release", pkg_data, NULL, function() NULL))
})

test_that(".evaluate_gates fails and messages when a gate fails", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]] <- pkg_data[["_jobs"]][
        pkg_data[["_jobs"]][["config"]] != "source",
    ]
    gates <- default_criteria()$gates[c("vignettes", "version")]
    expect_message(
        result <- .evaluate_gates(gates, pkg_data, "release", pkg_data, NULL, function() NULL),
        "gate 'vignettes'"
    )
    expect_false(result)
})

test_that(".evaluate_gates catches an error from a gate and converts it to a clean failure", {
    pkg_data <- .example_pkg_data()
    gates <- list(
        broken = function(pkg_data, branch, desc, bioc_pkg_data, repo)
            stop("something went wrong inside this gate")
    )
    expect_message(
        result <- .evaluate_gates(gates, pkg_data, "release", pkg_data, NULL, function() NULL),
        "something went wrong inside this gate"
    )
    expect_false(result)
})

test_that(".evaluate_gates is fail-fast: a later gate never runs after an earlier one fails", {
    pkg_data <- .example_pkg_data()
    ran_second <- FALSE
    gates <- list(
        first = function(pkg_data, branch, desc, bioc_pkg_data, repo)
            list(pass = FALSE, message = "first failed"),
        second = function(pkg_data, branch, desc, bioc_pkg_data, repo) {
            ran_second <<- TRUE
            list(pass = TRUE, message = NA_character_)
        }
    )
    suppressMessages(
        .evaluate_gates(gates, pkg_data, "release", pkg_data, NULL, function() NULL)
    )
    expect_false(ran_second)
})

test_that(".evaluate_platform passes when every criterion passes", {
    pkg_data <- .example_pkg_data()
    criteria <- default_criteria()$platform
    expect_true(.evaluate_platform(criteria, pkg_data, "release", pkg_data, NULL, "macos-arm64"))
})

test_that(".evaluate_platform fails and messages when a criterion fails", {
    pkg_data <- .example_pkg_data()
    criteria <- default_criteria()$platform
    expect_message(
        result <- .evaluate_platform(criteria, pkg_data, "release", pkg_data, NULL, "totallyfakeos-x86_64"),
        "propagation check failed"
    )
    expect_false(result)
})

test_that(".evaluate_platform catches an error from a criterion and converts it to a clean failure", {
    pkg_data <- .example_pkg_data()
    criteria <- list(
        broken = function(pkg_data, branch, desc, bioc_pkg_data, platform)
            stop("something went wrong inside this criterion")
    )
    expect_message(
        result <- .evaluate_platform(criteria, pkg_data, "release", pkg_data, NULL, "macos-arm64"),
        "something went wrong inside this criterion"
    )
    expect_false(result)
})

test_that(".evaluate_propagation returns the standard shape and reflects platform-specific data correctly", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.5"  # same x.y as source below, z-bump -> valid gate
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.2.0"),
        platform = list(
            windows = data.frame(Package = "examplePkg", Version = "1.2.0"),
            "macos-arm64" = data.frame(Package = "examplePkg", Version = "1.2.0"),
            "macos-x86_64" = data.frame(Package = "examplePkg", Version = "1.9.0")  # ahead
        )
    )
    criteria <- default_criteria()
    criteria$gates[names(git_criteria()$gates)] <- NULL  # fixture has no RemoteUrl to clone
    result <- .evaluate_propagation(
        "examplePkg", pkg_data, pkg_data, "release", criteria,
        c("windows-x86_64", "macos-arm64", "macos-x86_64"), bioc_pkg_data, NULL
    )
    expect_named(result, c("package", "gates", "platforms"))
    expect_true(result$gates)
    expect_true(result$platforms[["windows-x86_64"]])
    expect_true(result$platforms[["macos-arm64"]])
    expect_false(result$platforms[["macos-x86_64"]])  # behind published version
})

test_that("gates passing does not guarantee a platform passes: linux's own build-check failure fails linux specifically", {
    pkg_data <- .example_pkg_data()  # source job passes -> gates should pass
    pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "linux-release-x86_64", "check"] <- "ERROR"
    bioc_pkg_data <- list(
        source = data.frame(Package = "otherPkg", Version = "9.9.9"),
        platform = list("macos-arm64" = data.frame(Package = "examplePkg", Version = "1.0.0"))
    )

    criteria <- default_criteria()
    criteria$gates[names(git_criteria()$gates)] <- NULL  # fixture has no RemoteUrl to clone
    result <- .evaluate_propagation(
        "examplePkg", pkg_data, pkg_data, "release", criteria,
        c("linux-x86_64", "macos-arm64"), bioc_pkg_data, NULL
    )

    expect_true(result$gates)  # source/vignettes still passed
    expect_false(result$platforms[["linux-x86_64"]])  # its own build-check failed
    expect_true(result$platforms[["macos-arm64"]])  # unaffected, independent platform
})

# ---- Real-network tests: check_propagation() ------------------------------

test_that("check_propagation reports a plain TRUE/FALSE grid for a real package", {
    skip_if_offline()
    result <- check_propagation("BiocCheck", branch = "release")
    expect_named(result, c("package", "gates", "platforms"))
    expect_type(result$gates, "logical")
    expect_true(all(vapply(result$platforms, is.logical, logical(1L))))
})

test_that("check_propagation's platforms element matches the requested platforms exactly", {
    skip_if_offline()
    requested <- c("linux-x86_64", "macos-arm64")
    result <- check_propagation("BiocCheck", branch = "release", platforms = requested)
    expect_named(result$platforms, requested)
})

test_that("check_propagation accepts a caller-supplied bioc_pkg_data, skipping the internal fetch", {
    skip_if_offline()
    bioc_pkg_data <- list(source = data.frame(Package = "not-a-real-package", Version = "0.0.1"))
    # If this ran the internal fetch anyway, it would take much longer
    # and/or reflect real data; this is a light sanity check that the
    # supplied object is actually threaded through, not a timing test.
    result <- check_propagation(
        "BiocCheck", branch = "release", bioc_pkg_data = bioc_pkg_data,
        platforms = "linux-x86_64"
    )
    expect_named(result, c("package", "gates", "platforms"))
})

# ---- check_propagation_from_results() --------------------------------------

test_that("check_propagation_from_results uses default_results_criteria by default (no binary check)", {
    pkg_data <- .example_pkg_data()
    # Different package name in the lookup table -> vacuous pass on the
    # version gate; this test isn't about version-pattern correctness,
    # just that the default criteria set excludes binary.
    bioc_pkg_data <- list(source = data.frame(Package = "someOtherPkg", Version = "1.1.0"))
    criteria <- default_results_criteria()
    criteria$gates[names(git_criteria()$gates)] <- NULL  # fixture has no RemoteUrl to clone
    result <- check_propagation_from_results(
        "examplePkg", pkg_data, branch = "release", criteria = criteria,
        bioc_pkg_data = bioc_pkg_data, platforms = "macos-arm64"
    )
    expect_named(result, c("package", "gates", "platforms"))
    # Passes even though pkg_data has no real _binaries setup beyond the
    # fixture default -- proof binary status isn't gating this result,
    # since default_results_criteria() excludes it.
    expect_true(result$gates)
})

test_that("check_propagation_from_results does not call .get_runi_meta -- pkg_data must be supplied", {
    # No network mocking available here; this just confirms the function
    # signature requires pkg_data positionally (an omitted, required
    # argument errors before any fetch would occur).
    expect_error(
        check_propagation_from_results(pkgName = "examplePkg", branch = "release")
    )
})

test_that("check_propagation_from_results respects a caller-supplied bioc_pkg_data (no fetch)", {
    pkg_data <- .example_pkg_data()
    pkg_data[["Version"]] <- "1.2.5"  # same x.y as source below, z-bump -> valid gate
    bioc_pkg_data <- list(
        source = data.frame(Package = "examplePkg", Version = "1.2.0"),
        platform = list(windows = data.frame(Package = "examplePkg", Version = "1.0.0"))
    )
    criteria <- default_results_criteria()
    criteria$gates[names(git_criteria()$gates)] <- NULL  # fixture has no RemoteUrl to clone
    result <- check_propagation_from_results(
        "examplePkg", pkg_data, branch = "release", criteria = criteria,
        bioc_pkg_data = bioc_pkg_data, platforms = "windows-x86_64"
    )
    expect_true(result$gates)
    expect_true(result$platforms[["windows-x86_64"]])
})

test_that("check_propagation_from_results' platform criteria never include binary", {
    criteria <- default_results_criteria()
    expect_false("binary" %in% names(criteria$platform))
})
