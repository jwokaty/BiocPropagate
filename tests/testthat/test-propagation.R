test_that(".evaluate_gates passes when every gate passes", {
    pkg_data <- .example_pkg_data()
    # Non-git gates only -- the git-based gates in default_criteria()
    # need a real repo accessor, not the function() NULL stub used here;
    # those have their own dedicated tests in test-git_checks.R.
    gates <- default_criteria()$gates[c("vignettes", "bioc_checks", "version")]
    expect_true(.evaluate_gates(gates, pkg_data, "release", pkg_data, NULL, function() NULL))
})

test_that(".evaluate_gates fails and messages when a gate fails", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]] <- pkg_data[["_jobs"]][
        pkg_data[["_jobs"]][["config"]] != "source",
    ]
    gates <- default_criteria()$gates
    expect_message(
        result <- .evaluate_gates(gates, pkg_data, "release", pkg_data, NULL, function() NULL),
        "gate 'vignettes'"
    )
    expect_false(result)
})

test_that(".evaluate_gates catches an error from a gate and converts it to a clean failure", {
    pkg_data <- .example_pkg_data()
    gates <- list(
        broken = function(pkg_data, branch, desc, views, repo)
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
        first = function(pkg_data, branch, desc, views, repo)
            list(pass = FALSE, message = "first failed"),
        second = function(pkg_data, branch, desc, views, repo) {
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
        broken = function(pkg_data, branch, desc, views, platform)
            stop("something went wrong inside this criterion")
    )
    expect_message(
        result <- .evaluate_platform(criteria, pkg_data, "release", pkg_data, NULL, "macos-arm64"),
        "something went wrong inside this criterion"
    )
    expect_false(result)
})

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
