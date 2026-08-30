.make_desc_dir <- function(package = "examplePkg", version = "1.2.0", extra = list()) {
    dir <- tempfile("pkg-")
    dir.create(dir)
    fields <- c(list(Package = package, Version = version), extra)
    lines <- paste0(names(fields), ": ", unlist(fields))
    writeLines(lines, file.path(dir, "DESCRIPTION"))
    dir
}

test_that(".evaluate_gates passes when every gate passes", {
    pkg_data <- .example_pkg_data()
    gates <- default_criteria()$gates["vignettes"]
    expect_true(.evaluate_gates(gates, pkg_data, "release", NULL, NULL))
})

test_that(".evaluate_gates fails and messages when a gate fails", {
    pkg_data <- .example_pkg_data()
    pkg_data[["_jobs"]] <- pkg_data[["_jobs"]][
        pkg_data[["_jobs"]][["config"]] != "source",
    ]
    gates <- default_criteria()$gates["vignettes"]
    expect_message(
        result <- .evaluate_gates(gates, pkg_data, "release", NULL, NULL),
        "gate 'vignettes'"
    )
    expect_false(result)
})

test_that(".evaluate_gates catches an error from a gate and converts it to a clean failure", {
    pkg_data <- .example_pkg_data()
    gates <- list(
        broken = function(pkg_data, branch, bioc_pkg_data, source_path)
            stop("something went wrong inside this gate")
    )
    expect_message(
        result <- .evaluate_gates(gates, pkg_data, "release", NULL, NULL),
        "something went wrong inside this gate"
    )
    expect_false(result)
})

test_that(".evaluate_gates runs every gate, not just until the first failure", {
    pkg_data <- .example_pkg_data()
    ran_second <- FALSE
    gates <- list(
        first = function(pkg_data, branch, bioc_pkg_data, source_path)
            list(pass = FALSE, message = "first failed"),
        second = function(pkg_data, branch, bioc_pkg_data, source_path) {
            ran_second <<- TRUE
            list(pass = TRUE, message = NA_character_)
        }
    )
    suppressMessages(.evaluate_gates(gates, pkg_data, "release", NULL, NULL))
    expect_true(ran_second)
})

test_that(".evaluate_gates returns FALSE overall when any gate fails, even if later gates pass", {
    pkg_data <- .example_pkg_data()
    gates <- list(
        first = function(pkg_data, branch, bioc_pkg_data, source_path)
            list(pass = FALSE, message = "first failed"),
        second = function(pkg_data, branch, bioc_pkg_data, source_path)
            list(pass = TRUE, message = NA_character_)
    )
    result <- suppressMessages(.evaluate_gates(gates, pkg_data, "release", NULL, NULL))
    expect_false(result)
})

test_that(".evaluate_gates messages once per failing gate, not just the first", {
    pkg_data <- .example_pkg_data()
    gates <- list(
        first = function(pkg_data, branch, bioc_pkg_data, source_path)
            list(pass = FALSE, message = "first failed"),
        second = function(pkg_data, branch, bioc_pkg_data, source_path)
            list(pass = FALSE, message = "second failed")
    )
    msgs <- capture_messages(.evaluate_gates(gates, pkg_data, "release", NULL, NULL))
    combined <- paste(msgs, collapse = "")
    expect_match(combined, "first failed")
    expect_match(combined, "second failed")
})

test_that(".evaluate_platform passes when every criterion passes", {
    pkg_data <- .example_pkg_data()
    criteria <- default_criteria()$platform
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "linux-release-x86_64", ]
    expect_true(.evaluate_platform(criteria, pkg_data, "release", NULL, row))
})

test_that(".evaluate_platform fails and messages when a criterion fails", {
    pkg_data <- .example_pkg_data()
    criteria <- default_criteria()$platform
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "linux-release-x86_64", ]
    row[["check"]] <- "ERROR"
    expect_message(
        result <- .evaluate_platform(criteria, pkg_data, "release", NULL, row),
        "propagation check failed"
    )
    expect_false(result)
})

test_that(".evaluate_platform catches an error from a criterion", {
    pkg_data <- .example_pkg_data()
    criteria <- list(
        broken = function(pkg_data, branch, bioc_pkg_data, row)
            stop("something went wrong inside this criterion")
    )
    row <- pkg_data[["_jobs"]][pkg_data[["_jobs"]][["config"]] == "linux-release-x86_64", ]
    expect_message(
        result <- .evaluate_platform(criteria, pkg_data, "release", NULL, row),
        "something went wrong inside this criterion"
    )
    expect_false(result)
})

test_that("check_propagation returns jobs with a propagate column", {
    skip_if_offline()
    dir <- .make_desc_dir("totallyNotARealPackageXyz", "1.2.0")
    jobs <- data.frame(
        config = "linux-release-x86_64",
        r = as.character(.branch_r_version("release")),
        check = "OK",
        stringsAsFactors = FALSE
    )
    args <- list(
        package = "totallyNotARealPackageXyz", universe = "bioc-release",
        jobs = jobs, source_path = dir
    )
    result <- check_propagation(args)
    expect_true("propagate" %in% colnames(result))
    expect_equal(nrow(result), nrow(jobs))
})

test_that("check_propagation sets propagate = FALSE for every row when a gate fails", {
    skip_if_offline()
    dir <- .make_desc_dir(
        "totallyNotARealPackageXyz", "1.2.0",
        extra = list(Remotes = "github::user/pkg")
    )
    jobs <- data.frame(
        config = "linux-release-x86_64",
        r = as.character(.branch_r_version("release")),
        check = "OK",
        stringsAsFactors = FALSE
    )
    args <- list(
        package = "totallyNotARealPackageXyz", universe = "bioc-release",
        jobs = jobs, source_path = dir
    )
    result <- check_propagation(args)
    expect_false(any(result[["propagate"]]))
})

test_that("check_propagation respects a caller-supplied criteria", {
    skip_if_offline()
    dir <- .make_desc_dir("totallyNotARealPackageXyz", "1.2.0")
    jobs <- data.frame(
        config = "linux-release-x86_64",
        r = as.character(.branch_r_version("release")),
        check = "OK",
        stringsAsFactors = FALSE
    )
    always_fail_gate <- list(gates = list(
        always_fail = function(pkg_data, branch, bioc_pkg_data, source_path)
            list(pass = FALSE, message = "forced failure")
    ), platform = default_criteria()$platform)
    args <- list(
        package = "totallyNotARealPackageXyz", universe = "bioc-release",
        jobs = jobs, source_path = dir, criteria = always_fail_gate
    )
    result <- check_propagation(args)
    expect_false(any(result[["propagate"]]))
})

test_that("check_propagation sets propagate = NA for non-platform rows (source, bioc-checks, wasm)", {
    skip_if_offline()
    dir <- .make_desc_dir("totallyNotARealPackageXyz", "1.2.0")
    jobs <- data.frame(
        config = c("source", "bioc-checks", "wasm-release", "linux-release-x86_64"),
        r = as.character(.branch_r_version("release")),
        check = "OK",
        stringsAsFactors = FALSE
    )
    args <- list(
        package = "totallyNotARealPackageXyz", universe = "bioc-release",
        jobs = jobs, source_path = dir
    )
    result <- check_propagation(args)
    expect_true(is.na(result[["propagate"]][result[["config"]] == "source"]))
    expect_true(is.na(result[["propagate"]][result[["config"]] == "bioc-checks"]))
    expect_true(is.na(result[["propagate"]][result[["config"]] == "wasm-release"]))
    expect_false(is.na(result[["propagate"]][result[["config"]] == "linux-release-x86_64"]))
})

test_that("check_propagation sets propagate = NA for a channel that doesn't match the branch", {
    skip_if_offline()
    dir <- .make_desc_dir("totallyNotARealPackageXyz", "1.2.0")
    jobs <- data.frame(
        config = c(
            "linux-release-x86_64", "linux-devel-x86_64", "windows-oldrel-x86_64"
        ),
        r = as.character(.branch_r_version("release")),
        check = "OK",
        stringsAsFactors = FALSE
    )
    args <- list(
        package = "totallyNotARealPackageXyz", universe = "bioc-release",
        jobs = jobs, source_path = dir
    )
    result <- check_propagation(args)
    expect_false(is.na(result[["propagate"]][result[["config"]] == "linux-release-x86_64"]))
    expect_true(is.na(result[["propagate"]][result[["config"]] == "linux-devel-x86_64"]))
    expect_true(is.na(result[["propagate"]][result[["config"]] == "windows-oldrel-x86_64"]))
})

test_that("check_propagation's NA rows stay NA even when a gate fails", {
    skip_if_offline()
    dir <- .make_desc_dir(
        "totallyNotARealPackageXyz", "1.2.0",
        extra = list(Remotes = "github::user/pkg")
    )
    jobs <- data.frame(
        config = c("wasm-release", "linux-release-x86_64"),
        r = as.character(.branch_r_version("release")),
        check = "OK",
        stringsAsFactors = FALSE
    )
    args <- list(
        package = "totallyNotARealPackageXyz", universe = "bioc-release",
        jobs = jobs, source_path = dir
    )
    result <- check_propagation(args)
    expect_true(is.na(result[["propagate"]][result[["config"]] == "wasm-release"]))
    expect_false(result[["propagate"]][result[["config"]] == "linux-release-x86_64"])
})
