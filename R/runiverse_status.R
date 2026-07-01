#' @title Check the status of a package in the R-Universe
#'
#' @examplesIf interactive()
#' runiverse_status("BiocCheck")
#' @export
runiverse_status <- function(pkgName) {
    rver <- BiocManager:::.version_field("R")
    rver[, 3L] <- 0L

    if (nzchar(system.file(package = pkgName)))
        desc <- system.file("DESCRIPTION", package = pkgName) |>
            read.dcf()
    else
        desc <- gh::gh(
            "GET /repos/{owner}/{repo}/contents/{path}",
            owner = "bioconductor-source",
            repo = pkgName,
            path = "DESCRIPTION"
        ) |>
            `[[`(x = _, i = "content") |>
            base64enc::base64decode() |>
            rawToChar() |>
            textConnection() |>
            read.dcf()

    results <- glue::glue(
        .BIOC_UNIVERSE_URL, "/{pkgName}"
    ) |>
        rjsoncons::j_pivot(
            path = "_jobs[]", as = "data.frame"
        ) |>
        .filter_r_ver() |>
        .filter_other_checks() |>
        .filter_unsupported(desc)

    statuses <- results[["check"]] |>
        unlist() |>
        unname()

    .validate_status(statuses)

    status_rules <- data.frame(
        status = c("ERROR", "FAIL", "CANCELLED", "WARNING", "NOTE", "OK"),
        handler = c(
            rep("stop", 3L),
            rep("message", 3L)
        )
    )

    matched <-
        status_rules[status_rules[["status"]] %in% statuses, , drop = FALSE]

    if (nrow(matched)) {
        handler <- get(matched[1L, "handler"], mode = "function")
        sQuote(matched[["status"]], FALSE) |>
            paste(... = _, collapse = ", ") |>
            handler(
                ... = _,
                " status found in r-universe checks for package: ",
                pkgName
            )
    }
}

.validate_status <- function(statuses) {
    valid_statuses <- c("ERROR", "FAIL", "CANCELLED", "WARNING", "NOTE", "OK")
    if (!all(statuses %in% valid_statuses))
        warning(
            "Invalid status found in r-universe checks: ",
            paste(statuses, collapse = ", "),
            call. = FALSE
        )
}
