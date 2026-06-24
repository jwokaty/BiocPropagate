.BIOC_UNIVERSE_URL <- "https://bioc.r-universe.dev/api/packages"

.get_runi_meta <- function(pkgName) {
    glue::glue(
        .BIOC_UNIVERSE_URL, "/{pkgName}"
    ) |>
        jsonlite::fromJSON()
}

.get_ru_version <- function(pkgName) {
    ru_meta <- .get_runi_meta(pkgName = pkgName)
    mini_ver <- ru_meta[["_bioc"]]
    if (is.null(mini_ver)) {
        stop(
            "No Bioconductor information in r-universe for package: ", pkgName,
            call. = FALSE
        )
        return(invisible(NULL))
    }

    bioc_ver <- BiocManager::version() |> as.character()
    matched_ver <- match(bioc_ver, mini_ver[["bioc"]])

    if (is.na(matched_ver)) {
        stop(
            "No version in r-universe matches Bioconductor version: ", bioc_ver,
            call. = FALSE
        )
        return(invisible(NULL))
    }

    mini_ver[matched_ver, "version"] |>
        as.character()
}
