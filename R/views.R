#' @noRd
#' @title Look up a package's published version from a VIEWS table
#'
#' @param views A `data.frame` with `Package`/`Version` columns or `NULL`.
#' @param pkgName `character(1)`
#'
#' @return `character(1)` The published version, or `NA` if `views` is
#'   `NULL` or the package isn't in it (e.g. a brand-new package).
#'
#' @examples
#' views <- data.frame(Package = "foo", Version = "1.2.0")
#' .lookup_views_version(views, "foo")
#' .lookup_views_version(views, "bar")
#' .lookup_views_version(NULL, "foo")
.lookup_views_version <- function(views, pkgName) {
    if (is.null(views))
        return(NA_character_)
    idx <- match(pkgName, views[["Package"]])
    if (is.na(idx))
        return(NA_character_)
    views[["Version"]][idx]
}
