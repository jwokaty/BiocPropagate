#' @include api.R

#' @export
valid_version <- function(pkgName) {
    message("Checking for version number mismatch with r-universe...")

    pkg_version <- utils::packageVersion(pkgName) |> as.character()
    version_name <- BiocManager:::.version_field("BiocStatus") |>
        as.character()
    ru_version <- .get_ru_version(pkgName)

    ok_version <- TRUE
    if (identical(pkg_version, ru_version)) {
        message(
            "Version in r-universe (", version_name,
            ") matches package version: ", pkg_version
        )
    } else {
        warning(
            "Package version mismatch with r-universe (", version_name, "): ",
            "r-universe version: ", ru_version, "; package version: ",
            pkg_version,
            call. = FALSE
        )
        ok_version <- FALSE
    }
    ok_version
}

.check_incremental <- function(version1, version2) {
    version1 <- as.package_version(version1)
    version2 <- as.package_version(version2)

    if (version1 <= version2)
        return(FALSE)

    version2[, 3L] <- as.numeric(version2[, 3L]) + 1L

    identical(
        version1, version2
    )
}

.check_greater <- function(version1, version2) {
    version1 <- as.package_version(version1)
    version2 <- as.package_version(version2)

    version1 > version2
}

.check_major_increment <- function(version1, version2) {
    version1 <- as.package_version(version1)
    version2 <- as.package_version(version2)

    if (version1 <= version2)
        return(FALSE)

    version1[, 2L] == '99' &&
        identical(version1[, 1L], version2[, 1L])
}

#' @export
is_incremental <- function(pkgName) {
    message("Checking for valid version bump compared to r-universe version...")

    pkg_version <- utils::packageVersion(pkgName) |> as.character()
    version_name <- BiocManager:::.version_field("BiocStatus") |>
        as.character()
    ru_version <- .get_ru_version(pkgName)

    if (
        .check_incremental(pkg_version, ru_version) ||
        .check_greater(pkg_version, ru_version) ||
        .check_major_increment(pkg_version, ru_version)
    ) {
        TRUE
    } else {
        warning(
            "Version bump is not valid compared to r-universe (",
            version_name, "): ",
            "r-universe version: ", ru_version, "; package version: ",
            pkg_version,
            call. = FALSE
        )
        FALSE
    }
}

