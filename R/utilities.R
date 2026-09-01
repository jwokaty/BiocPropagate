#' @importFrom BiocBaseUtils checkInstalled
.get_gh_desc <- function(package) {
    checkInstalled("gh")
    gh::gh(
        "GET /repos/{owner}/{repo}/contents/{path}",
        owner = "bioconductor-source",
        repo = package,
        path = "DESCRIPTION"
    ) |>
        `[[`(x = _, i = "content") |>
        base64enc::base64decode() |>
        rawToChar() |>
        textConnection() |>
        read.dcf()
}

.get_sys_desc <- function(package) {
    system.file("DESCRIPTION", package = package) |>
        read.dcf()
}

#' Get exemptions from manifest
#'
#' @param package name
#' @param branch Bioconductor git branch
#' @param target_dir (default tempfile()) path to clone repository
#' @param manifest (default .BIOCONDUCTOR_MANIFEST) repo url
#'
#' @returns list() of gate names package is exempt from or character(0)
#' @noRd
.get_exemptions <- function(package,
                            branch,
                            target_dir = tempfile(),
                            manifest = .BIOCONDUCTOR_MANIFEST) {
    result <- system2("git", c("clone", "--branch", branch, "--depth", "1", manifest, target_dir),
                      stdout = TRUE, stderr = TRUE)
    status <- attr(result, "status")
    if (!is.null(status) && status != 0)
        message("git clone failed (status ", status, "): ", paste(result, collapse = "\n"))
    exemptions <- read.dcf(paste(target_dir, "exemptions.txt", sep = "/")) |>
        as.data.frame()
    package_exemptions <- exemptions[exemptions$Package == package, ]$Exemptions
    tryCatch({
        strsplit(package_exemptions, split = ",")[[1]] |>
            trimws()
        },
        error = function(e) {
            character(0)
    })
}

#' @noRd
#' @title Test-fixture helper: a minimal r-universe payload
#'
#' @return A named list shaped like a real r-universe package payload.
.example_pkg_data <- function() {
    r_ver <- paste0(as.character(.branch_r_version("release")), ".0")
    list(
        Package = "examplePkg",
        Version = "1.2.0",
        `_jobs` = data.frame(
            config = c(
                "source", "bioc-checks",
                "macos-release-arm64",
                "windows-release-x86_64", "windows-release-arm64",
                "linux-release-x86_64"
            ),
            r = r_ver,
            check = "OK",
            stringsAsFactors = FALSE
        )
    )
}

#' @noRd
#' @title Bioconductor branch to Bioconductor git branch
#'
#' @return "devel" or "RELEASE_X_Y"
.bioc_branch_to_git_branch <- function(branch) {
    ifelse(branch == "devel",
           "devel",
           paste0("RELEASE_", sub("\\.", "_", .branch_bioc_version(branch)))
    )
}
