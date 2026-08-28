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
    system2("git", c("clone", "--branch", branch, "--depth", "1", manifest,
            target_dir))
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
