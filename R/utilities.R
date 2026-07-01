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
