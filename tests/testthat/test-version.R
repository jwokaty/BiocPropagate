test_that(".version_field_for resolves status tags and explicit versions", {
    r_release <- .version_field_for("R", "release")
    r_by_version <- .version_field_for(
        "R", as.character(.version_field_for("Bioc", "release"))
    )
    expect_equal(r_release, r_by_version)
    expect_false(is.na(r_release))
})

test_that(".version_field_for errors for an unmatched branch", {
    expect_error(.version_field_for("R", "99.99"))
})

test_that(".branch_r_version returns major.minor only", {
    v <- .branch_r_version("release")
    expect_s3_class(v, "package_version")
    expect_length(unclass(v)[[1L]], 2L)
})

test_that(".branch_bioc_version resolves a real Bioc version number", {
    v <- .branch_bioc_version("devel")
    expect_s3_class(v, "package_version")
})

test_that(".major_minor truncates to two components", {
    expect_equal(.major_minor("4.6.1"), package_version("4.6"))
    expect_equal(.major_minor("4.6"), package_version("4.6"))
})
