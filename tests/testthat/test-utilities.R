MANIFEST <- "https://github.com/jwokaty/manifest"

test_that(".get_exemptions returns character(0) if no exemptions", {
    expect_equal(character(0),
                 .get_exemptions("package3", "devel", manifest = MANIFEST))
    expect_equal("nonexistant_gate",
                 .get_exemptions("package4", "devel", manifest = MANIFEST))
})

test_that(".get_exemptions clones the branch and returns exemptions", {
    expect_equal(c("no_merge_conflicts"),
                 .get_exemptions("package2", "devel", manifest = MANIFEST))
    expect_equal(c("no_large_files", "no_secrets", "no_merge_conflicts"),
                 .get_exemptions("package1", "devel", manifest = MANIFEST))
    expect_equal(c("no_large_files", "no_secrets"),
                 .get_exemptions("package1", "RELEASE_3_23", manifest = MANIFEST))
})
