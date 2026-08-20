test_that(".lookup_bioc_pkg_version finds a known package", {
    bioc_pkg_data <- data.frame(Package = "foo", Version = "1.2.0")
    expect_equal(.lookup_bioc_pkg_version(bioc_pkg_data, "foo"), "1.2.0")
})

test_that(".lookup_bioc_pkg_version returns NA for an unknown package", {
    bioc_pkg_data <- data.frame(Package = "foo", Version = "1.2.0")
    expect_true(is.na(.lookup_bioc_pkg_version(bioc_pkg_data, "bar")))
})

test_that(".lookup_bioc_pkg_version returns NA when bioc_pkg_data is NULL", {
    expect_true(is.na(.lookup_bioc_pkg_version(NULL, "foo")))
})

test_that(".get_bioc_pkg_data fetches real Package/Version data", {
    skip_if_offline()
    data <- .get_bioc_pkg_data("release")
    expect_true(all(c("Package", "Version") %in% colnames(data)))
    expect_gt(nrow(data), 0L)
})

test_that(".get_bioc_pkg_data errors on an unknown type", {
    expect_error(.get_bioc_pkg_data("release", type = "not-a-type"))
})

test_that(".get_bioc_pkg_data works for books via .PACKAGE_TYPE_PATH", {
    skip_if_offline()
    data <- .get_bioc_pkg_data("release", type = "books")
    expect_true(all(c("Package", "Version") %in% colnames(data)))
})

test_that(".get_bioc_platform_pkg_data fetches real windows data", {
    skip_if_offline()
    data <- .get_bioc_platform_pkg_data("release", os = "windows")
    expect_true(all(c("Package", "Version") %in% colnames(data)))
})

test_that(".get_bioc_platform_pkg_data fetches real macos-arm64 data", {
    skip_if_offline()
    data <- .get_bioc_platform_pkg_data("release", os = "macos", arch = "arm64")
    expect_true(all(c("Package", "Version") %in% colnames(data)))
})

test_that(".get_bioc_platform_pkg_data fetches real macos-x86_64 data (different codename than arm64)", {
    skip_if_offline()
    data <- .get_bioc_platform_pkg_data("release", os = "macos", arch = "x86_64")
    expect_true(all(c("Package", "Version") %in% colnames(data)))
})

test_that(".get_bioc_platform_pkg_data errors for an unknown macOS codename", {
    skip_if_offline()
    expect_error(
        .get_bioc_platform_pkg_data(
            "release", os = "macos", arch = "arm64", codenames = list()
        ),
        "no macOS codename known"
    )
})

test_that(".get_bioc_platform_pkg_data errors for an unsupported os", {
    skip_if_offline()
    expect_error(
        .get_bioc_platform_pkg_data("release", os = "linux"), "unsupported os"
    )
})

test_that(".get_all_bioc_pkg_data returns source and platform sub-lists (explicit type)", {
    skip_if_offline()
    data <- .get_all_bioc_pkg_data("release", "BiocCheck")
    expect_named(data, c("source", "platform"))
    expect_named(data$platform, c("windows", "macos-arm64", "macos-x86_64"))
    expect_true(all(c("Package", "Version") %in% colnames(data$source)))
})

test_that(".get_all_bioc_pkg_data auto-detects type when not supplied", {
    skip_if_offline()
    data <- .get_all_bioc_pkg_data("release", "BiocCheck")
    expect_true("BiocCheck" %in% data$source[["Package"]])
})

test_that(".get_all_bioc_pkg_data returns NULL source when the package isn't found in any type", {
    skip_if_offline()
    data <- .get_all_bioc_pkg_data("release", "totally-not-a-real-package-xyz")
    expect_null(data$source)
    expect_equal(data$platform, list())
})

test_that(".detect_bioc_pkg_type finds a known software package", {
    skip_if_offline()
    expect_equal(.detect_bioc_pkg_type("release", "BiocCheck"), "software")
})

test_that(".detect_bioc_pkg_type returns NA when the package isn't found in any type", {
    skip_if_offline()
    expect_true(is.na(.detect_bioc_pkg_type("release", "totally-not-a-real-package-xyz")))
})

test_that(".universe_to_branch resolves known universes", {
    expect_equal(.universe_to_branch("bioc"), "devel")
    expect_equal(.universe_to_branch("bioc-release"), "release")
})

test_that(".universe_to_branch errors for an unknown universe", {
    expect_error(.universe_to_branch("not-a-universe"),
                 "unknown universe 'not-a-universe' -- update .UNIVERSE_BRANCH_MAP")
})

test_that(".previous_bioc_version(\"devel\") resolves to release under normal conditions", {
    skip_if_offline()
    prev <- .previous_bioc_version("devel")
    expect_true(is.character(prev))
    expect_lt(package_version(prev), package_version(.branch_bioc_version("devel")))
})
