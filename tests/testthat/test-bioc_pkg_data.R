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

test_that(".get_bioc_pkg_data fetches real Package/Version data via available.packages()", {
    skip_if_offline()
    data <- .get_bioc_pkg_data("release")
    expect_true(all(c("Package", "Version") %in% colnames(data)))
    expect_gt(nrow(data), 0L)
})

test_that(".get_bioc_pkg_data errors on an unknown type", {
    expect_error(.get_bioc_pkg_data("release", type = "not-a-type"))
})

test_that(".get_books_packages fetches real Package/Version data", {
    skip_if_offline()
    data <- .get_books_packages("release")
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
    # .branch_bioc_version()/.branch_r_version() still need network before
    # the codename lookup is even reached.
    skip_if_offline()
    expect_error(
        .get_bioc_platform_pkg_data(
            "release", os = "macos", arch = "arm64", codenames = list()
        ),
        "no macOS codename known"
    )
})

test_that(".get_bioc_platform_pkg_data respects a codenames override", {
    skip_if_offline()
    rver <- as.character(.branch_r_version("release"))
    custom <- setNames(list(c(arm64 = "totallymadeupcodename")), rver)
    expect_warning(
        .get_bioc_platform_pkg_data(
            "release", os = "macos", arch = "arm64", codenames = custom
    ))
})

test_that(".get_bioc_platform_pkg_data errors for an unsupported os", {
    skip_if_offline()
    expect_error(
        .get_bioc_platform_pkg_data("release", os = "linux"), "unsupported os"
    )
})

test_that(".get_all_bioc_pkg_data returns source and platform sub-lists with the right names (explicit type)", {
    skip_if_offline()
    data <- .get_all_bioc_pkg_data("release", "BiocCheck", type = "software")
    expect_named(data, c("source", "platform"))
    expect_named(data$platform, c("windows", "macos-arm64", "macos-x86_64"))
    expect_true(all(c("Package", "Version") %in% colnames(data$source)))
    expect_true(all(c("Package", "Version") %in% colnames(data$platform$windows)))
})

test_that(".get_all_bioc_pkg_data auto-detects type when not supplied", {
    skip_if_offline()
    # No type given -- must detect "software" itself via .detect_bioc_pkg_type().
    data <- .get_all_bioc_pkg_data("release", "BiocCheck")
    expect_true("BiocCheck" %in% data$source[["Package"]])
})

test_that(".get_all_bioc_pkg_data returns NULL source and empty platform when the package isn't found in any type", {
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
