test_that(".lookup_views_version finds a known package", {
    views <- data.frame(Package = "foo", Version = "1.2.0")
    expect_equal(.lookup_views_version(views, "foo"), "1.2.0")
})

test_that(".lookup_views_version returns NA for an unknown package", {
    views <- data.frame(Package = "foo", Version = "1.2.0")
    expect_true(is.na(.lookup_views_version(views, "bar")))
})

test_that(".lookup_views_version returns NA when views is NULL", {
    expect_true(is.na(.lookup_views_version(NULL, "foo")))
})
