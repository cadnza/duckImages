testthat::test_that("search works", {
    final <- imageSearch(
        "oranges",
        page = 1,
        type = "photo",
        lay = NA
    )
    testthat::expect_true(
        class(final[1]) == "character",
        info = "An NA result means that curl_download failed.",
        label = "Curl result"
    )
})
