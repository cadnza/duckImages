testthat::test_that("search works", {
	final <- imageSearch(
		"oranges",
		page=1,
		type=NA,
		lay=NA
	)
	print(final)
	testthat::expect_condition(
		final[1],
		class="logical",
		info="An NA result means that curl_download failed.",
		label="Curl result"
	)
})
