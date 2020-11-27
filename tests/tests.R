library(testthat)
library(fars)
#testthat::test_that("FARS test",{
#        expect_that(fars::fars_read_years(2015),is_a("list"))
#})
expect_that(fars_summarize_years(2015),is_a("tbl_df"))
expect_that(fars_read_years(2015),is_a("list"))
