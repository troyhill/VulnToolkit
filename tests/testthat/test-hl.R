context("test-hl")
library(VulnToolkit)

test_that("fld.frq gives correct output", {
  expect_equal(fld.frq(2, 1:10, units = "percent"), 0.8)
  expect_error(fld.frq(2, "string")) 
})

test_that("fld.dur gives correct output", {
  expect_equal(fld.dur(2, 1:10), 0.8)
  expect_error(fld.dur(2, "string")) 
})
