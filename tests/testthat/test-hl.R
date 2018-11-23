context("test-hl")
library(VulnToolkit)

test_that("fld.frq gives correct output", {
  expect_equal(fld.frq(2, 1:10, units = "percent"), 0.8)
  expect_equal(fld.frq(2, 1:10, units = "tides"), 9)
  expect_error(fld.frq("string", 1:10)) 
  expect_error(fld.frq(2, "string"))
  expect_error(fld.frq(2, 1:10, units = "furlongs")) 
})

test_that("fld.dur gives correct output", {
  expect_equal(fld.dur(2, 1:10), 0.8)
  expect_error(fld.dur("string", 1:10)) 
  expect_error(fld.dur(2, "string")) 
})


test_that("HL gives correct output", {
  expect_error(HL("hello world"))
  expect_error(HL(1:10, "hello world")) 
  expect_equal(nrow(HL(rep(c(c(1:100), c(100:1)), times = 400)[1:8761], seq.POSIXt(from = ISOdate(1910,1,1), to = ISOdate(1911,1,1), by = "hours"))), 1868)
})
