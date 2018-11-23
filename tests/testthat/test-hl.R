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
  expect_equal(fld.dur(z = 2, level = 1:10), 0.8)
  expect_error(fld.dur(z = "string", level = 1:10)) 
  expect_error(fld.dur(z = 2, level = "string")) 
})

test_that("HL gives correct output", {
  expect_error(HL("hello world"))
  expect_error(HL(1:10, "hello world")) 
  expect_equal(nrow(HL(rep(c(c(1:100), c(100:1)), times = 400)[1:8761], seq.POSIXt(from = ISOdate(1910,1,1), to = ISOdate(1911,1,1), by = "hours"))), 1868)
})


test_that("wave.dur gives correct output", {
  expect_error(wave.dur(elevation = c(), level = 1:10)) 
  expect_equal(sum(wave.dur(level = 2, elevation = 1:10)), 1)
  expect_equal(sum(wave.dur(level = 2:10, elevation = 1:10)), 1)
  expect_equal(sum(wave.dur(level = c(NA, 2:10), elevation = 1:10)), 1)
})


test_that("psmsl error checking", {
  expect_error(psmsl(type = "string")) 
  expect_error(psmsl(interval = "string")) 
})
