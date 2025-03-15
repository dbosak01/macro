
base_path <- "c:\\packages\\macro\\tests\\testthat\\"

base_path <- "./"

dev <- FALSE

test_that("msource1: msource() basic functionality.", {

  fl <- file.path(base_path, "./programs/test1.R")

  res <- msource(fl)

  expect_equal(TRUE, TRUE)

})
