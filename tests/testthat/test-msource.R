
base_path <- "c:\\packages\\macro\\tests\\testthat"

base_path <- "./"

dev <- FALSE



test_that("msource1: mreplace() basic functionality.", {

  is_let("#%let x = 3")
  is_let("#%let y = 2")

  ln <- "Y is y."

  res <- mreplace(ln)

  expect_equal(res, "Y is 2")

  is_let("#%let y = 3")
  ln <- "Y is y."

  res <- mreplace(ln)

  expect_equal(res, "Y is 3")


  ln <- "Y is z."

  res <- mreplace(ln)

  expect_equal(res, "Y is z.")


  ln <- "X is x."

  res <- mreplace(ln)

  expect_equal(res, "X is 3")


})

test_that("msource2: msource() basic functionality.", {

  fl <- file.path(base_path, "programs/test0.R")

  res <- msource(fl)

  expect_equal(TRUE, TRUE)

})


test_that("msource3: msource() basic functionality.", {

  fl <- file.path(base_path, "programs/test1.R")

  res <- msource(fl)

  expect_equal(TRUE, TRUE)

})



