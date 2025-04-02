
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
  fl2 <- file.path(base_path, "programs/test0_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  ne <- new.env()

  # No environment passed
  res <- msource(fl, file_out = fl2, envir = ne)

  eres <- file.exists(fl2)
  hasa <- exists("a")

  expect_equal(eres, TRUE)
  expect_equal(hasa, FALSE)
  expect_equal(ne$a, 3)
})


test_that("msource3: msource() file_out functionality.", {

  fl1 <- file.path(base_path, "programs/test1.R")
  fl2 <- file.path(base_path, "programs/test1_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  res <- msource(fl1, file_out = fl2)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(a, 3)
  expect_equal(b, 4)
  expect_equal(d, 3)

})


test_that("msource4: msource() if/then/else functionality.", {

  fl1 <- file.path(base_path, "programs/test2.R")
  fl2 <- file.path(base_path, "programs/test2_mod.R")

  res <- msource(fl1, file_out = fl2)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(resx, c("mpg", "cyl", "disp"))
  expect_equal(nrow(dat), 14)
  expect_equal(ncol(dat), 3)

})



test_that("msource5: msource() if/then/else functionality.", {

  fl1 <- file.path(base_path, "programs/test3.R")
  fl2 <- file.path(base_path, "programs/test3_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  res <- msource(fl1, file_out = fl2)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(resx, "two")


})

test_that("msource6: msource() nested if/then/else functionality.", {

  fl1 <- file.path(base_path, "programs/test4.R")
  fl2 <- file.path(base_path, "programs/test4_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  res <- msource(fl1, file_out = fl2)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(resx, "one")
  expect_equal(resy, 2)

})

