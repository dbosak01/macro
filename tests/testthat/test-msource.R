
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

test_that("msource2: basic functionality.", {

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


test_that("msource3: file_out functionality.", {

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


test_that("msource4: if/then/else functionality.", {

  fl1 <- file.path(base_path, "programs/test2.R")
  fl2 <- file.path(base_path, "programs/test2_mod.R")

  res <- msource(fl1, file_out = fl2)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(resx, c("mpg", "cyl", "disp"))
  expect_equal(nrow(dat), 14)
  expect_equal(ncol(dat), 3)

})



test_that("msource5: if/then/else functionality.", {

  fl1 <- file.path(base_path, "programs/test3.R")
  fl2 <- file.path(base_path, "programs/test3_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  res <- msource(fl1, file_out = fl2)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(resx, "two")


})

test_that("msource6: nested if/then/else functionality.", {

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


test_that("msource7: assignments.", {

  fl1 <- file.path(base_path, "programs/test6.R")
  fl2 <- file.path(base_path, "programs/test6_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()
  e1$z. <- 4

  res <- msource(fl1, file_out = fl2, e1)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$v1, 1)
  expect_equal(e1$v2, 1.2)
  expect_equal(e1$v3, "Hello World!")
  expect_equal(e1$v4, as.Date("2025-07-15"))
  expect_equal(class(e1$v4), "Date")
  expect_equal(e1$v5, c(1, 2, 3, 4, 5))
  expect_equal(e1$v6, 4)
  expect_equal(e1$v7, 4)
  expect_equal(e1$v8, 5)
  expect_equal(e1$v9, 4)

})

test_that("msource8: test include.", {

  fl1 <- file.path(base_path, "programs/test7.R")
  fl2 <- file.path(base_path, "programs/test7_mod.R")
  l1 <- file.path(base_path, "programs/test0.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()
  e1$z. <- l1

  res <- msource(fl1, file_out = fl2, e1)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$a, 3)
  expect_equal(e1$b, 2)
  expect_equal(e1$c, 3)

})

test_that("msource9: test vector in if condition.", {

  fl1 <- file.path(base_path, "programs/test8.R")
  fl2 <- file.path(base_path, "programs/test8_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()
  e1$vars. <- c("age", "ageg")
  e1$v1. <- 2
  e1$v2. <- as.Date("2025-01-01")
  e1$v3. <- as.POSIXct("2025-09-17 15:25:18 EDT")
  e1$v4. <- as.POSIXlt("2025-09-17 15:25:18 EDT")

  res <- msource(fl1, file_out = fl2, e1)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$fnd, TRUE)
  expect_equal(e1$v1, 2)
  expect_equal(e1$v2, as.Date("2025-01-01"))
  expect_equal(e1$v3, as.POSIXct("2025-09-17 15:25:18 EDT"))
  expect_equal(e1$v4, as.POSIXlt("2025-09-17 15:25:18 EDT"))

})


# test_that("msource8: msource() do loop functionality.", {
#
#   fl1 <- file.path(base_path, "programs/test5.R")
#   fl2 <- file.path(base_path, "programs/test5_mod.R")
#
#   if (file.exists(fl2))
#     file.remove(fl2)
#
#   res <- msource(fl1, file_out = fl2)
#
#   eres <- file.exists(fl2)
#
#   expect_equal(eres, TRUE)
#   expect_equal(resx, "one")
#   expect_equal(resy, 2)
#
# })


