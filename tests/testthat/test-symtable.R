
base_path <- "c:\\packages\\macro\\tests\\testthat"

base_path <- "./"

dev <- FALSE



test_that("symtable01: mclear() works as expected.", {


  fl <- file.path(base_path, "programs/test24.R")
  fl2 <- file.path(base_path, "programs/test24_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- 1

  res <- msource(fl, file_out = fl2,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  hasa <- exists("z")

  expect_equal(eres, TRUE)
  expect_equal(hasa, TRUE)

  res2 <- mclear()

  expect_equal(res2, 2) # clears &x and &y

})

test_that("symtable02: mput() works as expected.", {


  fl <- file.path(base_path, "programs/test25.R")
  fl2 <- file.path(base_path, "programs/test25_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- 1

  res <- msource(fl, file_out = fl2,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)

  res2 <- mput()

  # Check normal printing
  print(res2)

  # Check verbose printing
  print(res2, verbose = TRUE)

  expect_equal(length(res2), 2)
  expect_equal(length(res2$variables), 3)
  expect_equal(length(res2$functions), 3)
  expect_equal(names(res2$functions), c("fork", "bork", "sammy"))

})


test_that("symtable03: mget() works as expected.", {


  fl <- file.path(base_path, "programs/test1.R")
  fl2 <- file.path(base_path, "programs/test1_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)


  res <- msource(fl, file_out = fl2)

  eres <- file.exists(fl2)


  expect_equal(eres, TRUE)

  expect_equal(mget("x"), "3")
  expect_equal(mget("y"), "4")
  expect_equal(mget("z"), "6")
  expect_equal(mget("q"), "floor(2 + 2.5)")
  expect_equal(mget("f"), "TRUE")
  expect_equal(mget("g"), "FALSE")


})

test_that("symtable04: mset() works as expected.", {


  fl <- file.path(base_path, "programs/test24.R")
  fl2 <- file.path(base_path, "programs/test24_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- 1
  mset("q", 3)

  res <- msource(fl, file_out = fl2, clear = FALSE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)

  res1 <- mget("q")

  expect_equal(res1, "3")

  res3 <- mput()

  expect_equal("&q" %in% names(res3$variables), TRUE)

})

test_that("symtable05: mput() with vectors works as expected.", {


  fl <- file.path(base_path, "programs/test25.R")
  fl2 <- file.path(base_path, "programs/test25_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- c(1, 2, 3)

  res <- msource(fl, file_out = fl2,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)

  res2 <- mput()

  # Check normal printing
  print(res2)

  # Check verbose printing
  print(res2, verbose = TRUE)

  expect_equal(length(res2), 2)
  expect_equal(length(res2$variables), 3)
  expect_equal(length(res2$functions), 3)
  expect_equal(names(res2$functions), c("fork", "bork", "sammy"))

})
