
base_path <- "c:\\packages\\macro\\tests\\testthat"

base_path <- "./"

dev <- FALSE



test_that("symtable01: symclear() works as expected.", {


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

  res2 <- symclear()

  expect_equal(res2, 2) # clears &x and &y

})

test_that("symtable02: symtable() works as expected.", {


  fl <- file.path(base_path, "programs/test25.R")
  fl2 <- file.path(base_path, "programs/test25_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- 1

  res <- msource(fl, file_out = fl2,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)

  res2 <- symtable()

  # Check normal printing
  print(res2)

  # Check verbose printing
  print(res2, verbose = TRUE)

  expect_equal(length(res2), 2)
  expect_equal(length(res2$variables), 3)
  expect_equal(length(res2$functions), 3)
  expect_equal(names(res2$functions), c("fork", "bork", "sammy"))

})


test_that("symtable03: symget() works as expected.", {


  fl <- file.path(base_path, "programs/test1.R")
  fl2 <- file.path(base_path, "programs/test1_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)


  res <- msource(fl, file_out = fl2)

  eres <- file.exists(fl2)


  expect_equal(eres, TRUE)

  expect_equal(symget("x"), "3")
  expect_equal(symget("y"), "4")
  expect_equal(symget("z"), "6")
  expect_equal(symget("q"), "floor(2 + 2.5)")
  expect_equal(symget("f"), "TRUE")
  expect_equal(symget("g"), "FALSE")


})

test_that("symtable04: symput() works as expected.", {


  fl <- file.path(base_path, "programs/test24.R")
  fl2 <- file.path(base_path, "programs/test24_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- 1
  symput("q", 3)

  res <- msource(fl, file_out = fl2, clear = FALSE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)

  res1 <- symget("q")

  expect_equal(res1, "3")

  res3 <- symtable()

  expect_equal("&q" %in% names(res3$variables), TRUE)

  symput("q")

  res4 <- symtable()

  expect_equal("&q" %in% names(res4$variables), FALSE)

})

test_that("symtable05: symtable() with vectors works as expected.", {


  fl <- file.path(base_path, "programs/test25.R")
  fl2 <- file.path(base_path, "programs/test25_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- c(1, 2, 3)

  res <- msource(fl, file_out = fl2,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)

  res2 <- symtable()

  # Check normal printing
  print(res2)

  # Check verbose printing
  print(res2, verbose = TRUE)

  expect_equal(length(res2), 2)
  expect_equal(length(res2$variables), 3)
  expect_equal(length(res2$functions), 3)
  expect_equal(names(res2$functions), c("fork", "bork", "sammy"))

})
