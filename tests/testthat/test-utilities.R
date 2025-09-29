
base_path <- "c:\\packages\\macro\\tests\\testthat"

base_path <- "./"

dev <- FALSE

test_that("utils1: is_let() basic functionality.", {


  l1 <- "let x = 1"

  res <- is_let(l1, TRUE)

  res

  expect_equal(res, FALSE)

  l2 <- "#%let x = 1"

  res <- is_let(l2, TRUE)

  res
  e[["x."]]

  expect_equal(res, TRUE)
  expect_equal(e[["x."]], '1')


  l3 <- "#%let x = 2"

  res <- is_let(l3, TRUE)

  res
  e[["x."]]

  expect_equal(res, TRUE)
  expect_equal(e[["x."]], '2')


  l4 <- "#%let x = 'hello'"

  res <- is_let(l4, TRUE)

  res
  e[["x."]]

  expect_equal(res, TRUE)
  expect_equal(e[["x."]], "'hello'")

})

test_that("utils2: is_let() sub_func functionality.", {


  l1 <- "#%let x = floor(1 + 2.5)"

  res <- is_let(l1, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$x., 'floor(1 + 2.5)')


  l2 <- "#%let y = %sysfunc(floor(1 + 2.5))"

  l3 <- sub_funcs(l2)
  res <- is_let(l3, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$y., "3")

  l4 <- "#%let z = %symexist(y)"

  l5 <- sub_funcs(l4)
  res <- is_let(l5, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$z., "TRUE")

  l6 <- "#%let w = %sysfunc(floor(1 + 2.5) / 1.3, '%.3f')"

  l7 <- sub_funcs(l6)
  res <- is_let(l7, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$w., "2.308")


})


test_that("utils3: is_if() basic functionality.", {

  l1 <- "#%if 1 == 1"

  res <- is_if(l1)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), TRUE)

  l1 <- "#%if 1 == 2"

  res <- is_if(l1)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), FALSE)


  l3 <- "#%if (1 == 1)"

  res <- is_if(l3)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), TRUE)

  l4 <- "#%if 1 == '1'"

  res <- is_if(l4)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), TRUE)


  l4 <- "#%elseif 1 == '1'"

  res <- is_if(l4)

  res

  expect_equal(as.logical(res), FALSE)


})


test_that("utils4: is_end() basic functionality.", {

  l1 <- "#%if 1 == 1"

  res <- is_end(l1)

  res

  expect_equal(as.logical(res), FALSE)

  l2 <- "#%end"

  res <- is_end(l2)

  res

  expect_equal(as.logical(res), TRUE)

  l3 <- "   #%end   "

  res <- is_end(l3)

  res

  expect_equal(as.logical(res), TRUE)


})


test_that("utils5: is_else() basic functionality.", {

  l1 <- "#%if 1 == 1"

  res <- is_else(l1)

  res

  expect_equal(as.logical(res), FALSE)

  l2 <- "#%else"

  res <- is_else(l2)

  res

  expect_equal(as.logical(res), TRUE)

  l3 <- "   #%else   "

  res <- is_else(l3)

  res

  expect_equal(as.logical(res), TRUE)


})


test_that("utils6: is_elseif() basic functionality.", {

  l1 <- "#%elseif 1 == 1"

  res <- is_elseif(l1)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), TRUE)

  l1 <- "#%elseif 1 == 2"

  res <- is_elseif(l1)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), FALSE)


  l3 <- "#%elseif (1 == 1)"

  res <- is_elseif(l3)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), TRUE)

  l4 <- "#%elseif 1 == '1'"

  res <- is_elseif(l4)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), TRUE)


  l4 <- "#%elseif 1 == '2'"

  res <- is_elseif(l4)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "value"), FALSE)

})



test_that("utils7: sub_funcs() basic functionality.", {

  ln <- "%let x <- %sysfunc(1 + 4)"

  res <- sub_funcs(ln)

  res

  expect_equal(res, "%let x <- 5")


  ln <- "%let x <- 1"

  res <- sub_funcs(ln)

  res

  expect_equal(res, "%let x <- 1")

})



test_that("utils8: is_include() basic functionality.", {

  ln <- "#%include 'c:/fork/sammy.txt'"


  res <- is_include(ln)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "path"), "c:/fork/sammy.txt")

  ln <- '#%include "c:/fork/sammy.txt"'


  res <- is_include(ln)

  res

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "path"), "c:/fork/sammy.txt")


  ln <- "#%qualude 'c:/fork/sammy.txt'"

  res <- is_include(ln)

  res

  expect_equal(as.logical(res), FALSE)

})


test_that("utils9: get_include() basic functionality.", {

  pth <- file.path(base_path, "programs/test0.R")


  lns <- get_include(pth)

  lns

  expect_equal(length(lns) > 0, TRUE)
  expect_equal(is.character(lns), TRUE)

})

# test_that("utils8: is_do() basic functionality.", {
#
#   l1 <- "#%do x = 1 %to 3"
#
#   res <- is_do(l1)
#
#   expect_equal(as.logical(res), TRUE)
#   expect_equal(attr(res, "variable"), "x")
#   expect_equal(attr(res, "start"), 1)
#   expect_equal(attr(res, "end"), 3)
#
#
#   vct <- c(1, 2, 3, 4)
#   l1 <- "#%do x = 2 %to length(vct)"
#
#   res <- is_do(l1)
#
#   expect_equal(as.logical(res), TRUE)
#   expect_equal(attr(res, "variable"), "x")
#   expect_equal(attr(res, "start"), 2)
#   expect_equal(attr(res, "end"), 4)
#
#
#   vct <- c(1, 2, 3, 4)
#   l1 <- "#%do x = 2 to length(vct)"
#
#   expect_error(is_do(l1))
#
#
#
# })
#
# test_that("utils8: do_info() basic functionality.", {
#
#   l1 <- "#%do x = 1 %to 3"
#
#   r1 <- is_do(l1)
#
#   res <- do_info(1, 2, attr(r1, "variable"),
#                  attr(r1, "start"),
#                  attr(r1, "end"))
#
#   expect_equal(res$lvl, 1)
#   expect_equal(res$dolvl, 2)
#   expect_equal(res$var, "x")
#   expect_equal(res$start, 1)
#   expect_equal(res$end, 3)
#
#
# })


