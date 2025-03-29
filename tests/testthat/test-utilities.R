

dev <- FALSE

test_that("utils1: is_let() basic functionality.", {


  l1 <- "let x = 1"

  res <- is_let(l1)

  res

  expect_equal(res, FALSE)

  l2 <- "#%let x = 1"

  res <- is_let(l2)

  res
  e[["x."]]

  expect_equal(res, TRUE)
  expect_equal(e[["x."]], 1)


  l3 <- "#%let x = 2"

  res <- is_let(l3)

  res
  e[["x."]]

  expect_equal(res, TRUE)
  expect_equal(e[["x."]], 2)


  l4 <- "#%let x = 'hello'"

  res <- is_let(l4)

  res
  e[["x."]]

  expect_equal(res, TRUE)
  expect_equal(e[["x."]], 'hello')

})

test_that("utils2: is_let() sysfunc functionality.", {


  l1 <- "#%let x = floor(1 + 2.5)"

  res <- is_let(l1)

  res

  expect_equal(res, TRUE)
  expect_equal(e$x., 3)


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



test_that("utils7: sub_sysfunc() basic functionality.", {

  ln <- "%let x <- %sysfunc(1 + 4)"

  res <- sub_sysfunc(ln)

  res

  expect_equal(res, "%let x <- 5")


  ln <- "%let x <- 1"

  res <- sub_sysfunc(ln)

  res

  expect_equal(res, "%let x <- 1")

})

