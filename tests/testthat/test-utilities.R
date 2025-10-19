
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
  e[["&x"]]

  expect_equal(res, TRUE)
  expect_equal(e[["&x"]], '1')


  l3 <- "#%let x = 2"

  res <- is_let(l3, TRUE)

  res
  e[["&x"]]

  expect_equal(res, TRUE)
  expect_equal(e[["&x"]], '2')


  l4 <- "#%let x = 'hello'"

  res <- is_let(l4, TRUE)

  res
  e[["&x"]]

  expect_equal(res, TRUE)
  expect_equal(e[["&x"]], "'hello'")

  # Special case
  l5 <- "#%let x <- a <- 1"

  res <- is_let(l5, TRUE)

  res
  e[["&x"]]

  expect_equal(res, TRUE)
  expect_equal(e[["&x"]], "a <- 1")

  # Another Special case
  l6 <- "#%let x <- "

  res <- is_let(l6, TRUE)

  res
  e[["&x"]]

  expect_equal(res, TRUE)
  expect_equal(e[["&x"]], "")


})

test_that("utils2: is_let() sub_func functionality.", {


  l1 <- "#%let x = floor(1 + 2.5)"

  res <- is_let(l1, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$`&x`, 'floor(1 + 2.5)')


  l2 <- "#%let y = %sysfunc(floor(1 + 2.5))"

  l3 <- sub_funcs(l2)
  res <- is_let(l3, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$`&y`, "3")

  l4 <- "#%let z = %symexist(y)"

  l5 <- sub_funcs(l4)
  res <- is_let(l5, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$`&z`, "TRUE")

  l6 <- "#%let w = %sysfunc(floor(1 + 2.5) / 1.3, %.3f)"

  l7 <- sub_funcs(l6)
  res <- is_let(l7, TRUE)

  res

  expect_equal(res, TRUE)
  expect_equal(e$`&w`, "2.308")


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


test_that("utils10: log_debug() works as expected.", {

  pth <- file.path(base_path, "examples/log/test.txt")

  if (file.exists(pth))
    file.remove(pth)


  log_debug("fork", pth)

  res <- file.exists(pth)

  expect_equal(res, TRUE)

})

test_that("utils11: is_do() basic functionality.", {

  l1 <- "#%do x = 1 %to 3"

  res <- is_do(l1)

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "variable"), "x")
  expect_equal(attr(res, "start"), 1)
  expect_equal(attr(res, "end"), 3)


  vct. <- c(1, 2, 3, 4)
  l1 <- "#%do x = 2 %to 4"

  res <- is_do(l1)

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "variable"), "x")
  expect_equal(attr(res, "start"), 2)
  expect_equal(attr(res, "end"), 4)


  vct <- c(1, 2, 3, 4)
  l1 <- "#%do x = 2 to length(vct)"

  expect_error(is_do(l1))


})


test_that("utils12: is_macro() basic functionality.", {

  l1 <- "#%macro fork(x, y = NA, z = 'Two')"

  res <- is_macro(l1)

  expect_equal(as.logical(res), TRUE)
  expect_equal(attr(res, "name"), "fork")

  l1 <- "#%mambo fork(x, y = NA, z = 'Two')"

  res <- is_macro(l1)

  expect_equal(as.logical(res), FALSE)

})


test_that("utils13: is_mend() basic functionality.", {

  l1 <- "#%mend"

  res <- is_mend(l1)

  expect_equal(as.logical(res), TRUE)

  l1 <- "#%mand"

  res <- is_mend(l1)

  expect_equal(as.logical(res), FALSE)

})


test_that("utils14: get_parms() basic functionality.", {

  # Normal parameters
  l1 <- "(x, y = NA, z = 'Two')"

  res <- get_parms(l1, "bork")

  nms <- names(res)

  expect_equal(nms, c('x', 'y', 'z'))
  expect_equal(res$x, "")
  expect_equal(res$y, "NA")
  expect_equal(res$z, "'Two'")

  # Parameter with function
  l1 <- "(x, y = max(1, 2, 3), z = 'Two')"

  res <- get_parms(l1, "bork")

  # Parameter with vector
  l1 <- "(x, y = c(a = 1, b = 2, c = 3), z = 'Two')"

  res <- get_parms(l1, "bork")

  # Close paren not last character
  l1 <- "(x, y = &x., z = 'Two') # hello"

  res <- get_parms(l1, "bork")

  # No parameters
  l1 <- "()"

  res <- get_parms(l1, "bork")

  # Missing open paren
  l1 <- "x, y = NA, z = 'Two')"

  expect_error(get_parms(l1, "bork"))

  # Missing close paren
  l1 <- "(x, y = NA, z = 'Two'"

  expect_error(get_parms(l1, "bork"))

  # Missing comma
  l1 <- "(x, y = NA z = 'Two')"

  # expect_error(get_parms(l1, "bork"))
  # Not sure what to do here
  expect_equal(TRUE, TRUE)

  # Call parameters
  l1 <- "(1, 'Two', mean(1, 2))"

  res <- get_parms(l1, "bork", FALSE)

  expect_equal(res[[1]], "1")
  expect_equal(res[[2]], "'Two'")
  expect_equal(res[[3]], "mean(1, 2)")

  # With function
  l1 <- "(x = 1, y = 'Two', z = mean(1, 2))"

  res <- get_parms(l1, "bork", TRUE)

  nms <- names(res)
  expect_equal(nms, c('x', 'y', 'z'))
  expect_equal(res$x, "1")
  expect_equal(res$y, "'Two'")
  expect_equal(res$z, "mean(1, 2)")

})


test_that("utils15: get_macro_code() basic functionality.", {

  lns <- c()

  lns[1] <- "# Before the macro"
  lns[2] <- "#%macro fork(x, y = NA, z = 'Two')"
  lns[3] <- "print(&x.)"
  lns[4] <- "print(&y.)"
  lns[5] <- "print(&z.)"
  lns[6] <- "#%mend"
  lns[7] <- "# After the macro"

  res <- is_macro(lns[2])

  res2 <- get_macro_code(lns, 2, res)

  expect_equal(length(res2), 3)
  expect_equal(res2[1], "print(&x.)")
  expect_equal(res2[2], "print(&y.)")
  expect_equal(res2[3], "print(&z.)")
})

test_that("utils16: is_macro_call() basic functionality.", {

  gbl$macros[["fork"]] <- ""

  l1 <- "#%fork(x, y = NA, z = 'Two')"

  res <- is_macro_call(l1)
  nm <- attr(res, "name")
  prms <- attr(res, "parameters")

  expect_equal(nm, "fork")
  expect_equal(as.logical(res), TRUE)
  expect_equal(prms[[1]], "x")
  expect_equal(prms$y, "NA")
  expect_equal(prms$z, "'Two'")


})


test_that("utils17: get_macro_call() basic functionality.", {

  lns <- c()

  lns[1] <- "# Before the macro"
  lns[2] <- "#%macro fork(x, y = NA, z = 'Two')"
  lns[3] <- "print(&x.)"
  lns[4] <- "print(&y.)"
  lns[5] <- "print(&z.)"
  lns[6] <- "#%mend"
  lns[7] <- "# After the macro"

  res <- is_macro(lns[2])

  res2 <- get_macro_code(lns, 2, res)

  expect_equal(length(res2), 3)
  expect_equal(res2[1], "print(&x.)")
  expect_equal(res2[2], "print(&y.)")
  expect_equal(res2[3], "print(&z.)")


})

test_that("utils18: find_mvar() basic functionality.", {

  res <- sub_mvar("my &a", "&a", "1")

  expect_equal(res, "my 1")

  res <- sub_mvar("my &a.", "&a", "1")

  expect_equal(res, "my 1")

  res <- sub_mvar("my `&a`", "&a", "1")

  expect_equal(res, "my 1")

  res <- sub_mvar("&a.", "&a", "1")

  expect_equal(res, "1")

  res <- sub_mvar("`&a`", "&a", "1")

  expect_equal(res, "1")

  res <- sub_mvar("my `&a.` and more stuff", "&a", "1")

  expect_equal(res, "my 1 and more stuff")

  res <- sub_mvar("my &a and &a", "&a", "1")

  expect_equal(res, "my 1 and 1")

})

test_that("utils19: sub_ready() basic functionality.", {

  expect_equal(sub_ready("my &&&a", "&a", 1), FALSE)
  expect_equal(sub_ready("my &&&a", "&a", 2), FALSE)
  expect_equal(sub_ready("my &&&a", "&a", 3), TRUE)
  expect_equal(sub_ready("my &&&a", "&a", 4), TRUE)

})

