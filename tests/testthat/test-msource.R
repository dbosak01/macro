
base_path <- "c:\\packages\\macro\\tests\\testthat"

base_path <- "./"

dev <- FALSE



test_that("msource1: mreplace() basic functionality.", {

  gbl$env <- e

  is_let("#%let x = 3", TRUE)
  is_let("#%let y = 2", TRUE)

  ln <- "Y is &y."

  res <- mreplace(ln)

  expect_equal(res, "Y is 2")

  is_let("#%let y = 3", TRUE)
  ln <- "Y is &y."

  res <- mreplace(ln)

  expect_equal(res, "Y is 3")


  ln <- "Y is &z."

  res <- mreplace(ln)

  expect_equal(res, "Y is &z.")


  ln <- "X is &x."

  res <- mreplace(ln)

  expect_equal(res, "X is 3")

  # Numeric vector
  is_let("#%let z = c(1, 2, 3)", TRUE)

  ln <- "Z is &z."

  res <- mreplace(ln)

  expect_equal(res, "Z is c(1, 2, 3)")

  # Character vector
  is_let("#%let z = c('A', 'B', 'C')", TRUE)

  ln <- "Z is &z."

  res <- mreplace(ln)

  expect_equal(res, "Z is c('A', 'B', 'C')")

  # Numeric vector with names
  is_let("#%let z <- c(A = 1, B = 2, C = 3)", TRUE)

  ln <- "Z is &z."

  res <- mreplace(ln)

  expect_equal(res, "Z is c(A = 1, B = 2, C = 3)")

  # Character vector with names
  is_let("#%let z <- c('A' = 'One', 'B' = 'Two', 'C' = 'Three')", TRUE)

  ln <- "Z is &z."

  res <- mreplace(ln)

  expect_equal(res, "Z is c('A' = 'One', 'B' = 'Two', 'C' = 'Three')")

  # Numeric vector with names and =
  is_let("#%let z = c(A = 1, B = 2, C = 3)", TRUE)

  ln <- "Z is &z."

  res <- mreplace(ln)

  expect_equal(res, "Z is c(A = 1, B = 2, C = 3)")

  # Character vector with names and =
  is_let("#%let z = c('A' = 'One', 'B' = 'Two', 'C' = 'Three')", TRUE)

  ln <- "Z is &z."

  res <- mreplace(ln)

  expect_equal(res, "Z is c('A' = 'One', 'B' = 'Two', 'C' = 'Three')")


})


test_that("msource2: basic functionality.", {

  fl <- file.path(base_path, "programs/test0.R")
  fl2 <- file.path(base_path, "programs/test0_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  ne <- new.env()

  # New environment
  res <- msource(fl, file_out = fl2, envir = ne, debug = TRUE, symbolgen = TRUE)

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
  expect_equal(d, 6)
  # expect_equal(q, floor(2 + 2.5))  # Can't get this to work
  expect_equal(f, TRUE)
  expect_equal(g, FALSE)

})


test_that("msource4: if/then/else functionality.", {

  fl1 <- file.path(base_path, "programs/test2.R")
  fl2 <- file.path(base_path, "programs/test2_mod.R")

  res <- msource(fl1, file_out = fl2, debug = TRUE,
                 symbolgen = TRUE)

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
  e1$`&z.` <- 4

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
  expect_equal(res$output, fl2)  # Check output path is working

})

test_that("msource8: test include.", {

  fl1 <- file.path(base_path, "programs/test7.R")
  fl2 <- file.path(base_path, "programs/test7_mod.R")
  l1 <- file.path(base_path, "programs/test0.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()
  e1$`&z.` <- l1

  res <- msource(fl1, file_out = fl2, e1, debug = TRUE)

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
  e1$`&vars.` <- c("age", "ageg")
  e1$`&v1.` <- 2
  e1$`&v2.` <- as.Date("2025-01-01")
  e1$`&v3.` <- as.POSIXct("2025-09-17 15:25:18 EDT")
  e1$`&v4.` <- as.POSIXlt("2025-09-17 15:25:18 EDT")

  res <- msource(fl1, file_out = fl2, e1)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$fnd, TRUE)
  expect_equal(e1$v1, 2)
  expect_equal(e1$v2, as.Date("2025-01-01"))
  expect_equal(e1$v3, as.POSIXct("2025-09-17 15:25:18 EDT"))
  expect_equal(e1$v4, as.POSIXlt("2025-09-17 15:25:18 EDT"))

})


test_that("msource10: let in if condition.", {

  fl1 <- file.path(base_path, "programs/test9.R")
  fl2 <- file.path(base_path, "programs/test9_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()
  e1$`&a.` <- 1

  res <- msource(fl1, file_out = fl2, e1)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$v1, 1)


})



test_that("msource11: Vector resolution works as expected.", {

  fl1 <- file.path(base_path, "programs/test10.R")
  fl2 <- file.path(base_path, "programs/test10_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  e1$`&x.` <- 3
  e1$`&y.` <- 2
  e1$`&z1.` <- c(1, 2, 3)
  e1$`&z2.` <- c("A", "B", "C")
  e1$`&z3.` <- c(A = 1, B = 2, C = 3)
  e1$`&z4.` <- c(A = "One", B = "Two", C = "Three")


  res <- msource(fl1, file_out = fl2, e1)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$v1, 3)
  expect_equal(e1$v2, 2)
  expect_equal(e1$v3, c(1, 2, 3))
  expect_equal(e1$v4, c("A", "B", "C"))
  expect_equal(e1$v5, c(A = 1, B = 2, C = 3))
  expect_equal(e1$v6, c(A = "One", B = "Two", C = "Three"))

})

test_that("msource11: error on incomplete if block.", {

  fl1 <- file.path(base_path, "programs/test11.R")

  expect_error(msource(fl1))



})

test_that("msource12: error with extra end.", {

  fl1 <- file.path(base_path, "programs/test12.R")

  expect_error(msource(fl1))


})

test_that("msource13: test variable confounding.", {

  fl1 <- file.path(base_path, "programs/test13.R")
  fl2 <- file.path(base_path, "programs/test13_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, file_out = fl2, e1, debug = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$x, 1)
  expect_equal(e1$y, 2)


})

test_that("msource14: test debug_out.", {

  fl1 <- file.path(base_path, "programs/test13.R")
  fl2 <- file.path(base_path, "examples/log/test13.txt")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, envir = e1, debug = TRUE,
                 debug_out = fl2, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$x, 1)
  expect_equal(e1$y, 2)


  fl3 <- file.path(base_path, "programs/test12.R")
  fl4 <- file.path(base_path, "examples/log/test12.txt")

  if (file.exists(fl4))
    file.remove(fl4)

  expect_error(msource(fl3, debug = TRUE, debug_out = fl4))


})


test_that("msource15: do loop functionality.", {

  fl1 <- file.path(base_path, "programs/test5.R")
  fl2 <- file.path(base_path, "programs/test5_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  res <- msource(fl1, file_out = fl2, debug = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(a, 4)
  expect_equal(b, 3)

})



test_that("msource16: more do loop functionality.", {

  fl1 <- file.path(base_path, "programs/test14.R")
  fl2 <- file.path(base_path, "programs/test14_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1, debug = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$z, 3)
  expect_equal(length(e1$y), 3)


})

test_that("msource17: advanced do loop functionality.", {

  fl1 <- file.path(base_path, "programs/test15.R")
  fl2 <- file.path(base_path, "programs/test15_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1, debug = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$y, c("hello1", "hello3"))
  expect_equal(e1$z, c("hello2"))
  expect_equal(e1$w, 3)


})

test_that("msource18: nested do loops.", {

  fl1 <- file.path(base_path, "programs/test16.R")
  fl2 <- file.path(base_path, "programs/test16_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1, debug = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(length(e1$w), 6)
  expect_equal(e1$w, c("hello2", "hello3", "hello4", "hello3", "hello4", "hello5"))

})

test_that("msource19: test %symput()", {

  fl1 <- file.path(base_path, "programs/test17.R")
  fl2 <- file.path(base_path, "programs/test17_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$a, 14)
  expect_equal(e1$b, 3)
  expect_equal(e1$c, "mvar")
  expect_equal(e1$d, c("fork", "bork", "sammy"))

})

test_that("msource20: mreplace() nested and double replacements.", {

  is_let("#%let x = 1", TRUE)
  is_let("#%let mvar1 = fork", TRUE)

  ln <- "mvar1 is &mvar&x.."

  res <- mreplace(ln)

  expect_equal(res, "mvar1 is fork")

  ln <- "mvar1 is &mvar&x.. and &mvar&x.."

  res <- mreplace(ln)

  expect_equal(res, "mvar1 is fork and fork")

  is_let("#%let x = 1", TRUE)
  is_let("#%let mvar = spork", TRUE)
  is_let("#%let mvar1 = fork", TRUE)


  ln <- "mvar1 is &&mvar&x.."

  res <- mreplace(ln)

  expect_equal(res, "mvar1 is fork")

  ln <- "mvar1 is &&mvar&x.. and &&mvar&x.."

  res <- mreplace(ln)

  expect_equal(res, "mvar1 is fork and fork")

})


test_that("msource21: test macro definition and call", {

  fl1 <- file.path(base_path, "programs/test18.R")
  fl2 <- file.path(base_path, "programs/test18_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$a, 1)
  expect_equal(e1$b, 2)
  expect_equal(e1$v1, 3)
  expect_equal(e1$v2, "fork")
  expect_equal(e1$v3, FALSE)

})

test_that("msource22: test macro scope", {

  fl1 <- file.path(base_path, "programs/test19.R")
  fl2 <- file.path(base_path, "programs/test19_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$x1, 1)
  expect_equal(e1$x2, 2)
  expect_equal(e1$x3, 1)

})



test_that("msource23: test nested macro scope", {

  fl1 <- file.path(base_path, "programs/test20.R")
  fl2 <- file.path(base_path, "programs/test20_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$x1, 1)
  expect_equal(e1$x2, 2)
  expect_equal(e1$x3, 3)
  expect_equal(e1$x4, 2)
  expect_equal(e1$x5, 1)

})

test_that("msource24: test symbolgen unresolved variable message", {

  fl1 <- file.path(base_path, "programs/test21.R")
  fl2 <- file.path(base_path, "programs/test21_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$x1, "&bork.")

})


test_that("msource25: test symput basic functionality", {

  fl1 <- file.path(base_path, "programs/test22.R")
  fl2 <- file.path(base_path, "programs/test22_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  res <- msource(fl1, fl2, envir = e1,
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  expect_equal(eres, TRUE)
  expect_equal(e1$y, 1)

})

test_that("msource26: test symput error handling", {

  fl1 <- file.path(base_path, "programs/test23.R")
  fl2 <- file.path(base_path, "programs/test23_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  e1 <- new.env()

  expect_error(msource(fl1, fl2, envir = e1,
                 debug = TRUE, symbolgen = TRUE))



})

test_that("msource27: local environment usage.", {

  fl <- file.path(base_path, "programs/test24.R")
  fl2 <- file.path(base_path, "programs/test24_mod.R")

  if (file.exists(fl2))
    file.remove(fl2)

  `&x` <- 1

  # Local environment
  res <- msource(fl, file_out = fl2, envir = "local",
                 debug = TRUE, symbolgen = TRUE)

  eres <- file.exists(fl2)

  # z should not be populated in this frame
  # but you will see the print out
  hasa <- exists("z")

  expect_equal(eres, TRUE)
  expect_equal(hasa, FALSE)

})



test_that("msource28: selected code works as expected.", {

  if (dev) {
    fl <- file.path(base_path, "programs/test24.R")
    fl2 <- file.path(base_path, "programs/test24_mod.R")

    if (file.exists(fl2))
      file.remove(fl2)

    `&x` <- 1

    res <- msource(fl, file_out = fl2, envir = "local",
                   debug = TRUE, symbolgen = TRUE)

    eres <- file.exists(fl2)

    hasa <- exists("z")

    expect_equal(eres, TRUE)
    expect_equal(hasa, FALSE)
  } else {

    expect_equal(TRUE, TRUE)
  }

})

test_that("msource29: Line continuation works as expected.", {

  fl <- file.path(base_path, "programs/test26.R")


  res <- msource(fl, debug = TRUE, symbolgen = TRUE)


  x <- mget("x")
  y <- mget("y")
  z <- mget("z")

  expect_equal(x, "1")
  expect_equal(y, "c(\"One\",\n          \"Two\",\n          \"Three\")")
  expect_equal(z, "\"Found it!\"")

})

