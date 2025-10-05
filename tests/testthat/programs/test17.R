

dat <- subset(mtcars, mpg > 20)

#%let x <- %symput(nrow(dat))

a <- x.

dat <- subset(dat, select = c("mpg", "cyl", "disp"))

#%let y <- %symput(ncol(dat))

b <- y.

mvar <- c("fork", "bork", "sammy")

#%let z <- mvar

c <- "z."

#%let w <- %symput(mvar)

d <- w.
