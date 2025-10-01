#% Unevaluated assignment
#%let a <- 2 + 2
w <- a.

#% Evaluated assignment
#%let b <- %sysfunc(2 + 2)
x <- b.

#% Unevaluated replacement
#%let c <- a. + b.
y <- c.

#% Evaluated replacement
#%let d <- %sysfunc(a. + b.)
z <- d.
