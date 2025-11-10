#%let x <- Hello
#%let y <- %nrstr(&x.)
a <- "Here is &x. a regular"
b <- "Here is %nrstr(&x.) a protected"
c <- "Here is %nrstr(&x.) multiple %nrstr(&y.) protected"
d <- "Here is &x. mixed %nrstr(&x.)"
e <- "Here is &y. with unresolved reference"
