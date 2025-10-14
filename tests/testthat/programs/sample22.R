#%let a <- # Subset mtcars
#%let b <- mtcars
#%let c <- , mpg > 25
#%let d <- dat <- subset(&b&c)
#%let e <- (dat)
`&a`
`&d`
print&e
