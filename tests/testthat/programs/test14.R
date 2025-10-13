

#%let a <- 3
#%let b <- "hello"

y <- c()

#%do x = 1 %to &a.
print(`&b.`)
y <- append(y, `&b.`)
#%end

z <- `&x.`
