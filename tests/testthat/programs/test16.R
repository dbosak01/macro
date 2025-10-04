

#%let a <- 2
#%let b <- 3
#%let c <- hello

w <- c()

#%do x = 1 %to a.

#%do y = 1 %to b.

w <- append(w, "hello%sysfunc(x. + y.)")

#%end

#%end

