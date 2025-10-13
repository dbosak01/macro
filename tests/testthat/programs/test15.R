

#%let a <- 1
#%let b <- 3
#%let c <- hello

y <- c()
z <- c()

#%if (&a. == 1)

#%do x = 1 %to &b.

#%if (%sysfunc(&x. %% 2) == 1)
print("&c.&x.")
y <- append(y, "&c.&x.")

#%else
print("&c.&x.")
z <- append(z, "&c.&x.")
#%end

w <- `&x.`

#%end

#%end
