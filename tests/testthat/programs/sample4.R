
#% Assign integer vector
#%let a = c(2, 5, 8, 9)

#% Sum vector without %sysfunc
#%let b = sum(a.) / 1.3

#% Sum vector with %sysfunc
#%let c = %sysfunc(sum(a.) / 1.3)

#% Format expression with %sysfunc
#%let d = %sysfunc(sum(a.) / 1.3, '%.2f')

# Text replacement
w <- a.

# Expression replacement
x <- b.

# Evaluated with %sysfunc()
y <- c.

# Formatted with %sysfunc()
z <- d.
