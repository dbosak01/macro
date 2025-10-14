

#% Check for existance of a
#%if (%symexist(a) == FALSE)
#%let a <- 2
#%end

#% Check for existance of b
#%if (%symexist(b) == FALSE)
#%let b <- 3
#%end

# Calculate a * b
x <- `&a` * `&b`

