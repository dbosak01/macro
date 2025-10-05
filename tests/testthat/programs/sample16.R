# Assign vector
v1 <- c("one", "two", "three")

#% Get length of vector
#%let x <- %symput(length(v1))

#% Get vector values
#%let y <- %symput(v1)

# Print each value of vector y.
#%do idx = 1 %to x.
print("Vector value %sysfunc(y.[idx.])")
#%end
