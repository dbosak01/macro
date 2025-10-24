#% Assign variable name to macro
#%let x <- cyl

# Leading macro replacement
`&x._mean` <- mean(mtcars[["&x"]])

# Trailing macro replacement
mean_&x <- mean(mtcars[["&x"]])

# Middle macro replacement
my_&x_mean <- mean(mtcars[["&x"]])

# Intermediate macro replacement
#%let myvar <- &x_mean
`&myvar` <- mean(mtcars[["&x"]])
