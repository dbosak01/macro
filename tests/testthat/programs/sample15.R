# Assign vector
v1 <- c("one", "two", "three")

#% Get length of vector
#%let x <- %symput(length(v1))

#% Get vector values
#%let y <- %symput(v1)

# Assign length with macro variable
a <- x.

# Assign vector values with macro variable
b <- y.

print(a)
print(b)
