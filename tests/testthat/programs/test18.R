

# Here is a comment
a <- 1

#% Here is a macro function
#%macro sammy(varx, vary, varz = TRUE)
x <- varx.
print(x)
y <- vary.
print(y)
z <- varz.
print(z)
#%mend

# Here is more stuff after the macro function
b <- 2


#% Now call the macro function
#%sammy(3, "fork", FALSE)

v1 <- x
v2 <- y
v3 <- z

# And put something after
