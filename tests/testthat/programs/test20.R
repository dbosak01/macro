
# Here is a comment
#%let a <- 1
x1 <- a.
print(x1)


# Here is a macro function
#%macro sammy()
#%let a <- 2
x2 <- a.
print(x2)

#%macro fork()
#%let a <- 3
x3 <- a.
print(x3)
#%mend
#%fork()
x4 <- a.
print(x4)
#%mend

#% Now call the macro function
#%sammy()

# After macro
x5 <- a.
print(x5)
