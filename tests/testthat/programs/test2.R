

#%let x = c("mpg", "cyl", "disp")
#%let y = mpg > 20

print(x.)

#%if length(x.) == 1

print("X is one")

#%elseif (length(x.) == 2)

print("X is two")

#%else

print("X is something else")
resx <- x.
dat <- subset(mtcars, subset = y., select = x.)

print(dat)

#%end

