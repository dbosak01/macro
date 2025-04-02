


print(c("mpg", "cyl", "disp"))


print("X is something else")
resx <- c("mpg", "cyl", "disp")
dat <- subset(mtcars, subset = mpg > 20, select = c("mpg", "cyl", "disp"))

print(dat)


