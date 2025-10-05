# Get data
dat <- mtcars

# Calculate mean for mpg
mn1 <- sprintf("%.2f", mean(dat$mpg))
print(paste0("Mean of 'mpg' is ", mn1))

# Calculate mean for disp
mn2 <- sprintf("%.2f", mean(dat$disp))
print(paste0("Mean of 'disp' is ", mn2))

# Calculate mean for hp
mn3 <- sprintf("%.2f", mean(dat$hp))
print(paste0("Mean of 'hp' is ", mn3))

# Calculate mean for drat
mn4 <- sprintf("%.2f", mean(dat$drat))
print(paste0("Mean of 'drat' is ", mn4))
