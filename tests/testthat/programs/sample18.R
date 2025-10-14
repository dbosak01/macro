#%let vars <- c("mpg", "disp", "hp", "drat")
# Get data
dat <- mtcars

#% Print mean for each variable
#%do v = 1 %to %sysfunc(length(&vars))
#%let var <- %sysfunc(&vars[&v])

# Calculate mean for var.
mn&v. <- sprintf("%.2f", mean(dat$`&var.`))
print(paste0("Mean of '&var.' is ", mn&v.))

#%end
