#%let vars <- c("mpg", "disp", "hp", "drat")
# Get data
dat <- mtcars


#% Print mean for each variable
#%do v = 1 %to length(vars.)
#%let var <- %sysfunc(vars.[v.])

# Calculate mean for var.
mnv. <- sprintf("%.2f", mean(dat$var.))
print(paste0("Mean of 'var.' is ", mnv.))

#%end
