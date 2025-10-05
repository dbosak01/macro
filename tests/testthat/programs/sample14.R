#%let vars <- c("mpg", "disp", "hp", "drat")
#%let dat <- mtcars

#% Print mean for each variable
#%do v = 1 %to length(vars.)
#%let var <- %sysfunc(vars.[v.])
#%let mn <- %sysfunc(mean(dat.[["var."]]), %.2f)
print("Mean of 'var.' is mn.")
#%end




#%let vars <- c("mpg", "disp", "hp", "drat")
#%let dat <- mtcars

#% Print mean for each variable
#%do v = 1 %to length(.vars)
#%let var <- %sysfunc(.vars[.v])
#%let mn <- %sysfunc(mean(.dat[[".var"]]), %.2f)
print("Mean of '.var' is .mn")
#%end




