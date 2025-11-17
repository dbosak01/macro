
library(procs)
library(macro)

#% Assign macro variables
#%let dat <- mtcars
#%let vars <- c("mpg", "disp", "drat")

#% Macro to run proc_means
#%macro get_means(dat, var)

# Analysis of '&var'
anl_&var <- proc_means(`&dat`, `&var`, class = cyl,
                      output = report)

#%mend

#% Loop over variables
#%do idx = 1 %to %sysfunc(length(&vars))
  #%let var <- %sysfunc(&vars[&idx])
  #%get_means(&dat, &var)
#%end

#% Create list of analysis datasets
#%let varlst <- %sysfunc(paste0("anl_", &vars, collapse = ", "))

# Combine analysis
final <- list(`&varlst`)

# Print to console
print(final)

# Print to viewer
proc_print(final,
           titles = c("Analysis of Selected MTCARS Variables",
                      "by cylinders"))

