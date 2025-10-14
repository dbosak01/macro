## macro.Rmd - user-defined macro section

#% Define macro function
#%macro do_analysis(data, varname)
# Analysis for "&varname"
mean_&varname <- mean(`&data`[["&varname"]])
min_&varname <- min(`&data`[["&varname"]])
max_&varname <- max(`&data`[["&varname"]])
anal_&varname <- data.frame("Variable" = "&varname",
                            "Mean" = mean_&varname,
                            "Min"  = min_&varname,
                            "Max"  = max_&varname)
#%mend

#%do_analysis(mtcars, mpg)
#%do_analysis(mtcars, cyl)
#%do_analysis(mtcars, disp)

# Combine analysis blocks
final <- rbind(anal_mpg, anal_cyl, anal_disp)

# Print result
print(final)
