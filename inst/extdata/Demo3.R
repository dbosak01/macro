

#%macro get_analysis(dat, vrs)

# Perform Analysis ----------------------------------------

#%do idx = 1 %to %sysfunc(length(&vrs))

#%let vr <- %sysfunc(&vrs[&idx])
#%let dt <- &dat$&vr
# Analysis for &vr
anl_&vr <- data.frame(VAR = "&vr",
                      MEAN = mean(`&dt`, na.rm = TRUE),
                      MEDIAN = median(`&dt`, na.rm = TRUE),
                      SD = sd(`&dt`, na.rm = TRUE),
                      MIN = min(`&dt`, na.rm = TRUE),
                      MAX = min(`&dt`, na.rm = TRUE))
#%end


# Bind Results ----------------------------------------

#%let anl_lst <- %sysfunc(paste0("anl_", &vrs, collapse = ", "))

# Combine all analysis data frames
final <- rbind(`&anl_lst`)

# Print Results
print(final)

#%mend

#%get_analysis(mtcars, c("mpg", "cyl", "disp"))
