#% Demo2: Correlation Analysis %#
#%macro get_correlation(dat, xvar, yvars)

# Perform Analysis ------------------------------------

#%do idx = 1 %to %sysfunc(length(&yvars))

#%let yvar <- %sysfunc(&yvars[&idx])
#%let xdat <- &dat$&xvar
#%let ydat <- &dat$&yvar
# Correlation between &xvar and &yvar
anl_&yvar <- data.frame(XVAR = "&xvar",
                        YVAR = "&yvar",
                        COR = cor(`&xdat`, `&ydat`,
                              method = "pearson"))
#%end


# Bind Results ----------------------------------------

#%let anl_lst <- %sysfunc(paste0("anl_", &yvars, collapse = ", "))

# Combine all analysis data frames
final <- rbind(`&anl_lst`)

# Print Results
print(final)

#%mend

#% Call Macro
#%get_correlation(mtcars, mpg, c("cyl", "disp", "drat"))


