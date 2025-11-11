#% Demo7: Logr integration example %#
library(logr)

options("logr.autolog" = TRUE,
        "logr.notes" = FALSE)

# Begin logging
lf <- log_open()

# Log parameters
put("Incoming data: %nrstr(&dat.) = &dat.")
put("Variables to by analyzed: &vars.")
put("Statistics desired: &stats.")

# Perform Analysis ------------------------------------

#% Macro for Dynamic Descriptive Stats
#%macro get_analysis(dat, var, stats = c("mean", "median", "sd", "min", "max"))
put("Performing analysis for &var.")
anl_&var <- `&dat` |>
  summarize(VAR = "&var",
            #%if ("mean" %in% &stats)
            MEAN = mean(`&var`, na.rm = TRUE),
            #%end
            #%if ("median" %in% &stats)
            MEDIAN = median(`&var`, na.rm = TRUE),
            #%end
            #%if ("sd" %in% &stats)
            SD = sd(`&var`, na.rm = TRUE),
            #%end
            #%if ("min" %in% &stats)
            MIN = min(`&var`, na.rm = TRUE),
            #%end
            #%if ("max" %in% &stats)
            MAX = max(`&var`, na.rm = TRUE),
            #%end
            END = NULL
  ) |> put()
#%mend

#% Loop through analysis variables
put("Begin macro loop")
#%do idx = 1 %to %sysfunc(length(&vars))
#%let var <- %sysfunc(&vars[&idx])
#%get_analysis(&dat, &var, &stats)
#%end

#% Get datasets to bind
#%let dsets <- %sysfunc(paste0("anl_", &vars, collapse = ", "))
put("Bind analysis datasets")
final <- bind_rows(`&dsets`) |> put()

put("Print results")
print(final)

# Close log
log_close()
