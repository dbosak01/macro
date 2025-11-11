#% Demo5: Template Analysis Program %#
library(dplyr)

# Perform Analysis ------------------------------------

#% Macro for Dynamic Descriptive Stats
#%macro get_analysis(dat, var, stats)
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
  )
#%mend

#% Loop through analysis variables
#%do idx = 1 %to %sysfunc(length(&vars))
#%let var <- %sysfunc(&vars[&idx])
#%get_analysis(&dat, &var, &stats)
#%end

#% Get datasets to bind
#%let dsets <- %sysfunc(paste0("anl_", &vars, collapse = ", "))
# Bind analysis datasets
final <- bind_rows(`&dsets`)

# Print results
print(final)


