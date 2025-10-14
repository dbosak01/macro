#%macro printit(txt)
print("Print the text: &txt.")
#%mend
#%printit(Hello!)


library(dplyr)

#%macro print_analysis(dat, byvar, var)
cat("Analysis for &var.\n")
`&dat` |>
  select(`&byvar`, `&var`) |>
  group_by(`&byvar`) |>
  summarize(Mean = mean(`&var`, rm.na = TRUE),
            SD = sd(`&var`),
            Quantile = quantile(`&var`, probs = .25, rm.na = TRUE)) |>
  as.data.frame() |> print()
cat("\n")

#%mend

#%let vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#%do idx = 1 %to %sysfunc(length(&vars))
#%print_analysis(iris, Species, %sysfunc(&vars[&idx]))
#%end
