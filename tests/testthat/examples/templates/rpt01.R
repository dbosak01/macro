
sep("Create and print report")

#%if (%symexist(out_type) == FALSE)
  #%let out_type <- "RTF"
#%end

# Create Table
tbl <- create_table(final, first_row_blank = TRUE) |>  
  column_defaults(from = `ARM A`, to = `ARM D`, align = "center", width = 1.1) |>  
  stub(vars = c("VAR", "LABEL"), "Variable", width = 2.5) |>  
  define(VAR, blank_after = TRUE, dedupe = TRUE, label = "Variable",
         label_row = TRUE) |>  
  define(LABEL, indent = .25, label = "Demographic Category") |> 
  titles(titles., bold = TRUE) |> 
  footnotes(footnotes.) 

# Add treatment groups
for (trt in names(trt_grps)) {
  tbl <- define(tbl, trt, label = trt_grps[trt], n = arm_pop[trt], standard_eval = TRUE)
}

# Create report
rpt <- create_report(o_path, 
                     output_type = "out_type.", 
                     font = "Arial") |> 
  page_header("Sponsor: sponsor_name.", "Study: study_name.") |> 
  set_margins(top = 1, bottom = 1) |>  
  add_content(tbl) |> 
  page_footer("Date Produced: {toupper(fapply(Sys.Date(), '%Y%b%d'))}", 
              right = "Page [pg] of [tpg]")

put("Write out the report")
res <- write_report(rpt)
