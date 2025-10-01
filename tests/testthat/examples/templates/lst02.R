#####################################################################
# Program Name: prog_name.
# Study: study_name.
#####################################################################

library(reporter)

# Output path
out_pth <- "output_dir./prog_name."

# Get listing data
load("data_dir./data_file.")

# Create table object
tbl <- create_table(data_name.) |>
  define(USUBJID, id_var = TRUE)

#% Assign default output type
#%if (%symexist(output_type) == FALSE)
#%let output_type <- RTF
#%end

# Create report object
rpt <- create_report(out_pth, font = "Courier", output_type = "output_type.") |>
  page_header("Sponsor: sponsor_name.", "Study: study_name.") |>
  titles(titles.) |>
  add_content(tbl, align = "left") |>
  footnotes(footnotes.) |>
  page_footer(Sys.time(), "CONFIDENTIAL", "Page [pg] of [tpg]")

# Write report to file
#%if (preview. == TRUE)
write_report(rpt, preview = 1)
#%else
write_report(rpt)
#%end
