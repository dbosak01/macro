#####################################################################
# Program Name: &prog_name.
# Study: &study_name.
#####################################################################

library(reporter)

# Output path
out_pth <- "&output_dir./&prog_name."

# Get listing data
load("&data_dir./&data_file.")

# Create table object
tbl <- create_table(dm) |>
  define(USUBJID, id_var = TRUE)

# Create report object
rpt <- create_report(out_pth, font = "Courier", output_type = "RTF") |>
  page_header("Sponsor: &sponsor_name.", "Study: &study_name.") |>
  titles(`&titles.`) |>
  add_content(tbl, align = "left") |>
  footnotes(`&footnotes.`) |>
  page_footer(Sys.time(), "CONFIDENTIAL", "Page [pg] of [tpg]")

# Write report to file
write_report(rpt)
