#####################################################################
# Program Name: l_dm
# Study: ABC
#####################################################################

library(reporter)

# Output path
out_pth <- "c:/packages/macro/tests/testthat/examples/output/l_dm"

# Get listing data
load("c:/packages/macro/tests/testthat/examples/data/dm.rda")

# Create table object
tbl <- create_table(dm) |>
  define(USUBJID, id_var = TRUE)


# Create report object
rpt <- create_report(out_pth, font = "Courier", output_type = "PDF") |>
  page_header("Sponsor: Acme", "Study: ABC") |>
  titles(c('Listing 1.0', 'SDTM Demographics')) |>
  add_content(tbl, align = "left") |>
  footnotes('Program: l_dm.R') |>
  page_footer(Sys.time(), "CONFIDENTIAL", "Page [pg] of [tpg]")

# Write report to file
write_report(rpt, preview = 1)
