

#***********************************#
#* Define Macro Variables
#***********************************#
sponsor_name. <- "Acme"
study_name. <- "ABC"
prog_name. <- "l_dm"
base_dir. <- "c:/packages/macro/tests/testthat/examples"
output_dir. <- file.path(base_dir., "output")
data_dir. <- file.path(base_dir., "data")
data_file. <- "dm.rda"
data_name. <- "dm"
titles. <- c("Listing 1.0", "SDTM Demographics")
footnotes. <- paste0("'Program: ", prog_name., ".R'")
output_type. <- "PDF"
preview. <- TRUE

#***********************************#
#* Macro Source Listing Template Code
#***********************************#
msource(file.path(base_dir., "templates/lst02.R"),
        file.path(base_dir., "code/l_dm.R"),
        debug = TRUE)


