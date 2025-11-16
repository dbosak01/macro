
library(macro)

#***********************************#
#* Define Macro Variables
#***********************************#
symput("sponsor_name", "Acme")
symput("study_name", "ABC")
symput("prog_name", "l_dm")
symput("base_dir", "c:/packages/macro/tests/testthat/examples")
symput("output_dir", "&base_dir/output")
symput("data_dir", "&base_dir/data")
symput("data_file", "dm.rda")
symput("data_name", "dm")
symput("titles", c("Listing 1.0", "SDTM Demographics"))
symput("footnotes", "'Program: &prog_name..R'")
symput("output_type", "PDF")
symput("preview", TRUE)

#***********************************#
#* Macro Source Listing Template Code
#***********************************#
msource(file.path(symget("base_dir"), "templates/lst02.R"),
        file.path(symget("base_dir"), "code/l_dm2.R"),
        debug = TRUE, clear = FALSE)


