#####################################
# Assign Macro Variables
#####################################

# Assign base path
symput("base_path", "c:/packages/macro/tests/testthat/examples")

# Assign relative paths
symput("log_path", "&base_path/log")
symput("output_path", "&base_path/output")
symput("template_path", "&base_path/templates")
symput("data_path", "&base_path/data")

# Assign global variables
symput("sponsor_name", "Acme, Inc.")
symput("study_name", "ABC")
symput("prog_name", "t_dm")

# Select analysis variables
symput("vars", c("AGE", "AGEG", "SEX", "RACE"))
symput("anals", c("cont", "cat", "cat", "cat"))
symput("lbls", c("Age", "Age Group", "Sex", "Race"))

# Assign or get titles
symput("titles", c("Table 1.0",
                   "Analysis of Demographic Characteristics",
                   "Safety Population"))

# Assign or get footnotes
symput("footnotes", c(paste0("Program: &prog_name..R"),
                      "NOTE: Denominator based on number of non-missing responses."))

# Assign treatment groups and labels
symput("trt_grps", c("ARM A" = "Placebo",
                     "ARM B" = "Drug 50mg",
                     "ARM C" = "Drug 100mg",
                     "ARM D" = "Competitor"))

# Assign other parameters
symput("env", "dev") # "prod"
symput("out_type", "PDF")

# Assign preview
symput("preview", ", preview = 1")

# Preprocess and Run Example3
macro::msource(paste0(symget("base_path"), "/templates/dm01.R"),
               paste0(symget("base_path"), "/code/t_dm.R"),
               debug = TRUE, symbolgen = TRUE, clear = FALSE)
