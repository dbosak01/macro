#####################################
# Assign Macro Variables
#####################################

# Assign base path
`&base_path` <- "c:/packages/macro/tests/testthat/examples"

# Assign relative paths
`&log_path` <- file.path(`&base_path`, "log")
`&output_path` <- file.path(`&base_path`, "output")
`&template_path` <- file.path(`&base_path`, "templates")
`&data_path` <- file.path(`&base_path`, "data")

# Assign global variables
`&sponsor_name` <- "Acme, Inc."
`&study_name` <- "ABC"
`&prog_name` <- "t_dm"

# Select analysis variables
`&vars` <- c("AGE", "AGEG", "SEX", "RACE")
`&anals` <- c("cont", "cat", "cat", "cat")
`&lbls` <- c("Age", "Age Group", "Sex", "Race")

# Assign or get titles
`&titles` <- c("Table 1.0",
             "Analysis of Demographic Characteristics",
             "Safety Population")

# Assign or get footnotes
`&footnotes` <- c(paste0("Program: ", `&prog_name`, ".R"),
                "NOTE: Denominator based on number of non-missing responses.")

# Assign treatment groups and labels
`&trt_grps` <- c("ARM A" = "Placebo", "ARM B" = "Drug 50mg", "ARM C" = "Drug 100mg",
               "ARM D" = "Competitor")

# Assign other parameters
`&env` <- "dev" # "prod"
`&out_type` <- "PDF"

# Assign preview
`&preview` <- ", preview = 1"

# Preprocess and Run Example3
macro::msource(paste0(`&base_path`, "/templates/dm01.R"),
               paste0(`&base_path`, "/code/t_dm.R"),
               debug = TRUE, symbolgen = TRUE)
