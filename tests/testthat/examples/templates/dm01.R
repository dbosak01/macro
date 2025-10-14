#####################################################################
# Program Name: &prog_name.
# Study: &study_name.
#####################################################################

library(sassy)

# Prepare Log -------------------------------------------------------------


options("logr.autolog" = TRUE,
        "logr.on" = TRUE,
        "logr.notes" = FALSE,
        "procs.print" = FALSE)

# Assign program name
prog_nm <- "&prog_name."

# Construct paths
l_path <- file.path("&log_path.", paste0(prog_nm, ".log"))
o_path <- file.path("&output_path.", prog_nm)

# Open log
lf <- log_open(l_path)

# Prepare formats ---------------------------------------------------------

sep("Prepare formats")

put("Compile format catalog")
fc <- fcat(MEAN = "%.1f", STD = "(%.2f)",
           Q1 = "%.1f", Q3 = "%.1f",
           MIN = "%d", MAX = "%d",
           CNT = "%2d", PCT = "(%5.1f%%)")


#%if ("AGEG" %in% &vars.)

put("Age Groups")
fc$AGEG <- value(condition(x >= 18 & x <= 29, "18 to 29"),
                 condition(x >=30 & x <= 39, "30 to 39"),
                 condition(x >=40 & x <=49, "40 to 49"),
                 condition(x >= 50, ">= 50"),
                 as.factor = TRUE)
#%end

#%if ("SEX" %in% &vars.)

put("Sex decodes")
fc$SEX <- value(condition(x == "M", "Male"),
                condition(x == "F", "Female"),
                condition(TRUE, "Other"),
                as.factor = TRUE)

#%end

#%if ("RACE" %in% &vars.)

put("Race decodes")
fc$RACE <- value(condition(x == "WHITE", "White"),
                 condition(x == "BLACK OR AFRICAN AMERICAN", "Black or African American"),
                 condition(x == "ASIAN", "Asian or Pacific Islander"),
                 condition(x == "UNKNOWN", "Unknown"),
                 condition(TRUE, "Other"),
                 as.factor = TRUE)

#%end



# Load and Prepare Data ---------------------------------------------------

sep("Prepare Data")

#%if ("&env." == "prod")

put("Get data")
libname(dat, "&data_path.", "Rda")

dm <- dat$dm

#%else

put("Create sample data.")
#%include '&template_path./dat01.R'

#%end

put("Log starting dataset")
put(dm)

put("Filter out screen failure")
dm_f <- subset(dm, ARM != 'SCREEN FAILURE')


put("Get ARM population counts")
proc_freq(dm_f, tables = ARM,
          output = long,
          options = v(nopercent, nonobs)) -> arm_pop

put("Log treatment groups variable")
trt_grps <- `&trt_grps`
put(trt_grps)

#%if ("AGEG" %in% &vars.)

put("Categorize AGE")
dm_f$AGEG <- fapply(dm_f$AGE, fc$AGEG)
#%end

#% Get length of variable vector
#%let varcnt <- %sysfunc(length(&vars.))
#%
# Perform Analysis  -------------------------------------------------------
#%
#% Iterate analysis variables
#%do idx = 1 %to &varcnt.
#%
#%let var <- %sysfunc(&vars[&idx])
#%let lvar <- %sysfunc(tolower("&var"))
#%let lbl <- %sysfunc(&lbls[&idx])
#%let anal <- %sysfunc(&anals[&idx])
#%
#%if ("&anal" == "cont")
#%
# &lbl. Summary Block -------------------------------------------------------

sep("Create summary statistics for &lvar..")

put("Call means procedure to get summary statistics for &lvar.")
proc_means(dm_f, var = `&var.`,
           stats = v(n, mean, std, median, q1, q3, min, max),
           by = ARM,
           options = v(notype, nofreq)) -> `&lvar._stats`

put("Combine stats")
datastep(`&lvar._stats`,
         format = fc,
         drop = find.names(`&lvar._stats`, start = 4),
         {
           VAR <- "lbl."
           `Mean (SD)` <- fapply2(MEAN, STD)
           Median <- MEDIAN
           `Q1 - Q3` <- fapply2(Q1, Q3, sep = " - ")
           `Min - Max` <- fapply2(MIN, MAX, sep = " - ")


         }) -> `&lvar._comb`

put("Transpose ARMs into columns")
proc_transpose(`&lvar._comb`,
               var = names(`&lvar._comb`),
               copy = VAR, id = BY,
               name = LABEL) -> `&lvar._block`
#%end
#%if ("&anal." == "cat")

# &lbl. Block ---------------------------------------------------------------

sep("Create frequency counts for &lbl.")

put("Get &lvar. frequency counts")
proc_freq(dm_f,
          table = `&var.`,
          by = ARM,
          options = nonobs) -> `&lvar._freq`

put("Combine counts and percents and assign age group factor for sorting")
datastep(`&lvar._freq`,
         format = fc,
         keep = v(VAR, LABEL, BY, CNTPCT),
         {
           VAR <- "&lbl."
           CNTPCT <- fapply2(CNT, PCT)
           #%if ("&var." == "AGEG")
           LABEL <- CAT
           #%else
           LABEL <- fapply(CAT, fc$`&var.`)
           #%end
         }) -> `&lvar._comb`


put("Sort by lvar. factor")
proc_sort(`&lvar._comb`, by = v(BY, LABEL)) -> `&lvar._sort`

put("Tranpose lvar. block")
proc_transpose(`&lvar._sort`,
               var = CNTPCT,
               copy = VAR,
               id = BY,
               by = LABEL,
               options = noname) -> `&lvar._block`
#%end
#%end

# Create final data frame -------------------------------------------------


#%let blocks <- %sysfunc(paste0(tolower(&vars.), "_block", collapse = ", "))

final <- rbind(`&blocks.`)

# Report ------------------------------------------------------------------

#% Include standard report code 01
#%include '&template_path./rpt01.R'

# Clean Up ----------------------------------------------------------------
sep("Clean Up")

put("Close log")
log_close()


# Uncomment to view report
# file.show(res$modified_path)

# Uncomment to view log
# file.show(lf)
