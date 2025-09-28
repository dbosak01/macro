#% Data source SAS or RDS
#%let src <- SAS

#% Select analysis variables
#%let anal_vars <- c("AGE", "AGEG", "SEX", "RACE", "PULSE", "TEMP")

######################
# Get data
######################
#%if ("src." == "SAS")
library(haven)

# Get adsl dataset
adsl <- haven("./data/ADSL.sas7bdat")

#%if (any(c("PULSE", "TEMP", "BP") %in% anal_vars.))

# Get advs dataset
advs <- haven("./data/ADVS.sas7bdat")
#%end

#%else

# Get adsl dataset
adsl <- readRDS("./data/ADSL.rds")

#%if (any(c("PULSE", "TEMP", "BP") %in% anal_vars.))

# Get advs dataset
advs <- readRDS("./data/ADVS.rds")
#%end

#%end
