#%let dev_path <- ./dev/data
#%let prod_path <- ./prod/data
#%let env <- dev

# Path to data
#%if ("env." == "prod")
pth <- "prod_path./dm.sas7bdat"
#%else
pth <- "dev_path./dm.sas7bdat"
#%end



#% Unquoted Assignment
#%let c <- Three

# Unquoted resolution
# z <- c.



