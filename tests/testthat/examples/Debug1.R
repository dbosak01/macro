#%let env <- dev
#%let dev_path <- ./dev/data
#%let prod_path <- ./prod/data

# Path to data
#%if ("env." == "prod")
pth <- "prod_path./dm.sas7bdat"
#%else
pth <- "dev_path./dm.sas7bdat"
#%end
