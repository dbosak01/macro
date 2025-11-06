
#% Determine appropriate data path
#%if ("&env." == "prod")
  #%let pth <- /projects/prod/data
#%else
  #%let pth <- /projects/dev/data
#%end
