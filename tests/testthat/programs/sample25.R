#%let trt_grps <- c("ARM A" = "Placebo",
#%>                 "ARM B" = "Drug 10mg",
#%>                 "ARM C" = "Drug 20mg",
#%>                 "ARM D" = "Competitor")

a <- `&trt_grps`
print(a)
