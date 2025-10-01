#%let a <- c(1.205, 4.683, 3.812, 6.281, 9.467)

#%if (%sysfunc(mean(a.)) > 5)
x <- ">5"
#%else
x <- "<=5"
#%end
print(x)
