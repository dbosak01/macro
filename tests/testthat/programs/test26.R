#%let x <- 1

#%let y <- c("One",
#%>          "Two",
#%>          "Three")

# Something after

#%if ("Two" %in% &y)
#%let z <- "Found it!"
#%else
#%let z <- "Sorry!"
#%end

print(`&z`)


