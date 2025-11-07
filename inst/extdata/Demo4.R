#% Create some macro variables
#%let x <- 1
#%let y <- 2
#%let z <- &x + &y

#% Create a macro function
#%macro test(vl = Hello!)
print("&vl")
#%mend

