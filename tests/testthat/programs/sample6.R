
#% Here is a macro comment

# Here is a regular comment

#% Assignment using left arrow
#%let a <- 1
print(`&a`)

#% Assignment using equals sign
#%let b = 2
print(`&b`)

#% Clear assignment
#%let b
print(`&b`)

#% Assignment using left arrow
`&c` <- 3
print(`&c`)
