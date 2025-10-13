# Assign integer value
#%let x = 1
v1 <- `&x.`
# Assign double value
#%let x = 1.2
v2 <- `&x.`
# Assign character value
#%let x = "Hello World!"
v3 <- `&x.`
# Assign date value
#%let x = as.Date("2025-07-15")
v4 <- `&x.`
# Assign vector
#%let x = c(1, 2, 3, 4, 5)
v5 <- `&x.`
# Assign calculated value
#%let x = 2 + 2
v6 <- `&x.`
# Assign using another macro variable
#%let y = &x.
v7 <- `&y.`
# Calculate using another macro variable
#%let y = &x. + 1
v8 <- `&y.`
v9 <- `&z.`
