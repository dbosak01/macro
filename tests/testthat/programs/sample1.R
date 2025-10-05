#% Assign integer value
#%let v1 = 1

#% Assign double value
#%let v2 = 1.2

#% Assign character value
#%let v3 = "Hello World!"

#% Assign date value
#%let v4 = as.Date("2025-07-15")

#% Assign vector
#%let v5 = c(1, 2, 3, 4, 5)

#% Assign calculated value
#%let v6 = 2 + 2

#% Assign using another macro variable
#%let v7 = v6.

#% Calculate using another macro variable
#%let v8 = v7. + 1

# Print inside loop
for (idx in seq(1, v6.)) {

  print(v3.)
}


# Resolve inside a comment
# The value of 'v1' is v1.

# Resolve in a text string
a <- "The value of 'v2' is v2."
print(a)

# Resolve in open code
b <- v3.
print(b)

# Resolve in a function
c <- mean(v5.)
print(c)
