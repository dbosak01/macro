#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#% Assign macro variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%let x = 2
#%let y = floor(1 + 2.5)
#%let z = %sysfunc(floor(1 + 2.5))

# Initial comment
print("Starting program")

#%if (&x == 1)

print("X is one")
print("another one")

#%end
#%if (&x == 2)

print("X is two")
print("another one")

#%end
#%if (&x. > 2)

print("X is &x.")
print("another one")

#%end

# Resolved assignment
a <- `&y`

# Results
print(paste("a is", a))
print("y is y.")
print("z is z.")
