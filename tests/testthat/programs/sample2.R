#% Assign integer value
#%let x = 1

#%if (x. == 1)
# X is one
print("Inside the first condition")
#%elseif (x. == 2)
# X is two
print("Inside the second condition")
#%else
# X is something else
print("Inside the else.")
#%end
