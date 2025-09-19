

#%let a = 1

#%if (%symexist(a))

  print("Macro variable 'a' exists!")
  print("Here is the value: a.")

#%else

  print("Macro variable 'a' does not exist!")

#%end


#%if (%symexist(b))

  print("Macro variable 'b' exists!")
  print("Here is the value: b.")

#%else

  print("Macro variable 'b' does not exist!")

#%end
