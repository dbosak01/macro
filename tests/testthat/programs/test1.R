
# Syntax basically same as SAS
#%let x = 1;

#%if (&x; == 1) %then %do;

print("X is one")

#%end;
#%elseif (&x == 2)

print("X is two")

#%end;
#%else %do;

print("X is &x;")

#%end;








