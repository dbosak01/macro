
# Here is a normal comment

#%let x = 3
#%let y = %sysfunc(floor(2 + 2.5))
#%let z = %sysfunc(9/&x. + max(&x.))
#%let q = floor(2 + 2.5)
#%let f = %symexist(x)
#%let g = %symexist(k)

print("z is &z.")

#%if (&x. == 1)

# A normal comment
print("X is one")

#%elseif (&x. == 2)

# Here is a another normal comment
print("X is two")

#%else

# Comment something
print("X is &x.")
a <- `&x.`
#%end

print(paste("z is", `&z.`))
print("Y is &y.")
b <- `&y.`
q <- `&q.`
d <- `&z.`
f <- `&f.`
g <- `&g.`

