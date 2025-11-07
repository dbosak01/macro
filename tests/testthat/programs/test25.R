#%let y <- &x.
#%let z <- &x. + &y.
a <- `&z`
print(a)

#%macro fork(b, c = 2)
g <- `&b` + `&c`
print(g)
#%mend

#%macro bork(b, c = 2)
h <- `&b` + `&c`
print(h)
#%mend

#%macro sammy()
print("Hello!")
#%mend


#%fork(2)
#%bork(4, 6)
#%sammy()


