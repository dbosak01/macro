
#%macro fork(a, b =, c = NULL)
print("&a")
print("&b")
print("&c")
x <- "&a"
y <- "&b"
z <- "&c"
#%mend


#%fork(2, 3, hello)
#%fork(4, 6)
#%fork(7)


