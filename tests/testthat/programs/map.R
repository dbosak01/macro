



#% Here is a comment

#%let x <- 1

#%if (&x == 1)
  print("First condition")
#%elseif (&x == 2)
  print("Second condition")
#%else
  print("Default condition")
#%end

#%include "./tests/testthat/programs/sample1.R"
#%include "myfile.R"

print("Y is %sysfunc(min(c(1, 2, 3)))")

#%do y = 1 %to 3
  print("Y is &y")
#%end


#%macro test()
  print("Hello!")
#%mend
#%test()




