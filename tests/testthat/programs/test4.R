

#%let x = 1
#%let y = 2

#%if (x. == 1)

  #%if (y. == 1)

    print("X is one and Y is 1")
    resx <- "one"
    resy <- 1

  #%else

    print("X is one and Y is y.")
    resx <- "one"
    resy <- y.

  #%end

#%elseif (x. == 2)

    #%if (y. == 1)

    print("X is two and Y is 1")

    #%else

    print("X is two and Y is y.")

    #%end

#%else

  #%if (y. == 1)

    print("X is x. and Y is 1")

  #%else

    print("X is x. and Y is y.")

  #%end

#%end

