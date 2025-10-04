<!-- badges: start -->

[![macro version](https://www.r-pkg.org/badges/version/macro)](https://cran.r-project.org/package=macro)
[![macro lifecycle](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://cran.r-project.org/package=macro)
[![macro downloads](https://cranlogs.r-pkg.org/badges/macro)](https://cran.r-project.org/package=macro)
[![macro total downloads](https://cranlogs.r-pkg.org/badges/grand-total/macro)](https://cran.r-project.org/package=macro)
[![R-CMD-check](https://github.com/dbosak01/macro/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dbosak01/macro/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# Introduction to **macro**
<!--img src="man/images/macro_new.png" align="left" height="138px" style="height:138px;margin-right:10px"/-->
<img src="man/figures/logo.png" align="left" height="138px" style="height:138px;margin-right:10px" alt="macro website" />


The SAS and R programming languages are very different.  One of the major differences is that
SAS has a "macro" language, and R does not.  The SAS macro language provides 
a code pre-processor that allows text-based substitution and manipulation 
of your code before it is executed.  Normally, in R, such a facility is not needed.
There are times, however, when it would be handy to have such a capability.
The **macro** package provides that capability.

The **macro** package offers the most basic "macro" functionality, such as:

1. Assigning macro variables.
2. Conditional macro logic.
3. Including code from external files.
4. Macro do loops.

The **macro** package will give some consolation to SAS
programmers transitioning to R who are accustomed to working with the SAS macro language.

More importantly, the **macro** functionality allows you to perform some operations that 
can be messy to perform natively in R. For instance, the macro functions are 
great for dynamic code generation.  

For additional reading, examples, and a complete function reference, refer to
the **macro** documentation site [here](https://macro.r-sassy.org/articles/macro.html).

### Installation

To install the **macro** package, you
may do so by running the following command from your R console:

    install.packages("macro")


Then put the following line at the top of your script:

    library(macro)

The **macro** package will then be loaded, and available for use in your project.


### Getting Help

If you need help with the **macro** package, the best place 
to turn to is the [macro](https://macro.r-sassy.org) web site. 
This web site offers many examples, and full
documentation on every function.  

If you want to look at the code for the **macro** package, visit the
github page [here](https://github.com/dbosak01/macro).

If you encounter a bug or have a feature request, please submit your
issue [here](https://github.com/dbosak01/macro/issues)

### See Also

The **macro** package is part of the **sassy** meta-package. 
The **sassy** meta-package includes several packages that help make R
easier for SAS® programmers.  You can read more about the **sassy** package
[here](https://sassy.r-sassy.org).

