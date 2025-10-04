
# Create temp file
tmp <- file.path(tempdir(), "test.R")
out <- file.path(tempdir(), "test_mod.R")

# Write macro code to temp file
cat("#%let a <- 1\n", file = tmp)
cat("#%if (a. == 1)\n", file = tmp, append = TRUE)
cat("print('a is one!')\n", file =tmp, append = TRUE)
cat("#%else\n", file = tmp, append = TRUE)
cat("print('a is something else!')\n", file = tmp, append = TRUE)
cat("#%end", file = tmp, append = TRUE)
cat("\n", file = tmp, append = TRUE)

# Process temp file
res <- msource(tmp, out)
# [1] "a is one!"

# Read output file
readLines(res$output)
# [1] "print('a is one!')"

# Show in debugger
msource(tmp, out, debug = TRUE)
# ********************************************************************************
# **  Pre-Processing
# ********************************************************************************
# -    File In: C:\Users\dbosa\AppData\Local\Temp\RtmpyM9Kep/test.R
# -   File Out: C:\Users\dbosa\AppData\Local\Temp\RtmpyM9Kep/test_mod.R
# ********************************************************************************
# [ In#][Out#]:
# [   1][    ]: #%let a <- 1
# [   2][    ]: #%if (a. == 1)
# [   3][   1]: print('a is one!')
# [   4][    ]: #%else
# [   5][    ]: print('a is something else!')
# [   6][    ]: #%end
# ********************************************************************************
# **  Execution
# ********************************************************************************
#
# > print('a is one!')
# [1] "a is one!"
#
# ********************************************************************************
# **  End
# ********************************************************************************
