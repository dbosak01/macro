
# Analysis for "mpg"
mean_mpg <- mean(mtcars[["mpg"]])
min_mpg <- min(mtcars[["mpg"]])
max_mpg <- max(mtcars[["mpg"]])
anal_mpg <- data.frame("Variable" = "mpg",
                            "Mean" = mean_mpg,
                            "Min"  = min_mpg,
                            "Max"  = max_mpg)
# Analysis for "cyl"
mean_cyl <- mean(mtcars[["cyl"]])
min_cyl <- min(mtcars[["cyl"]])
max_cyl <- max(mtcars[["cyl"]])
anal_cyl <- data.frame("Variable" = "cyl",
                            "Mean" = mean_cyl,
                            "Min"  = min_cyl,
                            "Max"  = max_cyl)
# Analysis for "disp"
mean_disp <- mean(mtcars[["disp"]])
min_disp <- min(mtcars[["disp"]])
max_disp <- max(mtcars[["disp"]])
anal_disp <- data.frame("Variable" = "disp",
                            "Mean" = mean_disp,
                            "Min"  = min_disp,
                            "Max"  = max_disp)

# Combine analysis blocks
final <- rbind(anal_mpg, anal_cyl, anal_disp)

# Print result
print(final)
