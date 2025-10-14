print("Print the text: Hello!")


library(dplyr)


cat("Analysis for Sepal.Length\n")
iris |>
  select(Species, Sepal.Length) |>
  group_by(Species) |>
  summarize(Mean = mean(Sepal.Length, rm.na = TRUE),
            SD = sd(Sepal.Length),
            Quantile = quantile(Sepal.Length, probs = .25, rm.na = TRUE)) |>
  as.data.frame() |> print()
cat("\n")

cat("Analysis for Sepal.Width\n")
iris |>
  select(Species, Sepal.Width) |>
  group_by(Species) |>
  summarize(Mean = mean(Sepal.Width, rm.na = TRUE),
            SD = sd(Sepal.Width),
            Quantile = quantile(Sepal.Width, probs = .25, rm.na = TRUE)) |>
  as.data.frame() |> print()
cat("\n")

cat("Analysis for Petal.Length\n")
iris |>
  select(Species, Petal.Length) |>
  group_by(Species) |>
  summarize(Mean = mean(Petal.Length, rm.na = TRUE),
            SD = sd(Petal.Length),
            Quantile = quantile(Petal.Length, probs = .25, rm.na = TRUE)) |>
  as.data.frame() |> print()
cat("\n")

cat("Analysis for Petal.Width\n")
iris |>
  select(Species, Petal.Width) |>
  group_by(Species) |>
  summarize(Mean = mean(Petal.Width, rm.na = TRUE),
            SD = sd(Petal.Width),
            Quantile = quantile(Petal.Width, probs = .25, rm.na = TRUE)) |>
  as.data.frame() |> print()
cat("\n")

