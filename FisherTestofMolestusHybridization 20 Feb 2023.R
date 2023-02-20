dat <- data.frame(
  "diapausing" = c(3, 5),
  "non-diapausing" = c(1, 7),
  row.names = c("MolHyb", "Pipiens"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("MolHyb", "Pipiens")

dat
chisq.test(dat)$expected
test <- fisher.test(dat)
test
citation("ggplot2")
citation("maps")

