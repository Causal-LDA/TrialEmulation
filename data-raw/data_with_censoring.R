set.seed(200)
data_censored <- data_gen_censored(
  ns = 100,
  nv = 20
)

colnames(data_censored) <- c(
  "id", "period", "treatment", "x1", "x2", "x3", "x4", "age",
  "age_s", "outcome", "censored", "eligible"
)

if (FALSE) {
  # only run this if you're sure!
  save(data_censored, file = "data/data_censored.rda", compress = "bzip2")
}
