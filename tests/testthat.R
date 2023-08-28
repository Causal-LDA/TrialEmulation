library(testthat)
library(TrialEmulation)

data.table::setDTthreads(2)
test_check("TrialEmulation")
