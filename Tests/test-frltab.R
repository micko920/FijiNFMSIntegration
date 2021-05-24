
setwd("..")
set.seed(08121976) #Seed set to remove random nature of MC Analysis for LCI & UCI
source(file = "./fiji_frl_all_R_code.R")

test_that("FRL Table Exists", {
  expect_equal(exists("frltab"), TRUE)
})

# Deforestation
test_that("FRL Table has expected aaneDF", {
  expect_equal(round(frltab[10, 2]), 2696831)
})


test_that("FRL Table has expected aaneDFlci", {
  expect_equal(round(frltab[10, 3]), 2143450)

})

test_that("FRL Table has expected aaneDFuci", {
  expect_equal(round(frltab[10, 4]), 3161518)

})

# Degradation
test_that("FRL Table has expected aaneFD", {
  expect_equal(round(frltab[11, 2]), 310442)
})

test_that("FRL Table has expected aaneFDlci", {
  expect_equal(round(frltab[11, 3]), 256766)

})

test_that("FRL Table has expected aaneFDuci", {
  expect_equal(round(frltab[11, 4]), 333500)

})

# Sinks
test_that("FRL Table has expected aaneEC", {
  expect_equal(round(frltab[12, 2]), -1370469)
})

test_that("FRL Table has expected aaneEClci", {
  expect_equal(round(frltab[12, 3]), -1594489)

})

test_that("FRL Table has expected aaneECuci", {
  expect_equal(round(frltab[12, 4]), -1025019)

})

# FRL
test_that("FRL Table has expected FRL", {
  expect_equal(round(frltab[13, 2]), 1636804)
})

test_that("FRL Table has expected FRLlci", {
  expect_equal(round(frltab[13, 3]), 1226558)

})

test_that("FRL Table has expected FRLuci", {
  expect_equal(round(frltab[13, 4]), 2142065)
})




