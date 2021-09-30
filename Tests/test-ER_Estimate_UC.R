
setwd("..")
set.seed(08121976) #Seed set to remove random nature of MC Analysis for LCI & UCI


# rm(list=c("MR_Values", "EmRems_Values", "ER_Values","UC_EmRems_Values")[
#    which(c("MR_Values", "EmRems_Values", "ER_Values","UC_EmRems_Values") %in% ls())])

create_env <- function() {
  code_env <- new.env()
  source(local= code_env, file = "./Fiji_ER_Estimate_UC.R", echo=FALSE)
  
  return(list(MR_Values=code_env$MR_Values, EmRems_Values=code_env$EmRems_Values, ER_Values=code_env$ER_Values))
}


# *** MR_Values *** #

test_that("All of them", {
  
  expect_output(v <- create_env())
  
#test_that("MR_Values$year1$year value is correct", {
  expect_equal(v$MR_Values$year1$year, "2018")

#test_that("MR_Values$year1$GrossEmDefor value is correct", {
  expect_equal(round(v$MR_Values$year1$GrossEmDefor), 2696827)

 #test_that("MR_Values$year1$EstEmRemsFDeg value is correct", {
   expect_equal(round(v$MR_Values$year1$EstEmRemsFDeg), 234514)
   
# test_that("MR_Values$year1$EstEmRemsEnh value is correct", {
 expect_equal(round(v$MR_Values$year1$EstEmRemsEnh), -1227582)

# test_that("MR_Values$year1$NetEmRems value is correct", {
   expect_equal(round(v$MR_Values$year1$NetEmRems), 1703759)
 
# test_that("MR_Values$year2$year value is correct", {
   expect_equal(v$MR_Values$year2$year, "2019")

# test_that("MR_Values$year2$GrossEmDefor value is correct", {
   expect_equal(round(v$MR_Values$year2$GrossEmDefor), 2696827)

# test_that("MR_Values$year2$EstEmRemsFDeg value is correct", {
   expect_equal(round(v$MR_Values$year2$EstEmRemsFDeg), 234514)
 
# test_that("MR_Values$year2$EstEmRemsEnh value is correct", {
   expect_equal(round(v$MR_Values$year2$EstEmRemsEnh), -1227582)
 
# test_that("MR_Values$year2$NetEmRems value is correct", {
   expect_equal(round(v$MR_Values$year2$NetEmRems), 1703759)
 
# test_that("MR_Values$MpGrossEmDefor value is correct", {
   expect_equal(round(v$MR_Values$MpGrossEmDefor), 5393654)
 
# test_that("MR_Values$MpEstEmRemsFDeg value is correct", {
 expect_equal(round(v$MR_Values$MpEstEmRemsFDeg), 469027)

# test_that("MR_Values$MpEstEmRemsEnh Value is correct", {
   expect_equal(round(v$MR_Values$MpEstEmRemsEnh), -2455164)
 
# test_that("MR_Values$MpNetEmRems Value is correct", {
   expect_equal(round(v$MR_Values$MpNetEmRems), 3407517)
 
# test_that("MR_Values$RpNetEmRems Value is correct", {
   expect_equal(round(v$MR_Values$RpNetEmRems), 2520629)
 
# test_that("MR_Values$MpEstFRL Value is correct", {
   expect_equal(round(v$MR_Values$MpEstFRL), 3273600)
 
# test_that("MR_Values$RpEstFRL Value is correct", {
   expect_equal(round(v$MR_Values$RpEstFRL), 2421567)
 
# test_that("MR_Values$MpEstERs Value is correct", {
   expect_equal(round(v$MR_Values$MpEstERs), -133917)
 
# test_that("MR_Values$RpEstERs Value is correct", {
   expect_equal(round(v$MR_Values$RpEstERs), -99062)
 
# test_that("MR_Values$MpEstERsDefEnh$median Value is correct", {
  expect_equal(round(v$MR_Values$MpEstERsDefEnh$median), 249020)

# test_that("MR_Values$MpEstERsDefEnh$UCI Value is correct", {
   expect_equal(as.numeric(round(v$MR_Values$MpEstERsDefEnh$UCI)), 1350196)
 
# test_that("MR_Values$MpEstERsDefEnh$LCI Value is correct", {
   expect_equal(as.numeric(round(v$MR_Values$MpEstERsDefEnh$LCI)), -905211)
 
# test_that("MR_Values$MpEstERsDefEnh$halfWidth Value is correct", {
   expect_equal(as.numeric(round(v$MR_Values$MpEstERsDefEnh$halfWidth)), 1127704)
 
# test_that("MR_Values$MpEstERsDefEnh$relativeMargin Value is correct", {
   expect_equal(as.numeric(signif(v$MR_Values$MpEstERsDefEnh$relativeMargin,6)), 4.52856)

#  test_that("MR_Values$MpEstERsDefEnh$conserFactor Value is correct", {
  expect_equal(signif(v$MR_Values$MpEstERsDefEnh$conserFactor,2), 0.15)
 
# test_that("MR_Values$MpEstERsFDeg$median Value is correct", {
   expect_equal(round(v$MR_Values$MpEstERsFDeg$median), 157308)

# test_that("MR_Values$MpEstERsFDeg$UCI Value is correct", {
  expect_equal(as.numeric(round(v$MR_Values$MpEstERsFDeg$UCI)), 234599)
 
# test_that("MR_Values$MpEstERsFDeg$LCI Value is correct", {
   expect_equal(as.numeric(round(v$MR_Values$MpEstERsFDeg$LCI)), 80425)

# test_that("MR_Values$MpEstERsFDeg$halfWidth Value is correct", {
   expect_equal(as.numeric(round(v$MR_Values$MpEstERsFDeg$halfWidth)), 77087)

# test_that("MR_Values$MpEstERsFDeg$relativeMargin Value is correct", {
  expect_equal(as.numeric(signif(v$MR_Values$MpEstERsFDeg$relativeMargin,6)), 0.490039)

#  test_that("MR_Values$MpEstERsFDeg$conserFactor Value is correct", {
expect_equal(v$MR_Values$MpEstERsFDeg$conserFactor, 0.08, tolerance=1e-3)
 
# test_that("MR_Values$ErpaCurrentFRL Value is correct", {
  expect_equal(round(v$MR_Values$ErpaCurrentFRL),2421567)

# test_that("MR_Values$ErpaCurrentEmRems Value is correct", {
   expect_equal(round(v$MR_Values$ErpaCurrentEmRems), 2520629)
 
# test_that("MR_Values$ErpaCurrentERs Value is correct", {
expect_equal(round(v$MR_Values$ErpaCurrentERs),-99062)
 
# test_that("MR_Values$ErpaCurrentBalance Value is correct", {
  expect_equal(round(v$MR_Values$ErpaCurrentBalance),-99062)

# test_that("MR_Values$IsReversal Value is correct", {
   expect_equal(v$MR_Values$IsReversal, TRUE)

# test_that("MR_Values$RpCanceledERs Value is correct", {
   expect_equal(v$MR_Values$RpCanceledERs, NaN)
 
# test_that("MR_Values$RpEstERsFDeg Value is correct", {
 expect_equal(round(v$MR_Values$RpEstERsFDeg), 112332)
 
# test_that("MR_Values$RpEstERsDefEnh Value is correct", {
  expect_equal(round(v$MR_Values$RpEstERsDefEnh), -211394)

# test_that("MR_Values$RpSetaside Value is correct", {
  expect_equal(round(v$MR_Values$RpSetaside), -22723)

# test_that("MR_Values$RpAdjERs Value is correct", {
  expect_equal(round(v$MR_Values$RpAdjERs), -76339)
 
# test_that("MR_Values$RpPotentialERs Value is correct", {
  expect_equal(round(v$MR_Values$RpPotentialERs), -76339)

# test_that("MR_Values$RpBufferedERs Value is correct", {
  expect_equal(round(v$MR_Values$RpBufferedERs), -12214)

# test_that("MR_Values$RpERs Value is correct", {
 expect_equal(round(v$MR_Values$RpERs), -64125)

})

