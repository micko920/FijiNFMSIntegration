
setwd("..")
set.seed(08121976) #Seed set to remove random nature of MC Analysis for LCI & UCI

# rm(list=c("MR_Values", "EmRems_Values", "ER_Values","UC_EmRems_Values")[
#   which(c("MR_Values", "EmRems_Values", "ER_Values","UC_EmRems_Values") %in% ls())])

create_env <- function() {
   code_env <- new.env()
   source(local= code_env, file = "./Fiji_ER_Estimate_Values.R", echo=FALSE)
   
   return(list(MR_Values=code_env$MR_Values, EmRems_Values=code_env$EmRems_Values, ER_Values=code_env$ER_Values))
}


# "***** EmRems_Values *******"

test_that("All of them", {

   expect_output(v <- create_env())

# Year1

# test_that("EmRems_Values$year1$EstEmDeforUp value is correct", {
    expect_equal(round(v$EmRems_Values$year1$EstEmDeforUp), 535467)

 # test_that("EmRems_Values$year1$EstEmDeforLow value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstEmDeforLow), 2161360)

 # test_that("EmRems_Values$year1$GrossEmDefor value is correct", {
   expect_equal(round(v$EmRems_Values$year1$GrossEmDefor), 2696827)

 # test_that("EmRems_Values$year1$EstEmFell value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstEmFell), 195316)

 # test_that("EmRems_Values$year1$EstRemFell value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstRemFell), -42362)

 # test_that("EmRems_Values$year1$NetEmRemsFell value is correct", {
   expect_equal(round(v$EmRems_Values$year1$NetEmRemsFell), 152955)

 # test_that("EmRems_Values$year1$EstEmFire value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstEmFire), 81559)

 # test_that("EmRems_Values$year1$EstRemARefor value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstRemARefor), -59545)

 # test_that("EmRems_Values$year1$EstEmFPlnHwd value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstEmFPlnHwd), 154194)

 # test_that("EmRems_Values$year1$EstEmFPlnSwd value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstEmFPlnSwd), 442001)

 # test_that("EmRems_Values$year1$EstRemFPlnHwd value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstRemFPlnHwd), -911581)

 # test_that("EmRems_Values$year1$EstRemFPlnSwd value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstRemFPlnSwd), -852651)

 # test_that("EmRems_Values$year1$GrossEmFPln value is correct", {
   expect_equal(round(v$EmRems_Values$year1$GrossEmFPln), 596195)

 # test_that("EmRems_Values$year1$GrossRemFPln value is correct", {
   expect_equal(round(v$EmRems_Values$year1$GrossRemFPln), -1764232)

 # test_that("EmRems_Values$year1$NetEmRemsFPln value is correct", {
   expect_equal(round(v$EmRems_Values$year1$NetEmRemsFPln), -1168037)

 # test_that("EmRems_Values$year1$GrossEm value is correct", {
   expect_equal(round(v$EmRems_Values$year1$GrossEm), 3569897)

 # test_that("EmRems_Values$year1$GrossRem value is correct", {
   expect_equal(round(v$EmRems_Values$year1$GrossRem), -1866139)

 # test_that("EmRems_Values$year1$EstEmRemsFDeg value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstEmRemsFDeg), 234514)

 # test_that("EmRems_Values$year1$EstEmRemsEnh value is correct", {
   expect_equal(round(v$EmRems_Values$year1$EstEmRemsEnh), -1227582)

 # test_that("EmRems_Values$year1$NetEmRems value is correct", {
   expect_equal(round(v$EmRems_Values$year1$NetEmRems), 1703759)

# Year2

 # test_that("EmRems_Values$year2$EstEmDeforUp value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmDeforUp), 535467)

 # test_that("EmRems_Values$year2$EstEmDeforLow value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmDeforLow), 2161360)

 # test_that("EmRems_Values$year2$GrossEmDefor value is correct", {
   expect_equal(round(v$EmRems_Values$year2$GrossEmDefor), 2696827)

 # test_that("EmRems_Values$year2$EstEmFell value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmFell), 195316)

 # test_that("EmRems_Values$year2$EstRemFell value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstRemFell), -42362)

 # test_that("EmRems_Values$year2$NetEmRemsFell value is correct", {
   expect_equal(round(v$EmRems_Values$year2$NetEmRemsFell), 152955)

 # test_that("EmRems_Values$year2$EstEmFire value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmFire), 81559)

 # test_that("EmRems_Values$year2$EstRemARefor value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstRemARefor), -59545)

 # test_that("EmRems_Values$year2$EstEmFPlnHwd value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmFPlnHwd), 154194)

 # test_that("EmRems_Values$year2$EstEmFPlnSwd value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmFPlnSwd), 442001)

 # test_that("EmRems_Values$year2$EstRemFPlnHwd value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstRemFPlnHwd), -911581)

 # test_that("EmRems_Values$year2$EstRemFPlnSwd value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstRemFPlnSwd), -852651)

 # test_that("EmRems_Values$year2$GrossEmFPln value is correct", {
   expect_equal(round(v$EmRems_Values$year2$GrossEmFPln), 596195)

 # test_that("EmRems_Values$year2$GrossRemFPln value is correct", {
   expect_equal(round(v$EmRems_Values$year2$GrossRemFPln), -1764232)

 # test_that("EmRems_Values$year2$NetEmRemsFPln value is correct", {
   expect_equal(round(v$EmRems_Values$year2$NetEmRemsFPln), -1168037)

 # test_that("EmRems_Values$year2$GrossEm value is correct", {
   expect_equal(round(v$EmRems_Values$year2$GrossEm), 3569897)

 # test_that("EmRems_Values$year2$GrossRem value is correct", {
   expect_equal(round(v$EmRems_Values$year2$GrossRem), -1866139)

 # test_that("EmRems_Values$year2$EstEmRemsFDeg value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmRemsFDeg), 234514)

 # test_that("EmRems_Values$year2$EstEmRemsEnh value is correct", {
   expect_equal(round(v$EmRems_Values$year2$EstEmRemsEnh), -1227582)

 # test_that("EmRems_Values$year2$NetEmRems value is correct", {
   expect_equal(round(v$EmRems_Values$year2$NetEmRems), 1703759)


# [1] "****** ER_Values *******"

 # test_that("ER_Values$MpGrossEmDefor value is correct", {
   expect_equal(round(v$ER_Values$MpGrossEmDefor), 5393654)

 # test_that("ER_Values$MpEstEmRemsFDeg value is correct", {
   expect_equal(round(v$ER_Values$MpEstEmRemsFDeg), 469027)

 # test_that("ER_Values$MpEstEmRemsEnh value is correct", {
   expect_equal(round(v$ER_Values$MpEstEmRemsEnh), -2455164)

 # test_that("ER_Values$MpNetEmRems value is correct", {
   expect_equal(round(v$ER_Values$MpNetEmRems), 3407517)

 # test_that("ER_Values$MpEstFRL value is correct", {
   expect_equal(round(v$ER_Values$MpEstFRL), 3273600)

 # test_that("ER_Values$MpEstERs value is correct", {
   expect_equal(round(v$ER_Values$MpEstERs), -133917)

 # test_that("ER_Values$MpEstFRLFDeg value is correct", {
   expect_equal(round(v$ER_Values$MpEstFRLFDeg), 620884)

 # test_that("ER_Values$MpEstERsFDeg value is correct", {
   expect_equal(round(v$ER_Values$MpEstERsFDeg), 151857)


# [1] "***** MR_Values ********"
# year1

 # test_that("MR_Values$year1$year value is correct", {
   expect_equal(v$MR_Values$year1$year, "2018")

 # test_that("MR_Values$year1$GrossEmDefor value is correct", {
   expect_equal(round(v$MR_Values$year1$GrossEmDefor), 2696827)

 # test_that("MR_Values$year1$EstEmRemsFDeg value is correct", {
   expect_equal(round(v$MR_Values$year1$EstEmRemsFDeg), 234514)

 # test_that("MR_Values$year1$EstEmRemsEnh value is correct", {
   expect_equal(round(v$MR_Values$year1$EstEmRemsEnh), -1227582)

 # test_that("MR_Values$year1$NetEmRems value is correct", {
   expect_equal(round(v$MR_Values$year1$NetEmRems), 1703759)

# $year2

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

 # test_that("MR_Values$MpGrossEmDefors value is correct", {
   expect_equal(round(v$MR_Values$MpGrossEmDefor), 5393654)


 # test_that("MR_Values$MpEstEmRemsFDeg value is correct", {
   expect_equal(round(v$MR_Values$MpEstEmRemsFDeg), 469027)


 # test_that("MR_Values$MpEstEmRemsEnh value is correct", {
   expect_equal(round(v$MR_Values$MpEstEmRemsEnh), -2455164)


 # test_that("MR_Values$MpNetEmRems value is correct", {
   expect_equal(round(v$MR_Values$MpNetEmRems), 3407517)


 # test_that("MR_Values$RpNetEmRems value is correct", {
   expect_equal(round(v$MR_Values$RpNetEmRems), 2520629)


 # test_that("MR_Values$MpEstFRL value is correct", {
   expect_equal(round(v$MR_Values$MpEstFRL), 3273600)


 # test_that("MR_Values$RpEstFRL value is correct", {
   expect_equal(round(v$MR_Values$RpEstFRL), 2421567)


 # test_that("MR_Values$MpEstERs value is correct", {
   expect_equal(round(v$MR_Values$MpEstERs), -133917)


# # test_that("MR_Values$RpEstERs value is correct", {
   expect_equal(round(v$MR_Values$RpEstERs), -99062)

})
