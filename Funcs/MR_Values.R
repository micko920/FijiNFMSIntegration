
create_EstMRValues <- function(UC_ER, ER, EmRems, MV, MRparams) {
  MR <- list()
  MR$year1 <- list()
  MR$year2 <- list()

  # Table4_2
  MR$year1$year <- MV$year1$year
  MR$year2$year <- MV$year2$year

  MR$year1$EstEmRemsDefor <- EmRems$year1$EmEstDFTotal
  MR$year2$EstEmRemsDefor <- EmRems$year2$EmEstDFTotal

  MR$MpEstEmRemsDefor <- ER$MpEstEmRemsDefor

  MR$year1$EstEmRemsFDeg <- EmRems$year1$FDEst
  MR$year2$EstEmRemsFDeg <- EmRems$year2$FDEst
  MR$MpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg

  MR$year1$EstEmRemsEnh <- EmRems$year1$ECEst
  MR$year2$EstEmRemsEnh <- EmRems$year2$ECEst
  MR$MpEstEmRemsEnh <- ER$MpEstEmRemsEnh

  MR$year1$NetEmRems <- EmRems$year1$NetEmTotal
  MR$year2$NetEmRems <- EmRems$year2$NetEmTotal

  # Table4_2, Table 4_3
  MR$MpNetEmRems <- ER$MpNetEmRems
  MR$RpNetEmRems <- ER$MpNetEmRems * MRparams$RpMpRatio

  MR$MpEstFRL <- ER$MpEstFRL
  MR$RpEstFRL <- ER$MpEstFRL * MRparams$RpMpRatio

  MR$MpEstERs <- ER$MpEstERs
  MR$RpEstERs <- ER$MpEstERs * MRparams$RpMpRatio
  return(MR)
}


create_MRValues <- function(UC_ER, ER, EmRems, MV, MRparams) {
  MR <- list()
  MR$year1 <- list()
  MR$year2 <- list()

  # Table4_2
  MR$year1$year <- MV$year1$year
  MR$year2$year <- MV$year2$year

  MR$year1$EstEmRemsDefor <- EmRems$year1$EmEstDFTotal
  MR$year2$EstEmRemsDefor <- EmRems$year2$EmEstDFTotal

  MR$MpEstEmRemsDefor <- ER$MpEstEmRemsDefor

  MR$year1$EstEmRemsFDeg <- EmRems$year1$FDEst
  MR$year2$EstEmRemsFDeg <- EmRems$year2$FDEst
  MR$MpEstEmRemsFDeg <- ER$MpEstEmRemsFDeg

  MR$year1$EstEmRemsEnh <- EmRems$year1$ECEst
  MR$year2$EstEmRemsEnh <- EmRems$year2$ECEst
  MR$MpEstEmRemsEnh <- ER$MpEstEmRemsEnh

  MR$year1$NetEmRems <- EmRems$year1$NetEmTotal
  MR$year2$NetEmRems <- EmRems$year2$NetEmTotal

  # Table4_2, Table 4_3
  MR$MpNetEmRems <- ER$MpNetEmRems
  MR$RpNetEmRems <- ER$MpNetEmRems * MRparams$RpMpRatio

  MR$MpEstFRL <- ER$MpEstFRL
  MR$RpEstFRL <- ER$MpEstFRL * MRparams$RpMpRatio

  MR$MpEstERs <- ER$MpEstERs
  MR$RpEstERs <- ER$MpEstERs * MRparams$RpMpRatio

  # Table5_2_2
  MR$MpEstERsDefEnh <- UC_ER$MpEstERsDefEnh
  MR$MpEstERsFDeg <- UC_ER$MpEstERsFDeg

  # Table7_2
  # A - MR$RpEstFRL
  # B - MRparams$ErpaPreviousFRL
  # C
  MR$ErpaCurrentFRL <- MR$RpEstFRL + MRparams$ErpaPreviousFRL
  # D - MR$RpNetEmRems
  # E - MRparams$ErpaPreviousEmRems
  # F
  MR$ErpaCurrentEmRems <- MR$RpNetEmRems + MRparams$ErpaPreviousEmRems # D + E
  # G
  MR$ErpaCurrentERs <- MR$ErpaCurrentFRL - MR$ErpaCurrentEmRems # C - F
  # H - MRparams$ErpaPreviousERs
  # I
  MR$ErpaCurrentBalance <- MR$ErpaCurrentERs - MRparams$ErpaPreviousERs # G - H
  # J -  MRparams$ErpaTransferredERs
  # k
  MR$IsReversal <- MR$ErpaCurrentBalance < 0
  MR$RpCanceledERs <- ifelse(MR$IsReversal,
    # J / (H * (H - G))
    MRparams$ErpaTransferredERs / (MRparams$ErpaPreviousERs * (MRparams$ErpaPreviousERs - MR$ErpaCurrentERs)),
    0
  )

  # Table8
  # A - MR$RpEstERs
  # B
  MR$RpEstERsFDeg <- ER$MpEstERsFDeg * MRparams$RpMpRatio
  # C
  MR$RpEstERsDefEnh <- MR$RpEstERs - MR$RpEstERsFDeg #  A - B
  # D - MR$MpEstERsDefEnh$conserFactor
  # E (0.15 * B) + (C * D), 0.15 is default ConserFactor for FDeg
  MR$RpSetaside <- (MR$RpEstERsFDeg * MR$MpEstERsFDeg$conserFactor) +
    (MR$RpEstERsDefEnh * MR$MpEstERsDefEnh$conserFactor)
  # F
  MR$RpAdjERs <- MR$RpEstERs - MR$RpSetaside # A - E
  # G - MRparams$ErpaContestedERs
  # H - MRparams$ErpaSoldERs
  # I
  MR$RpPotentialERs <- MR$RpAdjERs - MRparams$ErpaContestedERs - MRparams$ErpaSoldERs # F - G - H
  # J - MRparams$ErpaRiskSetaside
  # K
  MR$RpBufferedERs <- MR$RpPotentialERs * MRparams$ErpaRiskSetaside # (I * J)
  # L,  there is a mistake in the Table. (I - L) should be (I - K)
  MR$RpERs <- MR$RpPotentialERs - MR$RpBufferedERs # I - K


  return(MR)
}
