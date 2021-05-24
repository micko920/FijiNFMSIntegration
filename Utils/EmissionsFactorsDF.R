load(file = "./Data/fiji_frl_input.RData")



# Merge tree data from the small and large concentric circles
trees <- rbind(nfi_r3, nfi_r2)
# Add circle ID to 'trees'
trees$circle <- c(rep("r3", nrow(nfi_r3)), rep("r2", nrow(nfi_r2)))

# Merge tree data with species, cluster sub-plot and cluster plot data
# Merge 'trees' and 'nfi_species'.
trees <- merge(trees,
               nfi_species[,c("family", "genus", "species", "SpeciesKey", "wd", "sdwd",
                              "levelwd")],
               by = "SpeciesKey", all.x = TRUE, sort = FALSE)
# Merge 'trees' and 'nfi_spid'. 'nfi_spid' holds data on NFI cluster sub-plots.
trees <- merge(trees,
               nfi_spid[,c("PlotRecordingsId", "SampleClusterInformationId",
                           "PlotLocation")],
               all.x = TRUE, by = "PlotRecordingsId", sort = FALSE)[,-1]
# Merge 'trees' and 'nfi_cid'. 'nfi_cid' holds data on NFI cluster plots.
trees <- merge(trees,
               nfi_cid[,c("SampleClusterInformationId", "Province")],
               all.x = TRUE, sort = TRUE)

# Sample sizes (number of trees and number of cluster plots)
(n_nfi_trees <- nrow(trees))      # Trees
(n_nfi_plots <- nrow(nfi_cid))    # Cluster plots

# Fit a height model to the data collected during Fiji's PSP program
# Merge PSP tree data with species data
ph <- merge(psp_trees, nfi_species[,c("SpeciesKey","genus")],
            all.x = TRUE, by.x = "Species", by.y = "SpeciesKey", order = FALSE)

# Number of pine trees in 'trees'
nrow(trees[trees$genus == "Pinus",])

# Remove pine trees from PSP dataset (as there is only a single pine tree in 'trees')
ph <- ph[ph$genus != "Pinus",]

# Remove trees that do not have data on 'Species', 'Bole_DBH' and 'Crown_TreeHeight'
ph <- ph[!is.na(ph$Species) & !is.na(ph$Bole_DBH) & !is.na(ph$Crown_TreeHeight),]

# Number of trees used to fit the height model
n_psp_trees <- nrow(ph)

# Fit a very simple height model
height_model_psp <- lm(Crown_TreeHeight ~ log(Bole_DBH), data = ph)
# R-squared of the fitted height model (rather poor..)
r2_height_model_psp <- summary(height_model_psp)$r.squared

# Predict height of NFI trees using the PSP height model
trees$height <- predict(height_model_psp,
                        newdata = data.frame(Bole_DBH = trees$DBH))

# Refit biomass model using data from the Pan-tropical Tree Harvest Database (PTHD)
nlsChave <- gnls(dry_total_agb ~ a *
                   (wood_specific_gravity * dbh^2 * total_height)^b,
                 data = pthd,
                 start = list(a = .01, b = .9),
                 weights = varPower(form = ~I(dbh)))

# Predict AGB of NFI trees using coefficients from the refitted AGB model (we
# divide by 1000 because the model predicts AGB in kg not in metric tonnes)
trees$agb <- coef(nlsChave)[1] *
  (trees$wd * trees$height * trees$DBH^2)^coef(nlsChave)[2] / 1000

# Split NFI tree data (large circle r3 and small circle r2). Circle sizes differ and so
# do plot expansion factors.
trees_r3 <- trees[trees$circle == "r3",]
trees_r2 <- trees[trees$circle == "r2",]

# There are some cluster plots where no trees were measured on certain circle sizes
# (e.g., no tree measured on the large circles r3 on cluster plot 758; only small tree
# were present; plot does not appear in 'trees_r3'). We want to keep these "empty" plots
# in the dataset.

# Rename variables (less typing, less clumsy code)
nfi_cid$cid <- nfi_cid$SampleClusterInformationId
trees_r3$cid <- trees_r3$SampleClusterInformationId
trees_r2$cid <- trees_r2$SampleClusterInformationId

# Identify plots where no trees were measured on the circle size (e.g., plot 758 exists,
# but no large trees were measured on the cluster plot).
# trees[trees$SampleClusterInformationId == 758,]
mis_plots_r3 <- unique(nfi_cid$cid)[!(unique(nfi_cid$cid) %in% unique(trees_r3$cid))]
mis_plots_r2 <- unique(nfi_cid$cid)[!(unique(nfi_cid$cid) %in% unique(trees_r2$cid))]

# Convert 'cid' to factor
trees_r3$cid_F <- as.factor(trees_r3$cid)
trees_r2$cid_F <- as.factor(trees_r2$cid)

# Add factor levels of missing plots). Factor levels of the missing plots are added to
# existing factor levels.
levels(trees_r3$cid_F) <- c(levels(trees_r3$cid_F), mis_plots_r3)
levels(trees_r2$cid_F) <- c(levels(trees_r2$cid_F), mis_plots_r2)

# Aggregate plot level AGB (large circle)
# The R package data.table allows for fast aggregation of data. The object 'trees_r3' is
# converted to class 'data.table'.
t3dt <- data.table(trees_r3)

# Aggregate at subplot level (freq = number of tree stems; agb = above-ground biomass).
spid_r3 <- t3dt[,list(freq = .N, agb3 = sum(agb, na.rm = TRUE)),
                by = list(cid_F, PlotLocation)]

# Set key (sorting the data.table by 'cid_F' and 'PlotLocation')
setkeyv(spid_r3, c("cid_F", "PlotLocation"))

# Add plots without measured trees (add missing cluster plots), i.e., create a cross
# join that adds missing plots with freq, agb = NA
spid_r3 <- spid_r3[CJ(levels(t3dt[, cid_F]), unique(t3dt[, PlotLocation])),]

# Convert data.table to data.frame
spid_r3 <- as.data.frame(spid_r3)

# Rename variables
names(spid_r3) <- c("cid",            # Cluster plot ID (SampleClusterInformationId)
                    "PlotLocation",    # Cluster sub-plot location (C, N, E, S, W)
                    "freq",            # Number of trees on the cluster sub-plot
                    "agb3"             # AGB at the cluster sub-plot
)

# If 'agb' is NA on the subplot (i.e., no trees) convert to zero. 'freq' is not needed
# for the analysis and is therefore ignored.
spid_r3$agb3 <- ifelse(is.na(spid_r3$agb3), 0, spid_r3$agb3)

# Aggregate 'agb' of cluster sub-plots at cluster plot level
cid_r3 <- aggregate(agb3 ~ cid, data = spid_r3, FUN = sum)

# Do the same for the smaller circle size (r2)
t2dt <- data.table(trees_r2)
spid_r2 <- t2dt[,list(freq = .N, agb2 = sum(agb, na.rm = TRUE)),
                by = list(cid_F, PlotLocation)]
setkeyv(spid_r2, c("cid_F", "PlotLocation"))
spid_r2 <- spid_r2[CJ(levels(t2dt[, cid_F]), unique(t2dt[, PlotLocation])),]
spid_r2 <- as.data.frame(spid_r2)
names(spid_r2) <- c("cid", "PlotLocation", "freq", "agb2")
spid_r2$agb2 <- ifelse(is.na(spid_r2$agb2), 0, spid_r2$agb2)
cid_r2 <- aggregate(agb2 ~ cid, data = spid_r2, FUN = sum)

# Create new data.frame named 'nfi'
nfi <- cid_r3
# Rename column 'agb3' to 'agb3p' to indicate that it is the AGB at the plot (p) level
# and not per hectare.
names(nfi)[2] <- "agb3p"
# Add column with AGB from the small circles (r2), use 'p' again to indicate that it is
# the AGB at the plot level (and not per hectare).
nfi$agb2p <- cid_r2[,"agb2"]

# Head of 'nfi'
head(nfi)

# Plot expansion factors for large and small circles
EF3 <- 10000 / 400 / 5  # 1 hectare / circle area [m^2] / number of cluster sub-plots
EF2 <- 10000 / 100 / 5  # 1 hectare / circle area [m^2] / number of cluster sub-plots

# Compute AGB per hectare for the cluster plots
nfi$agb3 <- nfi$agb3p * EF3     # AGB per hectare (large circle)
nfi$agb2 <- nfi$agb2p * EF2     # AGB per hectare (small circle)
nfi$agb <- nfi$agb3 + nfi$agb2  # AGB per hectare (agg. AGB from large and small circle)

# Merge 'nfi' with cluster plot data ('nfi_cid')
nfi <- merge(nfi,
             nfi_cid[,c("cid",
                        "Island",
                        "Division",
                        "Province",
                        "NewDistrict",
                        "ForestType",
                        "XCoordinate",
                        "YCoordinate",
                        "Date")],
             all.x = TRUE, sort = FALSE, by = "cid")

# Compute root-to-shoot ratios
# Root-to-shoot ratios (R) are computed at the cluster plot level. The value of R is
# selected depending on the altitude at cluster plot center and Aridity Index at cluster
# plot center. Values are taken from see Table 4.4 in IPCC Volume 4, Chapter 4

# Merge 'nfi' with 'nfi_ai_esf_srtm' (to get AI and SRTM data at cluster plot center)
nfi <- merge(nfi,
             nfi_ai_esf_srtm[,c("SampleClusterInformationId", "ai", "srtm")],
             by.x = "cid",
             by.y = "SampleClusterInformationId",
             sort = FALSE, all.x = TRUE)

# Assign root-to-shoot ratios to NFI cluster plots depending on elevation and
# AI (see Table 4.4 in IPCC Volume 4, Chapter 4)
nfi$rsr <- NA
for(i in 1:nrow(nfi)){
  if(nfi$srtm[i] >= param.srtmthresh){    # Tropical mountain systems
    nfi$rsr[i] <- .27
  } else if(nfi$ai[i] >= param.aithresh){ # Tropical rainforest
    nfi$rsr[i] <- .37
  } else if(nfi$ai[i] < param.aithresh){
    if(nfi$agb[i] < 125){               # Tropical moist deciduous (<125 tAGB ha^-1)
      nfi$rsr[i] <- .2
    } else {                            # Tropical moist deciduous (>125 tAGB ha^-1)
      nfi$rsr[i] <- .24
    }
  }
}

# Compute plot level BGB, carbon and carbon dioxide equivalents
nfi$bgb <- nfi$agb * nfi$rsr        # below-ground biomass (BGB)
nfi$tb <- nfi$agb + nfi$bgb         # total biomass
nfi$tc <- nfi$tb * param.etacf      # total carbon
nfi$co2e <- nfi$tc * param.etacc    # carbon dioxide equivalents

# Compute average carbon stocks in Fiji's Natural Forest
# Stratified sampling was used for the NFI 2006. First, strata sizes are extracted from
# 'nfi_strata'.
A_closed_forest <- nfi_strata[8,2]
A_open_forest <- nfi_strata[8,3]

# Strata sizes are added as a column to the plots (fpc = finite population correction)
nfi$fpc <- ifelse(nfi$ForestType == "CLOSED", A_closed_forest, A_open_forest)

# Sampling weights are added (inverse of inclusion probabilities)
# Number of observation in strata
n_strata <- tapply(nfi$agb, nfi$ForestType, length)
n_closed <- n_strata[1]         # Number of plots in closed forest
n_open <- n_strata[2]           # Number of plots in open forest
# Sampling weights
nfi$w <- ifelse(test = nfi$ForestType == "CLOSED",
                yes = 1 / (n_closed / A_closed_forest),
                no = 1 / (n_open / A_open_forest))

# Add a column that indicates whether the cluster plot center is located in the FRL
# Accounting Area or not. The islands included are Viti Levu, Vanua Levu and Taveuni.
nfi$redd <- as.factor(ifelse(nfi$Island %in% c("VITI LEVU","VANUA LEVU","TAVEUNI"),
                             yes = "yes", no = "no"))

# Add a column that indicates if the plot is located in Lowland or Upland (the threshold
# is 600 m a.s.l.).
nfi$asl <- ifelse(nfi$srtm <= param.srtmthresh, yes = "Lowland", no = "Upland")
# Number of observations in Low- and Upland Natural Forest
n_Lowland <- nrow(nfi[nfi$asl == "Lowland",])
n_Upland <- nrow(nfi[nfi$asl == "Upland",])

# Add a column that indicates if the plot is located in wet or dry areas (the Aridity
# Index is used to distinguish between wet and dry areas; the threshold is AI = 2).
nfi$dw <- ifelse(nfi$ai >= param.aithresh, yes = "Wet", no = "Dry")

# Rename stratum indicator variable ('ForestType') to 'stratum'
nfi$stratum <- nfi$ForestType

# Define survey design (R functions from package 'survey')
svy <- svydesign(id = ~1, strata = ~stratum, weights = ~w, fpc = ~fpc, data = nfi)

# The domains Low- and Upland Natural Forest (within and outside the FRL Accounting
# Area) cut across the strata closed and open forest. Therefore, appropriate domain
# estimators have to be used (luckily 'svyby' does the job...).
nfi_tc_ha_LU_FRL <- svyby(formula = ~tc, by = ~asl+redd, design = svy, FUN = svymean,
                          keep.names = FALSE, vartype = c("se", "ci"))
# Average total carbon stocks for the FRL Accounting Area (Low- and Upland Natural
# Forest)
nfi_tc_ha <- nfi_tc_ha_LU_FRL[2:3,-2]

# Average total carbon stocks in the FRL Accounting Area
nfi_tc_ha_FRL <- svyby(formula = ~tc, by = ~redd, design = svy, FUN = svymean,
                       keep.names = FALSE, vartype = c("se", "ci"))[2,-1]

# Strata estimates (closed and open forest)
closedopen <- svyby(formula = ~tc, by = ~redd+stratum, design = svy, FUN = svymean,
                    keep.names = FALSE, vartype = c("se", "ci"))

# Average total carbon stock (AGB and BGB) per hectare
nfi_tc_ha

# Select data needed for the MC simulation
trees3MC <- trees_r3        # NFI 2006 trees (large circle)
trees2MC <- trees_r2        # NFI 2006 trees (small circle)
phMC <- ph                  # PSP tree data (first round; 2010)
pthdMC <- pthd              # Pan-tropical Tree Harvest Database

# Rename some variables (less typing, less clumsy code)
phMC$dbh <- phMC$Bole_DBH
phMC$height <- phMC$Crown_TreeHeight
phMC$Bole_DBH <- NULL               # Remove variable 'Bole_DBH'
phMC$Crown_TreeHeight <- NULL       # Remove variable 'Crown_TreeHeight'

# Create a data.frame that collects the results of the first 'for' loop.
MC <- data.frame(
  cid = nfi$cid,         # Cluster plot ID
  fpc = nfi$fpc,         # Finite population correction
  w = nfi$w,             # Sampling weight
  redd = nfi$redd,       # Within the FRL Accounting Area"
  asl = nfi$asl,         # Altitude a.s.l.
  stratum = nfi$stratum  # Stratum (closed/open)
)

# Start simulation =====================================================================
# Start at 1
k <- 1

# PROGRESS BAR: initiate
# pb <- txtProgressBar(min = 0, max = param.runs, style = 3)

# First MC loop ........................................................................
for(k in 1:param.runs){ # k <- 1

  # Predict tree heights of NFI 2006 trees ###########################################
  # Take a sample from the PSP plots (not trees!) with replacement
  Sph <- sample(length(unique(phMC$PSPPlotNo)),
                length(unique(phMC$PSPPlotNo)), replace = TRUE)
  # Select variables from PSP tree dataset and create 'phi'
  phi <- phMC[,c("PSPPlotNo","dbh","height")]
  # Select tress from PSP plots (select plots from 'Sph')
  for(l in 1:length(Sph)){ # l <- 1
    if(l == 1){
      # Create data.frame with trees from first PSP plot in 'Sph'
      Sphi <- phi[phi$PSPPlotNo == Sph[l],]
    } else {
      # Attach tree data from PSP plot l in 'Sph'
      Sphii <- phi[phi$PSPPlotNo == Sph[l],]
      Sphi <- rbind(Sphi, Sphii)
    }
  }
  # The data.frame 'phMCk' is a bootstrap sample of trees from the PSP plots (some
  # trees may appear more than once, because a 'with replacement' sample was drawn
  # from the PSP plots).
  phMCk <- Sphi

  # Add measurement error to DBH from Normal distribution N(mean = 0, sd = DBH * 0.1)
  phMCk$dbh <- phMCk$dbh + rnorm(nrow(phMCk), 0, phMCk$dbh * param.merrdbh)

  # Fit a simple height model to the PSP (bootstrap) sample (parameter estimates will
  # change at every MC run, because the data in 'Sph' will be different every time).
  lmmhNPk <- lm(height ~ log(dbh), data = phMCk)

  # Predict heights of PSP trees using the model fitted to the PSP bootstrap sample.
  phMCk$predHeight <- predict(lmmhNPk, data.frame(dbh = phMCk$dbh))

  # The following steps are needed to add random error to the predicted total tree
  # heights of NFI 2006 trees. We follow the methods described in McRoberts & Westfall
  # [2014].

  # Compute the residuals
  phMCk$resid <- phMCk$height - phMCk$predHeight

  # Model residual variance of height model
  HLhk <- data.frame(
    e = phMCk$resid,         # Residuals from PSP height model
    h = phMCk$height,        # "True" (measured) height
    hhat = phMCk$predHeight, # Predicted height from PSP height model
    dbh = phMCk$dbh          # DBH of PSP trees
  )
  # Remove rows where predicted PSP tree height is 'NA' (should not happen).
  HLhk <- HLhk[!is.na(HLhk$hhat),]

  # Split height predictions into groups
  # Define the number of groups (25 trees in each group)
  ngk <- round(nrow(HLhk) / 25)
  # Order data.frame by predicted tree heights
  HLhk <- with(HLhk, HLhk[order(hhat),])
  # Assign group membership
  HLhk$g <- rep(1:ngk, each = 25)[1:nrow(HLhk)]

  # Mean tree height in group
  gHLhk <- aggregate(h ~ g, HLhk, mean)
  # Mean predicted tree height in group
  gHLhk$hhat <- aggregate(hhat ~ g, HLhk, mean)[,2]
  # Residual variance in group
  gHLhk$eh <- aggregate(e ~ g, HLhk,
                        function(x) sqrt(sum(x^2)/(length(x) - 1)))[,2]

  # Model group-wise residual variance as a function of predicted height
  lmgHLhk <- lm(eh ~ -1 + hhat, data = gHLhk)

  # Predict tree height of NFI 2006 trees using the (bootstrap) PSP height model
  trees3MC$height <- predict(lmmhNPk, data.frame(dbh = trees3MC$DBH))
  trees2MC$height <- predict(lmmhNPk, data.frame(dbh = trees2MC$DBH))

  # Predict residual variance for NFI trees
  trees3MC$varHtrees <- predict(lmgHLhk, data.frame(hhat = trees3MC$height))
  trees2MC$varHtrees <- predict(lmgHLhk, data.frame(hhat = trees2MC$height))

  # Predict random tree height of NFI trees: predicted height + error
  trees3MC$heightp <- trees3MC$height + rnorm(nrow(trees3MC), 0, trees3MC$varHtrees)
  trees2MC$heightp <- trees2MC$height + rnorm(nrow(trees2MC), 0, trees2MC$varHtrees)

  # If predicted height is equal or below zero, assign 0
  trees3MC$height <- ifelse(trees3MC$heightp <= 0, trees3MC$height, trees3MC$heightp)
  trees2MC$height <- ifelse(trees2MC$heightp <= 0, trees2MC$height, trees2MC$heightp)

  # Add a random value for wood density (wd). 'wd' and 'sdwd' are taken from the wood
  # density database (Chave et al. [2009] and Zanne et al. [2009])
  trees3MC$rho3 <- rnorm(trees3MC$wd, mean = trees3MC$wd, sd = trees3MC$sdwd)
  trees2MC$rho2 <- rnorm(trees2MC$wd, mean = trees2MC$wd, sd = trees2MC$sdwd)

  # If random wood density 'rho' is equal or below zero assign 0.1
  trees3MC$rho3 <- ifelse(trees3MC$rho3 <= 0, 0.1, trees3MC$rho3)
  trees2MC$rho2 <- ifelse(trees2MC$rho2 <= 0, 0.1, trees2MC$rho2)

  # Predict AGB of NFI 2006 trees ####################################################
  # Take a bootstrap sample of trees from the Pan-tropical Tree Harvest Database
  # (PTHD). Data are in 'pthd'. For the AGB predictions the same procedures are used
  # as for predicted tree heights. That is, a random error is added to predicted AGB
  # using the methods described in McRoberts & Westfall [2014].
  pthdMCk <- pthdMC[sample(nrow(pthdMC), nrow(pthdMC), replace = TRUE),]

  # Refit Chave's biomass model using the bootstrap sample
  nlsChaveTk <- gnls(dry_total_agb ~ a *
                       (wood_specific_gravity * dbh^2 * total_height)^b,
                     data = pthdMCk,
                     start = list(a = .01, b = .9),
                     weights = varPower(form = ~I(dbh)))

  # Predict tree heights of the PTHD trees
  pthdMCk$predAgb <- predict(nlsChaveTk,
                             data.frame(dbh = pthdMCk$dbh,
                                        wood_specific_gravity = pthdMCk$wood_specific_gravity,
                                        total_height = pthdMCk$total_height))

  # Convert AGB in kilograms to AGB in metric tonnes
  pthdMCk$predAgb <- pthdMCk$predAgb / 1000

  # Residuals (in metric tonnes)
  pthdMCk$resid <- pthdMCk$dry_total_agb / 1000 - pthdMCk$predAgb

  # Model residual variance of AGB model
  AGBLhk <- data.frame(
    e = pthdMCk$resid,
    agb = pthdMCk$dry_total_agb / 1000,
    agbhat = pthdMCk$predAgb
  )
  # Remove row if it contains NA
  AGBLhk <- AGBLhk[!is.na(AGBLhk$agbhat),]

  # Split AGB predictions into groups
  # Define the number of groups (25 trees in each group)
  ngk <- round(nrow(AGBLhk) / 25)
  # Order data.frame by predicted AGB
  AGBLhk <- with(AGBLhk, AGBLhk[order(agbhat),])
  # Assign group membership
  AGBLhk$g <- rep(1:ngk, each = 25)[1:nrow(AGBLhk)]

  # Mean AGB in group
  gAGBLhk <- aggregate(agb ~ g, AGBLhk, mean)
  # Mean predicted AGB in group
  gAGBLhk$agbhat <- aggregate(agbhat ~ g, AGBLhk, mean)[,2]
  # Residual variance in group
  gAGBLhk$eagb <- aggregate(e ~ g, AGBLhk,
                            function(x) sqrt(sum(x^2)/(length(x) - 1)))[,2]

  # Model group-wise residual variance as a function of predicted AGB
  lmgAGBLhk <- lm(eagb ~ -1 + agbhat, data = gAGBLhk)

  # Predict tree AGB of NFI 2006 trees using the refitted bootstrap AGB model
  trees3MC$agb <- coef(nlsChaveTk)[1] *
    (trees3MC$rho3 * trees3MC$height *
       (trees3MC$DBH + rnorm(nrow(trees3MC), 0, trees3MC$DBH *
                               param.merrdbh))^2)^coef(nlsChaveTk)[2] / 1000
  trees2MC$agb <- coef(nlsChaveTk)[1] *
    (trees2MC$rho2 * trees2MC$height *
       (trees2MC$DBH + rnorm(nrow(trees2MC), 0, trees2MC$DBH *
                               param.merrdbh))^2)^coef(nlsChaveTk)[2] / 1000

  # Predict residual variance for NFI trees
  trees3MC$varAGBtrees <- predict(lmgAGBLhk, data.frame(agbhat = trees3MC$agb))
  trees2MC$varAGBtrees <- predict(lmgAGBLhk, data.frame(agbhat = trees2MC$agb))

  # Predict random AGB of NFI trees: predicted AGB + modeled error
  trees3MC$agbMCMC <- trees3MC$agb + rnorm(trees3MC$agb, 0, trees3MC$varAGBtrees)
  trees2MC$agbMCMC <- trees2MC$agb + rnorm(trees2MC$agb, 0, trees2MC$varAGBtrees)

  # Cluster plot level AGB ###########################################################
  # Aggregate predicted AGB of individual trees at sub-plot level (r = 3).
  t3dt <- data.table(trees3MC)
  res3 <- t3dt[,list(freq = .N, agb3 = sum(agbMCMC, na.rm = TRUE)),
               by=list(cid_F,PlotLocation)]
  setkeyv(res3,c("cid_F", "PlotLocation"))
  res3 <- res3[CJ(levels(t3dt[,cid_F]), unique(t3dt[,PlotLocation])),]
  res3 <- as.data.frame(res3)
  names(res3) <- c("cid","PlotLocation","freq","agb3")
  res3$agb3 <- ifelse(is.na(res3$agb3), 0, res3$agb3)
  res3 <- aggregate(agb3 ~ cid, res3, sum)

  # Aggregate predicted AGB of individual trees at sub-plot level (r = 2).
  t2dt <- data.table(trees2MC)
  res2 <- t2dt[,list(freq = .N, agb2 = sum(agbMCMC, na.rm = TRUE)),
               by=list(cid_F, PlotLocation)]
  setkeyv(res2,c("cid_F", "PlotLocation"))
  res2 <- res2[CJ(levels(t2dt[,cid_F]), unique(t2dt[,PlotLocation])),]
  res2 <- as.data.frame(res2)
  names(res2) <- c("cid","PlotLocation","freq","agb2")
  res2$agb2 <- ifelse(is.na(res2$agb2), 0, res2$agb2)
  res2 <- aggregate(agb2 ~ cid, res2, sum)

  # Create a new data.frame named 'nfii'
  nfii <- res3
  names(nfii)[2] <- "agb3p"   # Rename to 'agb3p' to indicate plot AGB
  nfii$agb2p <- res2[,2]      # Add plot AGB from small circles

  # Compute AGB per hectare and cluster plot (per circle)
  nfii$agb3 <- nfii$agb3p * EF3
  nfii$agb2 <- nfii$agb2p * EF2

  # AGB per hectare and cluster plot (sum of AGB ha^-1 of large and small circle)
  nfii$agb <- nfii$agb3 + nfii$agb2

  # Get data on Aridity Index, elevation (SRTM) and stratum (open and closed forest)
  nfiii <- merge(nfii, nfi[,c("cid","ai","srtm","stratum")],
                 by = "cid", all.x = TRUE, sort = FALSE)

  # Select the correct value for the root-to-shoot ratio. Root-to-shoot ratios are
  # sampled from Triangular distributions using values provided in IPCC [2006; Vol. 4,
  # Chap. 4, Tab. 4.4]
  # Tropical mountain systems
  Ruk <-   rtriangle(n = 1, theta = .27, lower = .2699999, upper = .28)
  # Tropical rainforest
  Rlwk <-  rtriangle(1, .37, .37 - .37 * param.errRlwk, .37 + .37 * param.errRlwk)
  # Tropical moist deciduous forest (<125 tAGB ha^-1)
  Rldk1 <- rtriangle(1,.20, .09, .25)
  # Tropical moist deciduous forest (>=125 tAGB ha^-1)
  Rldk2 <- rtriangle(1,.24, .22, .33)

  # Create a variable that holds data on the root-to-shoot ratio
  nfiii$rsr <- NA
  # Assign random root-to-shoot ratio
  for(i in 1:nrow(nfiii)){
    if(nfiii$srtm[i] >= param.srtmthresh){
      nfiii$rsr[i] <- Ruk
    } else if(nfiii$ai[i] >= param.aithresh){
      nfiii$rsr[i] <- Rlwk
    } else if(nfiii$ai[i] <  param.aithresh){
      if(nfiii$agb[i] < 125){
        nfiii$rsr[i] <- Rldk1
      } else {
        nfiii$rsr[i] <- Rldk2
      }
    }
  }

  # Compute cluster plot level total biomass
  nfiii$tb <- (nfiii$agb + nfiii$agb * nfiii$rsr)

  # Merge MC dataset (created at above) with 'nfiii'
  MC <- merge(MC, nfiii[,c("cid","tb")], all.x = TRUE)

  # Rename columns that are added to 'MC' at each iteration
  names(MC)[ncol(MC)] <- paste("tb", k, sep = "")

  # PROGRESS BAR: +1
  #     setTxtProgressBar(pb, k)
}

# PROGRESS BAR: close
# close(pb)

# Structure of 'MC'
str(MC)
# First six rows of 'MC'
head(MC)

# Reset the index 'i'
i <- 1

# Create a data.frame that collects the results (Low- and Upland Natural Forest)
res <- as.matrix(data.frame(run = rep(0, param.runs),
                            pm  = rep(0, param.runs),
                            mL  = rep(0, param.runs),
                            mU  = rep(0, param.runs)))

# For closed and open forest
resCO <- as.matrix(data.frame(run = rep(0, param.runs),
                              mC  = rep(0, param.runs),
                              mO  = rep(0, param.runs)))

# Split 'MC' dataset into closed and open forest
nfic <- data.table(MC[MC$stratum == "CLOSED",])
nfio <- data.table(MC[MC$stratum == "OPEN",])

# Draw bootstrap samples from closed and open forest strata
Snfic <- list()
Snfio <- list()
# for(i in 1:runs){
for(i in 1:param.runs){
  Snfic[[i]] <- sample(nrow(nfic), nrow(nfic), replace = TRUE)
  Snfio[[i]] <- sample(nrow(nfio), nrow(nfio), replace = TRUE)
}

# PROGRESS BAR: initiate
# pb <- txtProgressBar(min = 0, max = param.runs, style = 3)

# Run bootstrap ........................................................................
for(i in 1:param.runs){ # i <- 2
  nfiic <- nfic[Snfic[[i]],]  # Select observation closed forest
  nfiio <- nfio[Snfio[[i]],]  # Select observation open forest
  # Combine selected observations from closed and open forest
  nfii <- data.frame(rbindlist(list(nfiic, nfiio))) # Bootstrap NFI dataset
  # Create survey design (stratified simple random sampling)
  svyi <- svydesign(id = ~1, strata = ~stratum, weights = ~w, fpc = ~fpc,
                    data = nfii[,c(1:6,i+6)])
  # Population mean
  pm <- svymean(as.formula(paste("~",names(nfii)[i+6], sep = "")), svyi)
  # Means for Low- and Upland Forest
  mi <- svyby(as.formula(paste("~",names(nfii)[i+6], sep = "")),
              ~redd+asl, svyi, svymean, keep.names = FALSE)
  # Means for closed and open forest
  miCO <- svyby(as.formula(paste("~",names(nfii)[i+6], sep = "")),
                ~redd+stratum, svyi, svymean, keep.names = FALSE)

  # Merge results (Low- and Upland Natural Forest domains)
  res[i,1] <- i
  res[i,2] <- c(pm[1])
  res[i,3] <- c(mi[mi$asl == "Lowland" & mi$redd == "yes",3])
  res[i,4] <- c(mi[mi$asl == "Upland"  & mi$redd == "yes",3])

  # Merge results (closed and open forest)
  resCO[i,1] <- i
  resCO[i,2] <- c(miCO[miCO$stratum == "CLOSED" & miCO$redd == "yes",3])
  resCO[i,3] <- c(miCO[miCO$stratum == "OPEN"  & miCO$redd == "yes",3])

  # PROGRESS BAR: +1
  #     setTxtProgressBar(pb, i)
}

# PROGRESS BAR: close
# close(pb)

# MC estimates of average total biomass per hectare
res

# Convert matrix to data.frame
c_stock_mc <- as.data.frame(res)        # Low- and Upland Natural Forest
c_stock_mc_CO <- as.data.frame(resCO)   # Closed and open forest

# Convert from biomass to carbon
c_stock_mc[,2:4] <- c_stock_mc[,2:4] * param.etacf
c_stock_mc_CO[,2:3] <- c_stock_mc_CO[,2:3] * param.etacf

# Extract carbon stocks in Low- and Upland Natural Forest
carbon_L <- nfi_tc_ha[1,2]
carbon_U <- nfi_tc_ha[2,2]

# Rename 'c_stock_mc'
v_carbon <- c_stock_mc

# Carbon stocks in Low- and Upland Natural Forest
rs_nfi_mc <- data.frame(stratum = c("Lowland", "Upland"),
                        carbon_t_ha = c(carbon_L, carbon_U),
                        # Quantiles of MC estimates
                        lci_carbon_t_ha = c( # Lower
                          quantile(v_carbon[,3], probs = param.qlci),
                          quantile(v_carbon[,4], probs = param.qlci)
                        ),
                        uci_carbon_t_ha = c( # Upper
                          quantile(v_carbon[,3], probs = param.quci),
                          quantile(v_carbon[,4], probs = param.quci)
                        )
)
# Rename rows
row.names(rs_nfi_mc) <- 1:nrow(rs_nfi_mc)
# Print results
rs_nfi_mc

# Carbon stocks in grassland [tC ha^-1]
(c_grass <- param.cgrass)

# Relative error in the estimate of carbon stocks in grassland
param.errcgrass

# Simulate MC estimates of carbon stocks in grassland
v_c_grass <- rtriangle(n = param.runs,
                       theta = c_grass,
                       lower = c_grass - c_grass * param.errcgrass,
                       upper = c_grass + c_grass * param.errcgrass
)
# Show simulated values
v_c_grass

# Result table for carbon stocks in Fijian grasslands
rs_c_grass <- data.frame(carbon_t_ha = c_grass,
                         lci_carbon_t_ha = quantile(v_c_grass, probs = param.qlci),
                         uci_carbon_t_ha = quantile(v_c_grass, probs = param.quci))
row.names(rs_c_grass) <- 1
# Show result table
rs_c_grass

# Carbon stock change (deforestation) for Low- and Upland Natural Forest
(dc <- as.vector(nfi_tc_ha[1:2, 2]) - c_grass)

# MC estimates of carbon stock change (deforestation)
(v_dc <- v_carbon[,-1] - v_c_grass)

# Carbon stock change in Low- and Upland Natural Forest
rs_nfi_carbonloss <- data.frame(stratum = c("Lowland","Upland"),
                                carbon_loss_t_ha = dc,
                                lci_carbon_loss_t_ha = c(
                                  quantile(v_dc[,2], probs = param.qlci),
                                  quantile(v_dc[,3], probs = param.qlci)),
                                uci_carbon_loss_t_ha = c(
                                  quantile(v_dc[,2], probs = param.quci),
                                  quantile(v_dc[,3], probs = param.quci))
)
# Rename rows
row.names(rs_nfi_carbonloss) <- 1:nrow(rs_nfi_carbonloss)
# Show result table
rs_nfi_carbonloss



# Emission factors for deforestation in Low- and Upland Natural Forest
df_ef <- data.frame(
  stratum = c("Lowland","Upland"),
  ef_tco2e_ha = c(dc[1] * param.etacc, dc[2] * param.etacc),
  lci_ef_tco2e_ha = c(quantile(v_dc[,2], probs = param.qlci) * param.etacc,
                      quantile(v_dc[,3], probs = param.qlci) * param.etacc),
  uci_ef_tco2e_ha = c(quantile(v_dc[,2], probs = param.quci) * param.etacc,
                      quantile(v_dc[,3], probs = param.quci) * param.etacc)
)
# Rename rows
row.names(df_ef) <- 1:nrow(df_ef)
# Show result table
df_ef
