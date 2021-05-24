
# Load all necessary data
load(file = "./Data/fiji_frl_input.RData")

# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)

debug_frl <- FALSE #Turn printed output on
show_output <- TRUE #Turn final table printed output on


# Generic ..............................................................................
param.runs          = 10        # Number of Monte-Carlo runs
param.qlci          = 0.05      # Lower quantile (confidence bounds, 0.05 = 90% CI)
param.quci          = 0.95      # Upper quantile (confidence bounds, 0.95 = 90% CI)
param.etacf         = 0.47      # Biomass to carbon conversion (IPCC default)
param.etacc         = 44/12     # Carbon to CO_2e conversion (IPCC default * -1)

# Deforestation ........................................................................
param.srtmthresh    = 600       # Elevation threshold (<600m Lowland, Upland otherwise)
param.aithresh      = 2         # Aridity Index threshold (<2 dry, wet otherwise)

param.merrdbh       = 0.1       # Measurement error DBH 10% (NFI 2016 MC simulations)
param.Rlwk          = 0.37      # Root-to-shoot ration tropical rainforest (IPCC)
param.errRlwk       = 0.25      # Error root-to-shoot ratio tropical rainforest

param.cgrass        = 17.11364  # Average carbon stocks in grassland (Rounds, 2013)
param.errcgrass     = 0.75      # Percent error in 'param.cgrass'

# Forest degradation ...................................................................
param.efell         = 0.69      # Emission factor felling (component of TEF)
param.edam          = 0.15      # Emission factor damage (component of TEF)
param.einfr         = 0.21      # Emission factor infrastructure (component of TEF)
param.errtef        = 0.25      # Relative error of TEF
param.maiclnf       = 0.99      # Mean annual (total carbon increment in Natural Forest)
param.errmaiclnf    = 0.5       # Relative error in 'maiclnf'
param.erralnf       = 0.25      # Relative error in areas logged in Natural Forest

param.maibp         = 10        # Mean annual biomass increment in Softwood Plantations
param.errmaibp      = 0.25      # Relative error in 'maibp'

# Enhancement of carbon stocks .........................................................
param.maicar        = 1.918455  # Mean annual total carbon increment (AR)
param.errmaicar     = 0.5       # Relative error in maicar

param.bcefrhw       = 1.05     # Biomass conversion and expansion factor (Hardwood)
param.errbcefrhw    = 0.25     # Relative error in 'bcefrhw'
param.maivhww       = 5.85     # Weighted mean annual volume increment (Hardwood)
param.errmaivhw     = 0.25     # Relative error in 'maivhw' and 'maivhww'
param.bcefihw       = 1.1      # Biomass conversion and exp. factor (increment Hardwood)
param.errbcefihw    = 0.25     # Relative error in 'bcefihw'

param.wdsw          = 0.47     # Wood density Softwood
param.sdwdsw        = 0.0509   # Standard deviation of 'wdsw'
param.volTovol      = 0.76     # Harvested vol. to total vol. (recovered volume prop.)
param.errvolTovol   = 0.05     # Relative error in 'volTovol'
param.maibp         = 10       # Mean annual biomass increment in Softwood Plantations
param.errmaibp      = 0.25     # Relative error in 'maibp'
param.cuttingc      = 20       # Rotation length in year in Softwood Plantations
param.errcuttingc   = 5        # Error in 'cuttingc'
# Structure of 'lcc_mapped_areas'
if (debug_frl) str(lcc_mapped_areas)

# Print object 'lcc_mapped_areas'
if (debug_frl) lcc_mapped_areas

# Structure of 'aa_sample'
if (debug_frl) str(aa_sample)

# Head of 'aa_sample'
if (debug_frl) head(aa_sample)

# Number of sample points in the mapped classes
if (debug_frl) table(aa_sample$predicted)

# Get the total area mapped [ha]
A_mapped <- sum(lcc_mapped_areas[,2])

# Extract the area mapped of class i
A_mapped_i <- lcc_mapped_areas[,2]            # Extract area of class i
names(A_mapped_i) <- lcc_mapped_areas[,1]    # Assign the class code

# Compute the area proportion (mapped) of class i
round(W_i <- A_mapped_i / A_mapped, 5)

# Compute the sample error matrix (counts); map class in rows, reference class in columns
(err <- table(aa_sample$predicted, aa_sample$observed))

# Compute the sample error matrix (area proportions); map class in rows, reference class in columns
round(errp <- rep(W_i, length.out = length(W_i)^2) * (err / rowSums(err)), 5)

# Estimate class areas [ha]
(aa_est_areas <- A_mapped * colSums(errp))

# Load AA bootstrap function
source(file = "./Utils/aaboot.R")

# Take bootstrap sample and estimate areas (iterations = 10 times)
aa_boot <- aaboot(aa_sample = aa_sample,               # AA sample
                  areas_mapped = lcc_mapped_areas,      # Mapped areas of change
                  iterations = 10,                      # Number of iterations
                  progress_bar = FALSE)                 # Want a progress bar?

# Output of the function 'aaboot' with iterations = 10 runs
if (debug_frl) aa_boot

# Lower 90%-confidence bound
quantile(aa_boot[,"171"], probs = 0.05)
# Upper 90%-confidence bound
quantile(aa_boot[,"171"], probs = 0.95)

# Results of the accuracy assessment
rs_AA <- data.frame(
    # Change class code
    class_code = lcc_mapped_areas[,1],
    # Class description
    class_desc = c(
                   "Stable LF",     # LF = Lowland Natural Forest
                   "Stable UF",     # UF = Upland Natural Forest
                   "DF Lowland",    # DF = deforestation
                   "DF Upland",
                   "AR Lowland",    # AR = afforestation/reforestation
                   "AR Upland",
                   "Stable NF"      # NF = Non-Forest
                   ),
    # Mapped areas of change classes [ha]
    area_mapped_ha = lcc_mapped_areas[,2],
    # Estimated areas of change classes [ha]
    area_est_ha = aa_est_areas,
    # Lower limit of the 90%-confidence interval
    lci_area_ha = apply(aa_boot, 2, function(x) quantile(x, probs = param.qlci)),
    # Upper limit of the 90%-confidence interval
    uci_area_ha = apply(aa_boot, 2, function(x) quantile(x, probs = param.quci))
                    )
# Rename rows
row.names(rs_AA) <- 1:nrow(rs_AA)
if (debug_frl) rs_AA # Print results

# Extract change classes (remove stable classes)
rs_AA_annual <- rs_AA[3:6,]
# Compute annual average of change classes (10 years from mid 2006 to mid 2016)
rs_AA_annual[,3:6] <- rs_AA_annual[,3:6] / 10
# Rename rows
row.names(rs_AA_annual) <- 1:nrow(rs_AA_annual)

# DF = deforestation; AR = afforestation/reforestation
if (debug_frl) rs_AA_annual # Print

# Result table for deforestation (AD)
rs_AA_annual_df <- rs_AA_annual[1:2, c(2, 4:6)]
# Create row for the total (sum of Low- and Upland Natural Forest)
rs_AA_annual_df_total <- c(NA,
                           sum(rs_AA_annual_df[,2]),
                           quantile(aa_boot[,3] + aa_boot[,4], probs = param.qlci) / 10,
                           quantile(aa_boot[,3] + aa_boot[,4], probs = param.quci) / 10
                           )
# Merge results
rs_AA_annual_df <- rbind(rs_AA_annual_df, rs_AA_annual_df_total)
rs_AA_annual_df[,1] <- as.character(rs_AA_annual_df[,1])
rs_AA_annual_df[3,1] <- "Total"
if (debug_frl) rs_AA_annual_df # Print

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
if (debug_frl) head(nfi)

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
if (debug_frl) nfi_tc_ha

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
if (debug_frl) str(MC)
# First six rows of 'MC'
if (debug_frl) head(MC)

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
if (debug_frl) res

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
if (debug_frl) rs_nfi_mc

# Carbon stocks in grassland [tC ha^-1]
(c_grass <- param.cgrass)

# Relative error in the estimate of carbon stocks in grassland
if (debug_frl) param.errcgrass

# Simulate MC estimates of carbon stocks in grassland
v_c_grass <- rtriangle(n = param.runs,
                       theta = c_grass,
                       lower = c_grass - c_grass * param.errcgrass,
                       upper = c_grass + c_grass * param.errcgrass
                      )
# Show simulated values
if (debug_frl) v_c_grass

# Result table for carbon stocks in Fijian grasslands
rs_c_grass <- data.frame(carbon_t_ha = c_grass,
                         lci_carbon_t_ha = quantile(v_c_grass, probs = param.qlci),
                         uci_carbon_t_ha = quantile(v_c_grass, probs = param.quci))
row.names(rs_c_grass) <- 1
# Show result table
if (debug_frl) rs_c_grass

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
if (debug_frl) rs_nfi_carbonloss

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
if (debug_frl) df_ef

# Average annual area of deforestation in Low- and Upland Natural Forest
(resAADefor <- data.frame(stratum = c("Lowland","Upland"),
                          areaLoss = rs_AA_annual_df[1:2, 2]))

# Average annual area of deforestation in Low- and Upland Natural Forest (MC estimates)
MCaadeforL <- aa_boot[,3] / 10 # Lowland
MCaadeforU <- aa_boot[,4] / 10 # Upland

# Average annual carbon loss in Low- and Upland Natural Forest
(dfaaclL <- dc[1] * resAADefor[1,2]) # Lowland
(dfaaclU <- dc[2] * resAADefor[2,2]) # Upland

# Average annual emissions from deforestation
(df_L_aae <- dfaaclL * param.etacc)     # Lowland Natural Forest
(df_U_aae <- dfaaclU * param.etacc)     # Upland Natural Forest
(df_aae <- df_L_aae + df_U_aae)         # Low- and Upland Natural Forest

# MC estimates of average annual emissions from Lowland Natural Forest
v_df_L_aae <- MCaadeforL * v_dc[,2] * param.etacc
# MC estimates of average annual emissions from Upland Natural  Forest
v_df_U_aae <- MCaadeforU * v_dc[,3] * param.etacc

# Quantiles of MC average annual emissions from deforestation
# Lowland Natural Forest
lcidfaaeL <- quantile(v_df_L_aae, probs = param.qlci)
ucidfaaeL <- quantile(v_df_L_aae, probs = param.quci)
# Upland Natural Forest
lcidfaaeU <- quantile(v_df_U_aae, probs = param.qlci)
ucidfaaeU <- quantile(v_df_U_aae, probs = param.quci)
# Low- and Upland Natural Forest
lcidfaaeLU <- quantile(v_df_aae <- v_df_U_aae + v_df_L_aae, probs = param.qlci)
ucidfaaeLU <- quantile(v_df_U_aae + v_df_L_aae, param.quci)

# Result table: emissions from deforestation
rs_df <- data.frame(stratum = c("Lowland","Upland","Total"),
                    # Average annual emissions from deforestation
                    aa_em_tco2e_yr = c(df_L_aae, df_U_aae, df_aae),
                    # Lower confidence interval bound
                    lci_aa_em_tco2e_yr = c(lcidfaaeL, lcidfaaeU, lcidfaaeLU),
                    # Upper confidence interval bound
                    uci_aa_em_tco2e_yr = c(ucidfaaeL, ucidfaaeU, ucidfaaeLU)
                    )

rs_df_all <- rs_df          # Strata and aggregated strata
rs_df_strata <- rs_df[-3,]  # Strata only
rs_df <- rs_df[3,-1]        # Aggregated strata only
# Show result table
if (debug_frl) rs_df_all
# Volumes logged [m^3] in Natural Forest
if (debug_frl) lnf_volume
# Average annual volume extracted from Natural Forest [m^3]
(avg_vol_lnf <- mean(lnf_volume$volume))

# Years of the FRL Reference Period
Ty <- 2006:2016
# Number of years in the FRL Reference Period
Tl <- length(Ty)

# To 'translate' volumes extracted to carbon loss, the 'Total Emissions Factor' from
# Haas (2015) was used. The TEF has three components:
#   - EMFELL        C loss caused by the log itself (including logging residuals)
#   - EMDAM         C loss due to damage to the remaining stand
#   - EMINFR        C loss caused by logging infrastructure establishment
(EMFELL <- param.efell)
(EMDAM  <- param.edam)
(EMINFR <- param.einfr)

# Total Emission Factor (TEF)
(TEF <- EMFELL + EMDAM + EMINFR)

# Carbon loss per year in metric tonnes
# Create a data.frame for logging in Natural Forest (lnf); taking data from
# 'lnf_logging'
lnf <- lnf_volume

# Compute C loss for the different years
lnf$closs_t <- lnf_volume$volume * TEF

# Average annual C loss over the Reference Period
avg_closs_lnf <- mean(lnf$closs_t)

# Average annual emissions
(avg_co2e_lnf <- avg_closs_lnf * param.etacc)

# MC simulation to estimate the uncertainty in estimated emissions from logging in
# Natural Forest (source 'forest degradation')
fdaacl <- vector()

# Run MC simulation
for(i in 1:param.runs){ # i <- 1
    # Random TEF
    TEFi <- rtriangle(1, theta = TEF,
                      lower = TEF - TEF * param.errtef,
                      upper = TEF + TEF * param.errtef)
    # Average annual C loss caused by logging in Natural Forest (these values will be
    # converted to emissions below)
    fdaacl[i] <- avg_vol_lnf * TEFi
}

# Removals
# Mean annual increment (MAI) of total C (above- and below-ground carbon) for
# conventional logging (Mussong, unpublished)
(maic_lnf <- param.maiclnf)

# Years of growth over the Reference Period
deltaT <- 2016 - Ty + .5

# Above-ground carbon (AGC) accumulated over the Reference Period. This gives the C
# accumulation OVER the Reference Period.
lnf$cgain_t <- deltaT * lnf_area$area_harvested_total_ha * maic_lnf

# Areas harvested and C gains *over* the FRL Reference Period
data.frame(lnf_volume, area_harvested_ha = lnf_area[,5], c_gain = lnf$cgain_t)

# Total area harvested between 2006 and 2016
Aharvested <- round(sum(lnf_area$area_harvested_total_ha))

# Average annual C accumulation over the Reference Period
avg_cgain_lnf <- mean(lnf$cgain_t)

# Average annual CO2e removals over the Reference Period
(avg_co2r_lnf <- avg_cgain_lnf * param.etacc)

# Uncertainty assessment (MC simulation): gross removals after logging
fdaacg <- vector()     # Vector that collects the results

# Run MC simulation
for (i in 1:param.runs){ # i <- 1
    natfi <- lnf

    # Random MAI for C accumulation
    maicli <- rtriangle(n = 1, theta = maic_lnf,
                        lower = maic_lnf - maic_lnf * param.errmaiclnf,
                        upper = maic_lnf + maic_lnf * param.errmaiclnf
                       )

    # Random sample of areas harvested in year t
    v_area_harvested_total_ha <-
        rtriangle(n = 11, theta = lnf_area$area_harvested_total_ha,
                  lower = lnf_area$area_harvested_total_ha -
                      lnf_area$area_harvested_total_ha * param.erralnf,
                  upper = lnf_area$area_harvested_total_ha +
                      lnf_area$area_harvested_total_ha * param.erralnf)

    # Carbon accumulation (over the Reference Period) on areas harvested in year t
    # C accumulation = years of growth * area harvested * mean annual carbon increment
    natfi$agcrlt <- deltaT * v_area_harvested_total_ha * maicli
    # Collect results
    fdaacg[i] <- mean(natfi$agcrlt)
}

# Average annual gross emissions from forest degradation
# Convert C loss to emissions
(fd_lg_aae <- avg_closs_lnf * param.etacc)
# Upper and lower 90%-confidence bounds of emissions from forest degradation (logging)
lcifdaae <- quantile(fdaacl * param.etacc, probs = param.qlci)
ucifdaae <- quantile(fdaacl * param.etacc, probs = param.quci)
# MC emission estimates (forest degradation; logging)
v_fd_lg_aae <- fdaacl * param.etacc

# Average annual gross removals from forest degradation
# Convert C gain to tCO_2e (removals)
fd_lg_aar <- avg_cgain_lnf * param.etacc
# Upper and lower 90%-confidence bounds of removals from forest degradation
lcifdaar <- quantile(fdaacg * param.etacc, probs = param.qlci)
ucifdaar <- quantile(fdaacg * param.etacc, probs = param.quci)
# MC removal estimates (forest degradation)
v_fd_lg_aar <- fdaacg * param.etacc

# Average annual net emissions from forest degradation (logging)
# Convert net C loss to emissions
fd_lg_aane <- (avg_closs_lnf - avg_cgain_lnf) * param.etacc
# Upper and lower 90%-confidence bounds
lcifdaane <- quantile((fdaacl - fdaacg) * param.etacc, probs = param.qlci)
ucifdaane <- quantile((fdaacl - fdaacg) * param.etacc, probs = param.quci)
# MC net emissions estimates
v_fd_lg_aane <- (fdaacl - fdaacg) * param.etacc

# Create result table for forest degradation
rs_fd_logging <- data.frame(
                    # Average annual gross emissions
                    aa_emissions_tco2e_yr = fd_lg_aae,
                    # Confidence bounds
                    lci_aa_emissions_tco2e_yr = lcifdaae,
                    uci_aa_emissions_tco2e_yr = ucifdaae,
                    # Average annual gross removals
                    aa_removals_tco2e_yr = fd_lg_aar * -1,
                    # Confidence bounds
                    lci_aa_removals_tco2e_yr = ucifdaar * -1,
                    uci_aa_removals_tco2e_yr = lcifdaar * -1,
                    # Average annual net emissions
                    aa_net_emissions_tco2e_yr = fd_lg_aane,
                    # Confidence bounds
                    lci_aa_net_emissions_tco2e_yr = lcifdaane,
                    uci_aa_net_emissions_tco2e_yr = ucifdaane
                    )

# Forest degradation annual data
# Summary table for forest degradation (values per year)
fd <- data.frame(lnf[,1:2],
                 lnf_area[,"area_harvested_total_ha"],
                 lnf[,3] * - 1,
                 lnf[,3] * param.etacc,
                 lnf[,4],
                 lnf[,4] * param.etacc * -1)
# Rename columns
names(fd) <- c("year",
               "volume_logged_m3",
               "area_logged_ha",
               "carbon_loss_t",
               "emissions_tco2e",
               "carbon_gain_t",
               "removals_tco2e")

# Copy result tables (annual data)
rs_fd_table <- fd
fdtab <- fd
# Show annual data
if (debug_frl) fd[,-c(4,6)]

# Copy result table (annual average)
rsfd <- rs_fd_logging

# Create nice result table (annual average)
rsfdtab <- data.frame(sourcesink = c("FD gross emissions",
                                     "FD gross removals",
                                     "FD net emissions"))
rsfdtab$em <- unlist(c(rsfd[1,c(1,4,7)]))
rsfdtab$lci <- unlist(c(rsfd[1,c(2,5,8)]))
rsfdtab$uci <- unlist(c(rsfd[1,c(3,6,9)]))

# Results (net emissions from logging in Natural Forest; source 'forest degradation'
(rs_fd_lg <- rsfdtab)

# Structure of 'sw_barea'
if (debug_frl) str(sw_barea)


# Aggregate compartment data for the years 2015 to 2018 ................................
## Total area burnt in year t
sw_barea_agg <- aggregate(area_ha ~ year, sw_barea, sum)
# Average age of burnt compartments
sw_barea_agg$avg_age_yrs <- aggregate(age_yrs ~ year, sw_barea, mean)[,2]
# Number of compartments burnt in year t
sw_barea_agg$count <- aggregate(age_yrs ~ year, sw_barea, length)[,2]
# Rearrange columns
sw_barea_agg <- sw_barea_agg[,c(1,4,2,3)]
# Show
if (debug_frl) sw_barea_agg

# Mean annual (total) biomass increment in Softwood Plantations (Waterloo, 1994)
(maibsw <- param.maibp)

# Above- and below-ground biomass in compartments
# 0.2 = Rdll Root-to-shoot ratio tropical moist deciduous forest < 125 tB ha-1
sw_barea$agb <- sw_barea$age_yrs * (maibsw / (1 + 0.2))  # AGB
sw_barea$bgb <- sw_barea$age_yrs * (maibsw * 0.2)        # BGB

# Table of greenhouse gases
names(bioburn_ghgs)[1] <- "GHG"

# Table of greenhouse gases
if (debug_frl) bioburn_ghgs

# Emissions (in tCO2e) for each gas (and each compartment)
# CO_2 (above-ground biomass)
sw_barea$co2agb <- sw_barea$area_ha * sw_barea$agb * bioburn_ghgs[1,2] *
    bioburn_ghgs[1,3] * bioburn_ghgs[1,4] * 0.001
# CO_2 (below-ground biomass)
sw_barea$co2bgb <- sw_barea$area_ha * sw_barea$bgb * param.etacf *
    param.etacc * bioburn_ghgs[1,2]
# CH_4 (above-ground biomass)
sw_barea$ch4 <- sw_barea$area_ha * sw_barea$agb * bioburn_ghgs[2,2] *
    bioburn_ghgs[2,3] * bioburn_ghgs[2,4] * 0.001
# N_2O (above-ground biomass)
sw_barea$n2o <- sw_barea$area_ha * sw_barea$agb * bioburn_ghgs[3,2] *
    bioburn_ghgs[3,3] * bioburn_ghgs[3,4] * 0.001

# Sum of emissions per year
swfiret <- aggregate(. ~ year, sw_barea[,c(1, 6:9)], sum)

# Compute totals of gases for each year
swfiret$total <- rowSums(swfiret[,-1])
if (debug_frl) swfiret

# Average annual emissions [tCO2e yr^-1] from biomass burning in Softwood Plantations .
fdfswaae <- mean(swfiret$total)

# Uncertainty analysis
# Create vectors that collect the results of the MC simulation
swfireMC <- vector()
swfireMCnobgb <- vector()

# Random inputs for the MC simulation
mcf <- data.frame(
                  # Root to shoot ratio (IPCC [2006] default values)
                  r2s = rtriangle(param.runs, .2, .09, .25),
                  # Mean annual total biomass increment in Softwood Plantations
                  # (estimate from Waterloo [1994]). 25% error.
                  maibsw = rtriangle(param.runs,
                                     maibsw,
                                     maibsw - maibsw * 0.25,
                                     maibsw + maibsw * 0.25),
                  # Combustion factor in Softwood Plantations (IPCC, 2006).
                  cfsw = rtriangle(param.runs,
                                   .46, .46 - .46 * .5,
                                   .46 + .46 * .5),
                  # Emission factor CO_2 (IPCC default)
                  gefco2 = rnorm(param.runs, 1580, 90),
                  # Emission factor CH_4 (IPCC default)
                  gefch4 = rtriangle(param.runs,
                                     6.8,
                                     6.8 - 6.8 * 0.5,
                                     6.8 + 6.8 * 0.5),
                  # Emission factor N_2O (IPCC default)
                  gefn2o = rtriangle(param.runs,
                                     0.2,
                                     0.2 - 0.2 * 0.5,
                                     0.2 + 0.2 * 0.5),
                  # Global warming potential CO_2 (IPCC default)
                  gwpco2 = rep(bioburn_ghgs[1,4], param.runs),
                  # Global warming potential CH_4 (IPCC default)
                  gwpch4 = rtriangle(param.runs,
                                     28,
                                     28 - 28 * 0.5,
                                     28 + 28 * 0.5),
                  # Global warming potential N_2O (IPCC default)
                  gwpn2o = rtriangle(param.runs,
                                     265,
                                     265 - 265 * 0.5,
                                     265 + 265 * 0.5)
                  )

# MC simulation
# PROGRESS BAR
# pb <- txtProgressBar(min = 0, max = param.runs, style = 3) # Initiate progress bar

for(i in 1:param.runs){ # i <- 1
    # Create a copy of 'sw_barea'
    sw_bareai <- sw_barea

    # Compute AGB and BGB for each compartment .........................................
    sw_bareai$agb <- sw_bareai$age_yrs * (mcf$maibsw[i] / (1 + mcf$r2s[i]))
    sw_bareai$bgb <- sw_bareai$age_yrs * (mcf$maibsw[i] * mcf$r2s[i])

    # Compute emissions ................................................................
    # CO_2 (AGB)
    sw_bareai$co2agb <- sw_bareai$area_ha * sw_bareai$agb * mcf[i,"cfsw"] *
        mcf[i,"gefco2"] * mcf[i,"gwpco2"] * 0.001
    # CO_2 (BGB)
    sw_bareai$co2bgb <- sw_bareai$area_ha * sw_bareai$bgb * param.etacf *
        param.etacc * mcf[i,"cfsw"]
    # CH_4 (AGB)
    sw_bareai$ch4 <- sw_bareai$area_ha * sw_bareai$agb * mcf[i,"cfsw"] *
        mcf[i,"gefch4"] * mcf[i,"gwpch4"] * 0.001
    # N_2O (AGB)
    sw_bareai$n2o <- sw_bareai$area_ha * sw_bareai$agb * mcf[i,"cfsw"] *
        mcf[i,"gefn2o"] * mcf[i,"gwpn2o"] * 0.001

    # Aggregate results ................................................................
    swfireti <- aggregate(. ~ year, sw_bareai[,c(1, 6:9)], sum)
    swfireti$total <- rowSums(swfireti[,-1])

    # Annual average emissions .........................................................
    swfireMC[i] <- mean(swfireti$total)             # Including AGB and BGB

    # PROGRESS BAR: +1
#     setTxtProgressBar(pb, i)

}
# PROGRESS BAR: close
# close(pb)

# Get 90%-confidence bounds of emission estimates (including AGB and BGB)
lcifdfsweaae <- quantile(swfireMC, prob = c(param.qlci))  # Lower bound
ucifdfsweaae <- quantile(swfireMC, prob = c(param.quci))  # Upper bound

# Result table (AGB and BGB) ...........................................................
rs_fd_fire <- data.frame(aa_em_tco2e_yr = fdfswaae,
                            lci_aa_em_tco2e_yr = lcifdfsweaae,
                            uci_aa_em_tco2e_yr = ucifdfsweaae)

# Create a copy
rs_fd_bb <- rs_fd_fire
row.names(rs_fd_bb) <- "1"
# Show result table
if (debug_frl) rs_fd_bb

# Rename
fd_bb_aae <- fdfswaae
v_fd_bb_aae <- swfireMC

# Data on fuelwood consumption
# Number of households using fuelwood in rural and urban areas
fuelwhh <- data.frame(year = c(2007, 2017),
                      rural = c(60850,35210),
                      urban = c(12829,6718))
# Average fuelwood consumption per household in rural and urban areas
fuelwch <- data.frame(year = c(2007, 2017),
                      rural = c(0.927,0.927),
                      urban = c(0.378,0.378))

# Emissions from fuelwood consumption
aaefuelc <- mean(rowSums(fuelwhh[,-1] * fuelwch[,-1]) * param.etacf * param.etacc)
if (debug_frl) aaefuelc

# Uncertainty analysis
# Create vector
mcfuelc <- vector()

# PROGRESS BAR: initiate
# pb <- txtProgressBar(min = 0, max = param.runs, style = 3)

# Monte Carlo simulation
for(i in 1:param.runs){ # i <- 1
    # Random sample of the number of households using fuelwood
    fuelwhhi <- data.frame(year = c(2007, 2017),
                          rural = c(rnorm(1, fuelwhh[1,2], fuelwhh[1,2] * .1),
                                    rnorm(1, fuelwhh[2,2], fuelwhh[2,2] * .1)),
                          urban = c(rnorm(1, fuelwhh[1,3], fuelwhh[1,3] * .1),
                                    rnorm(1, fuelwhh[2,3], fuelwhh[2,3] * .1)))

    # Random sample of the average fuelwood consumption per household
    fuelwchi <- data.frame(year = c(2007, 2017),
                          rural = c(rnorm(1, fuelwch[1,2], fuelwch[1,2] * .25),
                                    rnorm(1, fuelwch[2,2], fuelwch[2,2] * .25)),
                          urban = c(rnorm(1, fuelwch[1,3], fuelwch[1,3] * .25),
                                    rnorm(1, fuelwch[2,3], fuelwch[2,3] * .25)))

    # Compute average annual emissions from fuelwood consumption
    mcfuelc[i] <- mean(rowSums(fuelwhhi[,-1] * fuelwchi[,-1]) *
                       param.etacf * param.etacc)

    # PROGRESS BAR: +1
#     setTxtProgressBar(pb, i)
}

# PROGRESS BAR: close
# close(pb)

# Compute 90%-confidence bounds
lcifuelc <- quantile(mcfuelc, prob = c(param.qlci))
ucifuelc <- quantile(mcfuelc, prob = c(param.quci))

# Result table 'emissions from fuelwood'
rs_fuelc <- data.frame(aa_em_tco2e_yr = aaefuelc,
                      lci_aa_em_tco2e_yr = lcifuelc,
                       uci_aa_em_tco2e_yr = ucifuelc)

# Create a copy
rs_fd_fu <- rs_fuelc
row.names(rs_fd_fu) <- "1"
# Show result table for fuelwood
if (debug_frl) rs_fd_fu

# Rename
fd_fu_aae <- aaefuelc
v_fd_fu_aae <- mcfuelc

# Result table for the net source 'forest degradation'
rs_fd <- data.frame(source = c("FD_Logging_net", "FD_Biomass_burning", "FD_total"),
              # Estimates (net emissions from logging in Natural Forest, emissions from
              # biomass burning in Softwood Plantations and total)
              est = c(
                      fd_lg_aane,               # Net emissions from logging (FD)
                      fd_bb_aae,                # Emissions from biomass burning (FD)
                      fd_lg_aane + fd_bb_aae    # Both
                      ),
              # Lower 90%-confidence limits
              lci = c(
                      quantile(v_fd_lg_aane,           # MC estimates net em. logging
                                probs = param.qlci),
                      quantile(v_fd_bb_aae,             # MC estimates fire
                               probs = param.qlci),
                      quantile(v_fd_lg_aane +           # MC estimates net em. logging
                               v_fd_bb_aae,             # and MC estimates fire
                               probs = param.qlci)
                      ),
              # Upper 90%-confidence limits
              uci = c(
                      quantile(v_fd_lg_aane,           # MC estimates net em. logging
                                probs = param.quci),
                      quantile(v_fd_bb_aae,             # MC estimates fire
                               probs = param.quci),
                      quantile(v_fd_lg_aane +           # MC estimates net em. logging
                               v_fd_bb_aae,             # and MC estimates fire
                               probs = param.quci)
                      )
                      )
# Show results: forest degaradtion
if (debug_frl) rs_fd
# AA results for afforestation/reforestation (AR); see Chapter on 'deforestation' (AD)
if (debug_frl) rs_AA_annual
# Extract data for AR
rs_AA_annual_ar <- rs_AA_annual[3:4, c(2, 4:6)]
# Create row for the total (sum of Low- and Upland Natural Forest)
rs_AA_annual_ar_total <- c(NA,
                           sum(rs_AA_annual_ar[,2]),
                           quantile(aa_boot[,5] + aa_boot[,6], probs = param.qlci) / 10,
                           quantile(aa_boot[,5] + aa_boot[,6], probs = param.quci) / 10
                           )
# Merge results
rs_AA_annual_ar <- rbind(rs_AA_annual_ar, rs_AA_annual_ar_total)
rs_AA_annual_ar[,1] <- as.character(rs_AA_annual_ar[,1])
rs_AA_annual_ar[3,1] <- "Total"
if (debug_frl) rs_AA_annual_ar # Print

# Mean annual carbon increment in Hardwood Plantations
(maicn <- param.maicar)

# Adding below-ground carbon
maic <- param.maicar * (1 + param.Rlwk)

# Uncertainty attached to the estimated total carbon increment for AR
armaic <- maic # Rename object
varmaic <- rtriangle(
           # Random mean annual carbon increment
                     n = param.runs,
                     theta = maicn,
                     lower = maicn - maicn * param.errmaicar,
                     upper = maicn + maicn * param.errmaicar
                    ) *
           # Uncertainty attached to root-to-shoot ratio (tropical rainforest)
           (1 + rtriangle(n = param.runs,
                          theta = param.Rlwk,
                          lower = param.Rlwk - param.Rlwk * param.errRlwk,
                          upper = param.Rlwk + param.Rlwk * param.errRlwk))

# Estimated average annual area of afforestation/reforestation
ARareas <- sum(aa_est_areas[5:6]) / 10

# Carbon gains on areas afforested/reforested in year t (over the Reference Period)
arcgainst <- seq(10.5, .5, -1) * ARareas * armaic

# Create a data frame of C gains over the Reference Period
arcgains <- data.frame(interval = as.character(2006:2016),
                       C_gain_t = arcgainst)

# Average annual C gains (AR) over the Reference Period
araacg <- sum(arcgainst) / 11
if (debug_frl) araacg

# Uncertainty analysis
# Create vector
varaacg <- vector()

# MC simulation
for(i in 1:param.runs){ # i <- 1
    varaacg[i] <- (sum(seq(10.5, 0.5, -1) *        # Time available for growth...
                       sum(aa_boot[i,5:6]) / 10 *  # Average annual area of AR
                       varmaic[i])                  # Random increment
                  ) / 11                            # Length of the FRL Reference Period

}

# Average annual removals from afforestation/reforestation (AR)
ec_ar_aar <- araacg * param.etacc                                # Estimate
lciaraar <- quantile(varaacg * param.etacc, probs = param.qlci) # Lower confidence limit
uciaraar <- quantile(varaacg * param.etacc, probs = param.quci) # Upper confidence limit
v_ec_ar_aar <- varaacg * param.etacc                             # MC estimates

# Result table AR (estimates are multiplied by -1, because removals always have
# a negative sign)
rs_ar <- data.frame(aa_removals_tco2e_yr = ec_ar_aar * -1,
                    lci_aa_removals_tco2e_yr = uciaraar * -1,
                    uci_aa_removals_tco2e_yr = lciaraar * -1
                    )

# Create a copy
rs_ec_ar <- rs_ar
row.names(rs_ec_ar) <- "1"
# Show result table
if (debug_frl) rs_ec_ar

# Volumes extracted from Hardwood Plantations
# These data were provided by Fiji Hardwood Corporation Limited (FHCL)
hw <- hwsw_volharv[, 1:2]               # Hardwood data
names(hw) <- c("year","vol_m3")         # Rename columns
if (debug_frl) hw                                      # Print 'hw'

# Biomass conversion and expansion factor (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.5 value
# for humid tropical natural forest >200 m^3)
bcefrm <- param.bcefrhw

# Root-to-shoot ratio (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.4 R for tropical rainforest);
# R for Wet Lowland (0.37)
rwl <- param.Rlwk

# Compute AGB for extracted volumes for the years 2006 to 2016
hw$agb_extracted_t <- hw$vol_m3 * bcefrm
# Compute BGB for extracted volumes for the years 2006 to 2016
hw$bgb_extracted_t <- hw$vol_m3 * bcefrm * rwl
# Compute TB for extracted volumes for the years 2006 to 2016
hw$biomass_extracted_t <- hw$agb_extracted_t + hw$bgb_extracted_t

# Convert to total carbon
hw$carbon_extracted_t <- hw$biomass_extracted_t * param.etacf
# Average annual carbon extraction
mcem <- mean(hw$carbon_extracted_t)

# Average annual CO2e emissions from Hardwood Plantations
mco2eem <- mcem * param.etacc

# Uncertainty assessment (Monte Carlo simulations)
# Vector to collect simulation runs
resmcem <- vector()

# Run simulation
for(i in 1:param.runs){ # i <- 1
    # Draw a random biomass conversion and expansion factor from a Triangular dist.
    bcefrmi <- rtriangle(1, theta = bcefrm,
                         lower = bcefrm - bcefrm * param.errbcefrhw,
                         upper = bcefrm + bcefrm * param.errbcefrhw)
    # Draw a random root-to-shoot ratio (tropical rainforest)
    rwli <- rtriangle(1, rwl, rwl - rwl * param.errRlwk, rwl + rwl * param.errRlwk)
    # Average annual C loss
    resmcem[i] <- mean(((hw$vol_m3 * bcefrmi) + (hw$vol_m3 * bcefrmi * rwli)) *
                       param.etacf)
}

# Average annual emissions from Hardwood Plantations
(ec_hw_aae <- mcem * param.etacc)                                     # Estimate
lci_ec_hw_aae <- quantile(resmcem * param.etacc, probs = param.qlci)  # Lower CI limit
uci_ec_hw_aae <- quantile(resmcem * param.etacc, probs = param.quci)  # Upper CI limit
v_ec_hw_aae <- resmcem * param.etacc                                  # MC estimates

# Mean annual increment (MAI) for volume [m^3]. Estimated from data in the last table in
# the document provided by Fiji Hardwood Corporation Limited (FHCL)
# Calculation: sum(hw_species[,2] * (hw_species[,5] / sum(hw_species[,5])))
maivm <- param.maivhww

# Biomass conversion and expansion factor (BCEF) for MAI volume (IPCC, 2006; Vol. 4,
# Chap. 4, Tab. 4.5, humid tropical rainforest, growing stock 21-40 m^3)
bcefim <- param.bcefihw

# Mean annual increment (tree biomass)
maibm <- maivm * bcefim + maivm * bcefim * rwl

# Mean annual increment C [t ha^-1 yr^-1]
maicm <- maibm * param.etacf

# Area planted between 2006 and 2010 (data from FHCL)
area_planted_0110_ha <- 3050.3 # in hectares

# Average area planted between 2006 and 2010
avg_planted_0610_ha <- area_planted_0110_ha / 10

# Area stocked in 2011
A_ha_2011 <- 56652

# Areas harvested and planted in Hardwood Plantations
hw$ahmt <- hw_ahp[,2] # Areas harvested
hw$apmt <- hw_ahp[,3] # Areas planted

# Area stocked December 31, 2005
A_ha_2005 <- A_ha_2011 + sum(hw$ahmt[1:5]) - sum(hw$apmt[1:5])

# Area that was neither harvested nor planted between 2006 and 2016. It just grows...
(atm <- A_ha_2005 - sum(hw$ahmt))

# Mean annual C removals on areas that just grow during the Reference Period
ctm <- atm * maicm

# Accumulation of C on planted areas over the Reference Period
hw$cpmt <- hw$apmt * deltaT * maicm
# Average annual C accumulation on planted areas over the Reference Period
mcpm <- mean(hw$cpmt)

# Reverse of 'deltaT'
rdeltaT <- rev(deltaT)
# C accumulation on areas that were harvested in year t
mchm <- mean(hw$ahmt * rdeltaT * maicm)

# Total average annual C removals
mcrm <- mcpm + ctm + mchm
# Total average annual CO_2 removals
mco2erm <- mcrm * param.etacc

# Uncertainty assessment (removals) ....................................................
resmcrm <- vector() # Vector that collects the results
resmcrmwm <- list()

# Run simulation .......................................................................
for(i in 1:param.runs){ # i <- 1
    hwi <- hw # Create a copy of 'hw'
    # Random realization of MAI volume
    maivmi <- rtriangle(1, theta = maivm,
                        lower = maivm - maivm * param.errmaivhw,
                        upper = maivm + maivm * param.errmaivhw
                        )
    # Random realization of BCEF_IM
    bcefimi <- rtriangle(1, theta = bcefim,
                         lower = bcefim - bcefim * param.errbcefihw,
                         upper = bcefim + bcefim * param.errbcefihw
                         )
    # Random root-to-shoot ratio for Wet Lowland
    rwli <- rtriangle(1, rwl, rwl - rwl * param.errRlwk, rwl + rwl * param.errRlwk)
    # Compute MAI for C
    maicmi <- (maivmi * bcefimi + maivmi * bcefimi * rwli) * param.etacf
    # Average annual C accumulation on areas that just grow
    atmi <- rtriangle(n = 1,
                      theta = atm,
                      lower = atm - atm * 0.5,
                      upper = atm + atm * 0.5)
    ctmi <- atmi * maicmi
    # Carbon accumulation in plantations that were harvested in year t
    cthmi <- mean(hw$ahmt * rdeltaT * maicmi)
    # C accumulation on planted areas over the Reference Period ........................
    ## Random draw for the area planted in year t (2006-2010)
    apmt2001_2010 <- runif(n = 10, min = 0, max = area_planted_0110_ha)
    ## Ensure that the sum of random draws does not exceed the total area planted
    ## between 2001 and 2010
    apmt2001_2010 <- apmt2001_2010 * area_planted_0110_ha / sum(apmt2001_2010)
    # Combine sampled areas planted (2006-2010) and reported areas planted (2011-2016)
    hwi$apmt <- c(apmt2001_2010[6:10], hw$apmt[6:11])
    # Compute C gain on areas planted
    hwi$cpmt <- hwi$apmt * deltaT * maicmi
    # Total average annual C accumulation
    resmcrm[i] <- mean(hwi$cpmt) +  # C gain areas planted
                  ctmi +            # C gain on areas that "just grow"
                  cthmi             # C gains from harvested compartments
}

# Average annual removals from Hardwood Plantations
ec_hw_aar <- mcrm * param.etacc                                       # Estimate
lci_ec_hw_aar <- quantile(resmcrm * param.etacc, probs = param.qlci)  # Lower CI limit
uci_ec_hw_aar <- quantile(resmcrm * param.etacc, probs = param.quci)  # Upper CI limit
v_ec_hw_aar <- resmcrm * param.etacc                                  # MC estimates

# Net emissions from Hardwood Plantations
ec_hw_aane <- ec_hw_aae - ec_hw_aar         # Point estimate
v_ec_hw_aane <- v_ec_hw_aae - v_ec_hw_aar   # MC estimates

# Volumes extracted from Softwood Plantations
sw <- hwsw_volharv[, c(1, 3)]           # Softwood data
names(sw) <- c("year","vol_m3")         # Rename columns
if (debug_frl) sw                                      # Print 'sw'

# Wood density of pine (softwoods); see Cown (1981). Pine plantations are mostly located
# below 300 m a.s.l.
wdPine <- param.wdsw

# Standard deviation wood density pine (Cown, 1981; Tab. 2)
sdwdPine <- param.sdwdsw

# The value of param.volTovol = .76 is the ratio between utilizable volume and total tree
# volume The value was taken from Waterloo (1994), Fig. 11.7, page 221.
volToAgbPine <- 1 / param.volTovol * wdPine

# Root-to-shoot ratio (IPCC, 2006; Vol. 4, Chap. 4, Tab. 4.4 R for tropical moist
# deciduous with AGB >125 t ha^-1); R for Dry Lowland
rdl <- .24

# Compute AGB for extracted volumes for the years 2006 to 2016
sw$agb_extracted_t <- sw$vol_m3 * volToAgbPine
# Compute BGB for extracted volumes for the years 2006 to 2016
sw$bgb_extracted_t <- sw$vol_m3 * volToAgbPine * rdl
# Compute TB for extracted volumes for the years 2006 to 2016
sw$biomass_extracted_t <- sw$agb_extracted_t + sw$bgb_extracted_t
# Convert to total carbon
sw$carbon_extracted_t <- sw$biomass_extracted_t * param.etacf
# Convert to carbon dioxide equivalents
sw$co2_emissions <- sw$carbon_extracted_t * param.etacc

# Average annual carbon extraction
mcep <- mean(sw$carbon_extracted_t)
# Average annual CO_2 emissions from Softwood Plantations
mco2eep <- mcep * param.etacc

# Uncertainty assessment (emissions from Softwood Plantations)
# List to collect simulation runs
rescept <- list()   # Needed later (MC simulation for removals)

# Run MC simulation ....................................................................
for(i in 1:param.runs){ # i <- 1
    # Draw a random volTovol (recovery proportion Softwood)
    volToAgbPinei <- 1 / rnorm(1, param.volTovol,
                               param.volTovol *
                               param.errvolTovol) *
                     rnorm(1, wdPine, sdwdPine)

    # Random root-to-shoot ratio (tropical moist deciduous forest; IPCC default)
    rdli <- rtriangle(n = 1, theta = .24, lower = 0.22, upper = 0.33)

    # Total carbon (AGC + BGC) loss for the years 2006 to 2016
    rescept[[i]] <- ((sw$vol * volToAgbPinei) +
                     (sw$vol * volToAgbPinei * rdli)) * param.etacf
}

# Average per year
respcem <- sapply(rescept, mean)

# Average annual emissions from Softwood Plantations
ec_sw_aae <- mcep * param.etacc                                        # Estimate
lci_ec_sw_aae <- quantile(respcem * param.etacc, probs = param.qlci)  # Lower CI limit
uci_ec_sw_aae <- quantile(respcem * param.etacc, probs = param.quci)  # Upper CI limit
v_ec_sw_aae <- respcem * param.etacc                                   # MC estimates

# Mean annual increment (total tree biomass, i.e., AGB + BGB).  Table 11.19 in
# Waterloo (1994)
(maibp <- param.maibp)
# Mean annual carbon increment in Softwood Plantations (C ha^-1 yr^-1)
maicp <- maibp * param.etacf

# Area stocked end of 2006 in FPL's lease area
A2006 <- 49503

# Cutting cycle length softwood (data provided by FPL)
cl <- param.cuttingc
# Estimate of the areas harvested [ha] (data provided by FPL could not be used; i.e.,
# for 2012 the area harvested was zero, the reported volume extracted 158214 m3).
sw$area_harvested_ha <- sw$carbon_extracted_t / (maicp * cl)

# Areas planted
sw$area_planted_ha <- sw_hvol_parea[,3]

# Area stocked in 2005
A2005 <- A2006 + sw$area_harvested_ha[1] - sw$area_planted_ha[1]
A2005p <- A2005

# Area that was neither planted nor harvested during the Reference Period; they just
# growth...
atp <- A2005 - sum(sw$area_harvested_ha)
# Mean annual C removals on areas that just grow during the Reference Period
ctp <- atp * maicp
# Accumulation of C on planted areas over the Reference Period
sw$cgain_t <- sw$area_planted_ha * deltaT * maicp
# C accumulation on areas that were harvested in year t
mchp <- mean(sw$area_harvested_ha * rdeltaT * maicp)
# Average annual C accumulation on planted areas over the Reference Period
mcpp <- mean(sw$cgain_t)

# Total average annual C removals
mcrp <- mcpp + ctp + mchp
# Total average annual CO2e removals
mco2erp <- mcrp * param.etacc

# Uncertainty analysis (removals in Softwood Plantations)
resmcrp <- vector() # Vector that collects the results
resmcrpwm <- list()

# Run simulation .......................................................................
for(i in 1:param.runs){ # i <- 1
    swi <- sw # Create a copy of 'sw'
    # Random realization of MAI AGB (25% error)
    maibpi <- rtriangle(1, theta = maibp,
                        lower = maibp - maibp * param.errmaibp,
                        upper = maibp + maibp * param.errmaibp
                        )
    # Convert MAI (volume) to MAI (carbon)
    maicpi <- maibpi * param.etacf
    # Random cutting cycle length
    cli <- rtriangle(n = 1, theta = cl,
                     lower = cl - param.errcuttingc,
                     upper = cl + param.errcuttingc)
    # Compute areas harvested
    swi$ahpt <- rescept[[i]] / (maicpi * cli)
    # Area stocked in 2005
    A2005i <- A2006 + swi$area_harvested_ha[1] - swi$area_planted_ha[1]
    # Areas that just grow during the Reference Period
    atpi <- A2005i - sum(swi$area_harvested)
    # Average annual C accumulation on areas that just grow
    ctpi <- atpi * maicpi
    # Carbon accumulation in plantation compartments that were harvested in year t
    cthpi <- mean(swi$area_harvested_ha * rdeltaT * maicpi)
    # C accumulation on planted areas over the Reference Period
    swi$cppt <- swi$area_planted_ha * deltaT * maicpi
    # Total average annual C accumulation
    resmcrp[i] <- mean(swi$cppt) + ctpi + cthpi
}

# Average annual removals from Softwood Plantations ....................................
ec_sw_aar <- mcrp * param.etacc                                # Estimate
lci_ec_sw_aar <- quantile(resmcrp * param.etacc, probs = param.qlci) # Lower CI limit
uci_ec_sw_aar <- quantile(resmcrp * param.etacc, probs = param.quci) # Upper CI limit
v_ec_sw_aar <- resmcrp * param.etacc                                 # MC estimate

# Net emissions from Hardwood Plantations
ec_sw_aane <- ec_sw_aae - ec_sw_aar
v_ec_sw_aane <- v_ec_sw_aae - v_ec_sw_aar

# Hardwood Plantations .................................................................
# Create a table for Hardwood Plantations
hw <- data.frame(hw[,1:6], co2eemt = hw[,6] * param.etacc, hw[,7:9])

# Rename columns in 'hw'
names(hw) <- c("year","volume_logged_m3","agb_loss_t","bgb_loss_t","tb_loss_t",
               "carbon_loss_t","emissions_tco2e","area_logged_ha","area_planted_ha",
               "carbon_gain_planted_t")
# Add a column with removals
hw$removals_planted_tco2e <- hw$carbon_gain_planted_t * param.etacc
# Make removals negative
hw[,c(3:6,11)] <- hw[,c(3:6,11)] * -1

# Stocking area in 2005 (Hardwood)
hwStocking <- A_ha_2005 - hw[,8] + hw[,9]

# Softwood Plantations .................................................................
# Rename columns in 'sw' (Softwood Plantations)
names(sw) <- c("year", "volume_logged_m3", "agb_loss_t", "bgb_loss_t", "tb_loss_t",
               "carbon_loss_t", "emissions_tco2e", "area_logged_ha", "area_planted_ha",
               "carbon_gain_planted_t")
# Add a column with removals
sw$removals_planted_tco2e <- sw$carbon_gain_planted_t * param.etacc
# Make removals negative
sw[,c(3:6,11)] <- sw[,c(3:6,11)] * -1

# Stocking area in 2005 (Softwood)
swStocking <- A2005p - sw[,8] + sw[,9]

# Confidence interval bounds
# Net emissions Softwood Plantations
lciv_ec_sw_aane <- quantile(v_ec_sw_aae -   # Emissions Softwood
                            v_ec_sw_aar,    # Removals Softwood
                            probs = param.qlci)
uciv_ec_sw_aane <- quantile(v_ec_sw_aae -
                            v_ec_sw_aar,
                            probs = param.quci)
# Net emissions Hardwood Plantations
lciv_ec_hw_aane <- quantile(v_ec_hw_aae -   # Emissions Hardwood
                            v_ec_hw_aar,    # Removals Hardwood
                            probs = param.qlci)
uciv_ec_hw_aane <- quantile(v_ec_hw_aae -
                            v_ec_hw_aar,
                            probs = param.quci)
# Gross emissions Forest Plantations (Hard- and Softwood)
lciv_ec_hwsw_aae <- quantile(v_ec_sw_aae +  # Emissions Softwood
                             v_ec_hw_aae,   # Emissions Hardwood
                             probs = param.qlci)
uciv_ec_hwsw_aae <- quantile(v_ec_sw_aae +
                             v_ec_hw_aae,
                             probs = param.quci)
# Gross removals Forest Plantations (Hard- and Softwood)
lciv_ec_hwsw_aar <- quantile(v_ec_sw_aar +  # Removals Softwood
                             v_ec_hw_aar,   # Removals Hardwood
                             probs = param.qlci)
uciv_ec_hwsw_aar <- quantile(v_ec_sw_aar +
                             v_ec_hw_aar,
                             probs = param.quci)
# Net emissions Forest Plantations (Hard- and Softwood)
lciv_ec_hwsw_aane <- quantile((v_ec_sw_aae + v_ec_hw_aae) - # Emissions Plantations
                              (v_ec_sw_aar + v_ec_hw_aar),  # Removals Plantations
                              probs = param.qlci)
uciv_ec_hwsw_aane <- quantile((v_ec_sw_aae + v_ec_hw_aae) -
                              (v_ec_sw_aar + v_ec_hw_aar),
                               probs = param.quci)

# Create a result table for Forest Plantations .........................................
rs_ec_plantations <- data.frame(plantations = c("Softwood","Hardwood","Total"),
     aa_em_tco2e_yr = c(ec_sw_aae,               # Emissions Softwood
                               ec_hw_aae,               # Emissions Hardwood
                               ec_sw_aae + ec_hw_aae),  # Emissions Plantations
     lci_aa_em_tco2e_yr = c(lci_ec_sw_aae, lci_ec_hw_aae, lciv_ec_hwsw_aae),
     uci_aa_em_tco2e_yr = c(uci_ec_sw_aae, uci_ec_hw_aae, uciv_ec_hwsw_aae),

     aa_removals_tco2e_yr = c(ec_sw_aar,                    # Removals Softwood
                              ec_hw_aar,                    # Removals Hardwood
                              ec_sw_aar + ec_hw_aar) * -1,  # Removals Plantations
     lci_rem_tco2e_yr = c(uci_ec_sw_aar, uci_ec_hw_aar, uciv_ec_hwsw_aar) * -1,
     uci_rem_tco2e_yr = c(lci_ec_sw_aar, lci_ec_hw_aar, lciv_ec_hwsw_aar) * -1,

     aa_net_em_tco2e_yr = c(
                                   # Net emissions Softwood
                                   ec_sw_aae - ec_sw_aar,
                                   # Net emissions Hardwood
                                   ec_hw_aae - ec_hw_aar,
                                   # Net emissions Forest Plantations
                                   (ec_sw_aae + ec_hw_aae) - (ec_sw_aar + ec_hw_aar)),
     lci_aa_net_em_tco2e_yr = c(lciv_ec_sw_aane, lciv_ec_hw_aane,
                                       lciv_ec_hwsw_aane),
     uci_aa_net_em_tco2e_yr = c(uciv_ec_sw_aane, uciv_ec_hw_aane,
                                       uciv_ec_hwsw_aane)
                     )

# Create copies of result table
rs_ec_plantations_all <- rs_ec_plantations          # Softwood, Hardwood, Total
rs_ec_plantations_nt <- rs_ec_plantations[-3,]      # Softwood, Hardwood
rs_ec_plantations <- rs_ec_plantations[3,-1]        # Total

# Average annual gross emissions from Forest Plantations
ec_hwsw_aae <- ec_hw_aae + ec_sw_aae
v_ec_hwsw_aae <- v_ec_hw_aae + v_ec_sw_aae

# Average annual gross removals from Forest Plantations
ec_hwsw_aar <- ec_hw_aar + ec_sw_aar
v_ec_hwsw_aar <- v_ec_hw_aar + v_ec_sw_aar

# Average annual net emissions from Forest Plantations
ec_hwsw_aane <- ec_hwsw_aae - ec_hwsw_aar
v_ec_hwsw_aane <- v_ec_hwsw_aae - v_ec_hwsw_aar

# Create result table for Hardwood Plantations (annual; data)
hwtab <- cbind(hw, hwStocking)
hwtab <- hwtab[,c(1,2,8:9,12,6,10)]

# Create result table for Softwood Plantations (annual; data)
swtab <- cbind(sw, swStocking)
swtab <- swtab[,c(1,2,8:9,12,6,10)]

# Result table Forest Plantations
# Create copies and select rows from result tables
rs_ec_plantations <- rs_ec_plantations_all

# Create the final result table for Forest Plantations
rs_ec_plantations_tab <- rs_ec_plantations[c(2,1,3),1:4]
rs_ec_plantations_re <- rs_ec_plantations[c(2,1,3),c(1,5:7)]
names(rs_ec_plantations_re) <- names(rs_ec_plantations_tab)
rs_ec_plantations_tab <- rbind(rs_ec_plantations_tab, rs_ec_plantations_re)
rs_ec_plantations_ne <- rs_ec_plantations[c(2,1,3),c(1,8:10)]
names(rs_ec_plantations_ne) <- names(rs_ec_plantations_tab)
rs_ec_plantations_tab <- rbind(rs_ec_plantations_tab, rs_ec_plantations_ne)

# Add a column with information on sub-sources and sub-sinks
ests <- c(
          " Gross em. Hardw.",      # Gross emissions Hardwood Plantations
          " Gross em. Softw.",      # Gross emissions Softwood Plantations
          " Gross em. Plant.",      # Gross emissions Forest Plantations
          " Gross rem. Hardw.",     # Gross removals Hardwood Plantations
          " Gross rem. Softw.",     # Gross removals Softwood Plantations
          " Gross rem. Plant.",     # Gross removals Forest Plantations
          " Net em. Hardw.",        # Net emissions Hardwood Plantations
          " Net em. Softw.",        # Net emissions Softwood Plantations
          " Net em. Plant."         # Net emissions Forest Plantations
          )

# Add column
rs_ec_plantations_tab[,1] <- ests

# Create a copy
rs_ec_pl <- rs_ec_plantations_tab
# Show result table for Forest Plantations
if (debug_frl) rs_ec_pl
# Fiji's Forest Reference Level (FRL) ##################################################
# FRL table (including all sources and sinks) ..........................................
frl_table <- data.frame(source_sink = c(
        # Sources and sinks:
                # DF = deforestation
                # FD = forest degradation
                # ECAR = enhancement of carbon stocks (afforestation/reforestation)
                # ECHS = enhancement of carbon stocks (Hard- and Softwood Plantations)
                        "DF",
                        "FDL",
                        "FDF",
                        "FUEL",
                        "ECAR",
                        "ECHS"),

        # Average annual gross emissions .......................................
        aa_emissions_tco2e_yr = c(rs_df[1,1],      # DF gross emission
                                  rs_fd_lg[1,2],   # FD logging gross emissions
                                  rs_fd_bb[1,1],   # FD fire gross emissions
                                  rs_fd_fu[1,1],   # FD fuelwood gross emissions
                                  0,               # EC AR gross emissions
                                  rs_ec_pl[3,2]),  # EC Plantations gross emissions

        # Lower confidence limit of average annual gross emissions
        lci_aa_emissions_tco2e_yr = c(rs_df[1,2],
                                      rs_fd_lg[1,3],
                                      rs_fd_bb[1,2],
                                      rs_fd_fu[1,2],
                                      0,
                                      rs_ec_pl[3,3]),

        # Upper confidence limit of average annual gross emissions
        uci_aa_emissions_tco2e_yr = c(rs_df[1,3],
                                      rs_fd_lg[1,4],
                                      rs_fd_bb[1,3],
                                      rs_fd_fu[1,3],
                                      0,
                                      rs_ec_pl[3,4]),

        # Average annual gross removals ........................................
        aa_removals_tco2e_yr = c(0,                 # DF gross removals
                                 rs_fd_lg[2,2],     # FD logging gross removals
                                 0,                 # FD fire gross removals
                                 0,                 # FD fuelwood gross removals
                                 rs_ec_ar[1,1],     # EC AR gross removals
                                 rs_ec_pl[6,2]),    # EC Plantations gross removals

        # Lower confidence limit of average annual gross removals
        lci_aa_removals_tco2e_yr = c(0,
                                     rs_fd_lg[2,3],
                                     0,
                                     0,
                                     rs_ec_ar[1,2],
                                     rs_ec_pl[6,3]),

        # Upper confidence limit of average annual gross removals
        uci_aa_removals_tco2e_yr = c(0,
                                     rs_fd_lg[2,4],
                                     0,
                                     0,
                                     rs_ec_ar[1,3],
                                     rs_ec_pl[6,4]),

        # Average annual net emissions .........................................
        aa_net_emissions_tco2e_yr=c(rs_df[1,1],     # DF net emissions
                                    rs_fd_lg[3,2],  # FD logging net emissions
                                    rs_fd_bb[1,1],  # FD fire net emissions
                                    rs_fd_fu[1,1],  # FD fuelwood net emissions
                                    rs_ec_ar[1,1],  # EC AR net emissions
                                    rs_ec_pl[9,2]), # EC Plantations net emissions

        # Lower confidence limit of average annual net emissions
        lci_aa_net_emissions_tco2e_yr=c(rs_df[1,2],
                                        rs_fd_lg[3,3],
                                        rs_fd_bb[1,2],
                                        rs_fd_fu[1,2],
                                        rs_ec_ar[1,2],
                                        rs_ec_pl[9,3]),

        # Upper confidence limit of average annual net emissions
        lci_aa_net_emissions_tco2e_yr=c(rs_df[1,3],
                                        rs_fd_lg[3,4],
                                        rs_fd_bb[1,3],
                                        rs_fd_fu[1,3],
                                        rs_ec_ar[1,3],
                                        rs_ec_pl[9,4])
                        )

# Average annual emissions and removals from EC ========================================
# EC = enhancement of forest carbon stocks
# Estimate of average annual net emissions
aaneec <- rs_ec_ar[1,1] +   # Gross/net removals AR
          rs_ec_pl[9,2]     # Net emissions Forest Plantations
# Lower confidence limit
lciaaneec <- quantile((v_ec_ar_aar * -1) +          # MC gross/net emissions AR
                      (v_ec_sw_aae + v_ec_hw_aae) - # MC gross emissions Plantations
                      (v_ec_sw_aar + v_ec_hw_aar),  # MC gross removals Plantations
                      probs = param.qlci)
# Upper confidence limit
uciaaneec <- quantile((v_ec_ar_aar * -1) +          # MC gross/net emissions AR
                      (v_ec_sw_aae + v_ec_hw_aae) - # MC gross emissions Plantations
                      (v_ec_sw_aar + v_ec_hw_aar),  # MC gross removals Plantations
                      probs = param.quci)

# Average annual emissions and removals from forest degradation ========================
# Gross emissions forest degradation (FD) ..............................................
aaefd <- rs_fd_lg[1,2] +    # Gross emissions FD logging
         rs_fd_bb[1,1] +    # Gross emissions FD biomass burning
         rs_fd_fu[1,1]      # Gross emissions FD fuelwood
# Lower confidence limit
lciaaefd <- quantile(v_fd_lg_aae +      # MC gross emissions logging
                     v_fd_bb_aae +      # MC gross emissions biomass burning
                     v_fd_fu_aae,       # MC gross emissions fuelwood
                     probs = param.qlci)
# Upper confidence limit
uciaaefd <- quantile(v_fd_lg_aae +      # Gross emissions logging
                     v_fd_bb_aae +      # Gross emissions biomass burning
                     v_fd_fu_aae,       # Gross emissions fuelwood
                     probs = param.quci)

# Gross removals forest degradation (FD) ...............................................
aarfd <- rs_fd_lg[2,2]                  # Gross removals FD logging
# Lower confidence limit
lciaarfd <- quantile(v_fd_lg_aar,       # MC gross removals logging
                     probs = param.qlci)
# Upper confidence limit
uciaarfd <- quantile(v_fd_lg_aar,       # MC gross removals logging
                     probs = param.quci)

# Net emissions forest degradation (FD) ................................................
# Gross emissions FD fuelwood was removed from this calculation

aanefd <- rs_fd_lg[1,2] +   # Gross emissions FD logging
          rs_fd_bb[1,1] +   # Gross emissions FD fire
          rs_fd_lg[2,2]     # Gross removals FD logging

aanefdf <- aanefd
# Lower confidence limit
lciaanefd <- quantile(v_fd_lg_aae +     # MC gross emissions FD logging
                      v_fd_bb_aae -     # MC gross emissions FD fire
                      v_fd_lg_aar,      # MC gross removals FD logging
                      probs = param.qlci)
lciaanefdf <- lciaanefd
# Upper confidence limit
uciaanefd <- quantile(v_fd_lg_aae +     # MC gross emissions FD logging
                      v_fd_bb_aae -     # MC gross emissions FD fire
                      v_fd_lg_aar,      # MC gross removals FD logging
                      probs = param.quci)
uciaanefdf <- uciaanefd

# Gross emissions and removals (all sources and sinks) =================================
# Total average annual gross emissions .................................................
aa_emissions_tco2e_yr <- sum(frl_table[,2])
# MC estimates
v_aa_emissions_tco2e_yr <- (v_df_L_aae + v_df_U_aae) +  # MC Emissions deforestation
                            v_fd_lg_aae +               # MC Emissions FD logging
                            v_fd_bb_aae +               # MC Emissions FD fire
                            v_fd_fu_aae +               # MC Emissions FD fuelwood
                            v_ec_hw_aae +               # MC Emissions EC Hardwood
                            v_ec_sw_aae                 # MC Emissions EC Softwood
# Lower confidence limit
lci_aa_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr, probs = param.qlci)
# Upper confidence limit
uci_aa_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr, probs = param.quci)

# Total average annual gross removals ..................................................
aa_removals_tco2e_yr <- sum(frl_table[,5])
# MC estimates
v_aa_removals_tco2e_yr <- (v_fd_lg_aar +        # MC gross removals FD logging
                           v_ec_ar_aar +        # MC gross removals AR
                           v_ec_hw_aar +        # MC gross removals Hardwood Plantations
                           v_ec_sw_aar) * -1    # MC gross removals Softwood Plantations
# Lower confidence limit
lci_aa_removals_tco2e_yr <- quantile(v_aa_removals_tco2e_yr, probs = param.qlci)
# Upper confidence limit
uci_aa_removals_tco2e_yr <- quantile(v_aa_removals_tco2e_yr, probs = param.quci)


# The FRL (including fuelwood) =========================================================
# Estimate
aa_net_emissions_tco2e_yr <- aa_emissions_tco2e_yr + aa_removals_tco2e_yr
# MC estimates
v_aa_net_emissions_tco2e_yr <- v_aa_emissions_tco2e_yr + v_aa_removals_tco2e_yr
# Lower confidence limit
lci_aa_net_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr +
                                          v_aa_removals_tco2e_yr,
                                          probs = param.qlci)
# Upper confidence limit
uci_aa_net_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr +
                                          v_aa_removals_tco2e_yr,
                                          probs = param.quci)

# Create FRL result table ..............................................................
frl <- data.frame(
                    aa_emissions_tco2e_yr = sum(frl_table[,2]),
                    lci_aa_emissions_tco2e_yr = lci_aa_emissions_tco2e_yr,
                    uci_aa_emissions_tco2e_yr = uci_aa_emissions_tco2e_yr,
                    aa_removals_tco2e_yr = sum(frl_table[,4]),
                    lci_aa_removals_tco2e_yr = lci_aa_removals_tco2e_yr,
                    uci_aa_removals_tco2e_yr = uci_aa_removals_tco2e_yr,
                    aa_net_emissions_tco2e_yr = aa_net_emissions_tco2e_yr,
                    lci_aa_net_emissions_tco2e_yr = lci_aa_net_emissions_tco2e_yr,
                    uci_aa_net_emissions_tco2e_yr = uci_aa_net_emissions_tco2e_yr
                 )

# FRL table including all sources and sinks ............................................
frl_table_data <- rbind(frl_table[,-1],
                   c(aa_emissions_tco2e_yr, lci_aa_emissions_tco2e_yr,
                     uci_aa_emissions_tco2e_yr, aa_removals_tco2e_yr,
                     uci_aa_removals_tco2e_yr, lci_aa_removals_tco2e_yr,
                     aa_net_emissions_tco2e_yr, lci_aa_net_emissions_tco2e_yr,
                     uci_aa_net_emissions_tco2e_yr))
frl_table <- data.frame(source_sink = c(as.character(frl_table[,1]), "FRL"),
                        frl_table_data)

frl_tablefuel <- frl_table

# Table of sources and sinks considered in the FRL .....................................
sourcesSinks <- data.frame(
 source_sink = c("DF","FDL","FDF","FUEL","ECAR","ECHS"),
 description = c(
            "Deforestation",
            "Forest degradation (logging)",
            "Forest degradation (fire)",
            "Fuelwood consumption",
            "Enhancement of forest carbon stocks (afforestation/reforestation)",
            "Enhancement of forest carbon stocks (Hard- and Softwood Plantations)"
            )
)

# Contributions of the different sources and sinks .....................................
# Contributions to gross emissions
contributione <- round(frl_table$aa_emissions_tco2e_yr[-7] /
                       frl_table$aa_emissions_tco2e_yr[7] * 100, 2)
names(contributione) <- c("DF","FDL","FDF","FUEL","ECAR","ECHS")
contributione <- data.frame(contributione)

# Contributions to gross removals
contributionr <- round(frl_table$aa_removals_tco2e_yr[-7] /
                       frl_table$aa_removals_tco2e_yr[7] * 100, 2)
names(contributionr) <- c("DF","FDL","FDF","FUEL","ECAR","ECHS")
contributionr <- data.frame(contributionr)

# Contributions to net emissions
contributionn <- round(abs(frl_table$aa_net_emissions_tco2e_yr)[-7] /
                       sum(abs(frl_table$aa_net_emissions_tco2e_yr)[-7]) * 100, 2)
names(contributionn) <- c("DF","FDL","FDF","FUEL","ECAR","ECHS")
contributionn <- data.frame(contributionn)

# Summary of contributions
contributions <- data.frame(sourceSink = c("DF","FDL","FDF","FUEL","ECAR","ECHS"),
                            gross_emissions = contributione[,1],
                            gross_removals = contributionr[,1],
                            net_emissions = contributionn[,1])

# Contributions (including emissions from fuelwood consumption)
contributionsfuel <- contributions
# Contributions of all sources and sinks in percent (including fuelwood)
if (debug_frl) contributionsfuel

# DF    = deforestation
# FDL   = forest degradation (logging)
# FDF   = forest degradation (fire)
# FUEL  = fuelwood
# ECAR  = enhancement of forest carbon stocks (afforestation/reforestation)
# ECHS  = enhancement of forest carbon stocks (Hard- and Softwood Plantations)

# Gross emissions and removals (all sources and sinks; excluding fuelwood) =============
# Total average annual gross emissions .................................................
aa_emissions_tco2e_yr <- sum(frl_table[-c(4,7),2])
# MC estimates
v_aa_emissions_tco2e_yr <- (v_df_L_aae + v_df_U_aae) +  # MC Emissions deforestation
                            v_fd_lg_aae +               # MC Emissions FD logging
                            v_fd_bb_aae +               # MC Emissions FD fire
                            v_ec_hw_aae +               # MC Emissions EC Hardwood
                            v_ec_sw_aae                 # MC Emissions EC Softwood
# Lower confidence limit
lci_aa_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr, probs = param.qlci)
# Upper confidence limit
uci_aa_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr, probs = param.quci)

# Total average annual gross removals
aa_removals_tco2e_yr <- sum(frl_table[-c(4,7),5])
# MC estimates
v_aa_removals_tco2e_yr <- (v_fd_lg_aar +        # MC gross removals FD logging
                           v_ec_ar_aar +        # MC gross removals AR
                           v_ec_hw_aar +        # MC gross removals Hardwood Plantations
                           v_ec_sw_aar) * -1    # MC gross removals Softwood Plantations
# Lower confidence limit
lci_aa_removals_tco2e_yr <- quantile(v_aa_removals_tco2e_yr, probs = param.qlci)
# Upper confidence limit
uci_aa_removals_tco2e_yr <- quantile(v_aa_removals_tco2e_yr, probs = param.quci)

# The FRL ==============================================================================
# Estimate
aa_net_emissions_tco2e_yr <- aa_emissions_tco2e_yr + aa_removals_tco2e_yr
# MC estimates
v_aa_net_emissions_tco2e_yr <- v_aa_emissions_tco2e_yr + v_aa_removals_tco2e_yr
# Lower confidence limit
lci_aa_net_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr +
                                          v_aa_removals_tco2e_yr,
                                          probs = param.qlci)
# Upper confidence limit
uci_aa_net_emissions_tco2e_yr <- quantile(v_aa_emissions_tco2e_yr +
                                          v_aa_removals_tco2e_yr,
                                          probs = param.quci)

# Create FRL result table ..............................................................
frl <- data.frame(
                    aa_emissions_tco2e_yr = sum(frl_table[,2]),
                    lci_aa_emissions_tco2e_yr = lci_aa_emissions_tco2e_yr,
                    uci_aa_emissions_tco2e_yr = uci_aa_emissions_tco2e_yr,
                    aa_removals_tco2e_yr = sum(frl_table[,4]),
                    lci_aa_removals_tco2e_yr = uci_aa_removals_tco2e_yr,
                    uci_aa_removals_tco2e_yr = lci_aa_removals_tco2e_yr,
                    aa_net_emissions_tco2e_yr = aa_net_emissions_tco2e_yr,
                    lci_aa_net_emissions_tco2e_yr = lci_aa_net_emissions_tco2e_yr,
                    uci_aa_net_emissions_tco2e_yr = uci_aa_net_emissions_tco2e_yr
                 )

# FRL table including all sources and sinks ............................................
frl_table_data <- rbind(frl_table[-c(4,7),-1],
                   c(aa_emissions_tco2e_yr, lci_aa_emissions_tco2e_yr,
                     uci_aa_emissions_tco2e_yr, aa_removals_tco2e_yr,
                     uci_aa_removals_tco2e_yr, lci_aa_removals_tco2e_yr,
                     aa_net_emissions_tco2e_yr, lci_aa_net_emissions_tco2e_yr,
                     uci_aa_net_emissions_tco2e_yr))
frl_table <- data.frame(source_sink = c(as.character(frl_table[-c(4,7),1]), "FRL"),
                        frl_table_data)

# Contributions of the different sources and sinks .....................................
# Contributions to gross emissions
contributione <- round(frl_table$aa_emissions_tco2e_yr[-6] /
                       frl_table$aa_emissions_tco2e_yr[6] * 100, 2)
names(contributione) <- c("DF","FDL","FDF","ECAR","ECHS")
contributione <- data.frame(contributione)

# Contributions to gross removals
contributionr <- round(frl_table$aa_removals_tco2e_yr[-6] /
                       frl_table$aa_removals_tco2e_yr[6] * 100, 2)
names(contributionr) <- c("DF","FDL","FDF","ECAR","ECHS")
contributionr <- data.frame(contributionr)

# Contributions to net emissions
contributionn <- round(abs(frl_table$aa_net_emissions_tco2e_yr)[-6] /
                       sum(abs(frl_table$aa_net_emissions_tco2e_yr)[-6]) * 100, 2)
names(contributionn) <- c("DF","FDL","FDF","ECAR","ECHS")
contributionn <- data.frame(contributionn)

# Summary table of contributions (excluding fuelwood)
contributions <- data.frame(sourceSink = c("DF","FDL","FDF","ECAR","ECHS"),
                            gross_emissions = contributione[,1],
                            gross_removals = contributionr[,1],
                            net_emissions = contributionn[,1])

# The contribution of all sources and sinks in percent (excluding fuelwood)
if (debug_frl) contributions

# DF    = deforestation
# FDL   = forest degradation (logging)
# FDF   = forest degradation (fire)
# ECAR  = enhancement of forest carbon stocks (afforestation/reforestation)
# ECHS  = enhancement of forest carbon stocks (Hard- and Softwood Plantations)

# FRL result table =====================================================================
frl <- frl_table

# The final result table of Fiji's Forest Reference Level 2006-2016 ....................
frltab <- data.frame(
       source_sink = c(
                       # Gross emissions ...............................................
                       "aaeDF",        # Gross emissions deforestation
                       "aaeFD_L",      # Gross emissions FD logging
                       "aaeFD_BSW",    # Gross emissions FD biom. burning Softwood
                       "aaeEC_HS",     # Gross emissions EC Hard- & Softwood Plantations
                       "aae_Combined", # Gross emissions (all sources)

                       # Gross removals ................................................
                       "aarFD_L",       # Gross removals FD logging
                       "aarEC_AR",      # Gross removals EC AR
                       "aarEC_HS",      # Gross removals EC hard- & Softwood Plantations
                       "aar_Combined",  # Gross removals (all sinks)

                       # Net emissions .................................................
                       "aaneDF",        # Net emissions deforestation
                       "aaneFD",        # Net emissions forest degradation
                       "aaneEC",        # Net emissions EC

                       # The FRL
                       "FRL"            # Fiji's Forest reference Level
                       ),
       estimate_tco2e_yr = c(
                    frl[1,2],                   # aaeDF
                    frl[2,2],                   # aaeFD_L
                    frl[3,2],                   # aaeFD_BSW
                    frl[5,2],                   # aaeEC_HS
                    sum(frl[c(1,2,3,5),2]),     # aae_Combined

                    frl[2,5],                   # aarFD_L
                    frl[4,5],                   # aarEC_AR
                    frl[5,5],                   # aarEC_HS
                    sum(frl[c(2,4,5),5]),       # aar_Combined

                    frl[1,8],                   # aaneDF
                    sum(frl[2:3,8]),            # aaneFD
                    sum(frl[c(4:5),8]),         # aaneEC

                    frl[6,8]                    # FRL
                    ),
       lci_tco2e_yr = c(
               frl[1,3],    # aaeDF
               frl[2,3],    # aaeFD_L
               frl[3,3],    # aaeFD_BSW
               frl[5,3],    # aaeEC_HS
               frl[6,3],    # aae_Combined

               frl[2,6],    # aarFD_L
               frl[4,6],    # aarEC_AR
               frl[5,6],    # aarEC_HS
               frl[6,6],    # aar_Combined

               frl[1,9],    # aaneDF
               lciaanefd,   # aaneFD
               lciaaneec,   # aaneEC

               frl[6,9]     # FRL
               ),
       uci_tco2e_yr = c(
               frl[1,4],    # aaeDF
               frl[2,4],    # aaeFD_L
               frl[3,4],    # aaeFD_BSW
               frl[5,4],    # aaeEC_HS
               frl[6,4],    # aae_Combined

               frl[2,7],    # aarFD_L
               frl[4,7],    # aarEC_AR
               frl[5,7],    # aarEC_HS
               frl[6,7],    # aar_Combined

               frl[1,10],   # aaneDF
               uciaanefd,   # aaneFD
               uciaaneec,   # aaneEC

               frl[6,10]    # FRL
               )

           )


# The final FRL table ##################################################################
if (debug_frl | show_output) {
  frltab
  #**************************************************************************
  # put results in txt file
  sink("Fiji_FRL_Results.txt")
  print(frltab)
  sink()
}

# FD = forest degradation
# EC = enhancement of forest carbon stocks
# AR = afforestation/reforestation

# aaeDF        # Gross emissions deforestation
# aaeFD_L      # Gross emissions FD logging
# aaeFD_BSW    # Gross emissions FD biomass. burning Softwood
# aaeEC_HS     # Gross emissions EC Hard- & Softwood Plantations
# aae_Combined # Gross emissions (all sources)

#  Gross removals ................................................
# aarFD_L       # Gross removals FD logging
# aarEC_AR      # Gross removals EC AR
# aarEC_HS      # Gross removals EC hard- & Softwood Plantations
# aar_Combined  # Gross removals (all sinks)

#  Net emissions .................................................
# aaneDF        # Net emissions deforestation
# aaneFD        # Net emissions forest degradation
# aaneEC        # Net emissions EC

if (debug_frl) sessionInfo()
