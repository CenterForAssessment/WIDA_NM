######################################################################################
###                                                                                ###
###       WIDA New Mexico Learning Loss Analyses -- Create Baseline Matrices    ###
###                                                                                ###
######################################################################################

### Load necessary packages
require(SGP)

###   Load data from the 'official' 2020 SGP analyses
load("Data/WIDA_NM_Data_LONG.Rdata")

###   Create a smaller subset of the LONG data to work with.
WIDA_NM_Baseline_Data <- WIDA_NM_Data_LONG[YEAR <= "2020", c("ID", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "VALID_CASE"),]

### NULL out existing baseline matrices (that never get used anyway)
SGPstateData[["WIDA_NM"]][["Baseline_splineMatrix"]] <- NULL

###   Read in Baseline SGP Configuration Scripts and Combine
source("SGP_CONFIG/2020/BASELINE/Matrices/READING.R")

WIDA_NM_BASELINE_CONFIG <- READING_BASELINE.config


###   Create Baseline Matrices

WIDA_NM_SGP <- prepareSGP(WIDA_NM_Baseline_Data, create.additional.variables=FALSE)

WIDA_NM_Baseline_Matrices <- baselineSGP(
				WIDA_NM_SGP,
				sgp.baseline.config=WIDA_NM_BASELINE_CONFIG,
				return.matrices.only=TRUE,
				calculate.baseline.sgps=FALSE,
				goodness.of.fit.print=FALSE,
				parallel.config = list(
					BACKEND="PARALLEL", WORKERS=list(TAUS=4))
)

###   Save results
save(WIDA_NM_Baseline_Matrices, file="Data/WIDA_NM_Baseline_Matrices.Rdata")
