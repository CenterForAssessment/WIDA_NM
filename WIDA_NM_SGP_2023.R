##########################################################################################
###
### Script for calculating SGPs for 2023 WIDA/ACCESS New Mexico 
###
##########################################################################################

### Load SGP package
require(SGP)
require(data.table)

### Load Data
load("Data/WIDA_NM_Data_LONG.Rdata")

###   Add single-cohort baseline matrices to SGPstateData
SGPstateData <- SGPmatrices::addBaselineMatrices("WIDA_NM", "2023")

###   Read in Baseline SGP Configuration Scripts and Combine
source("SGP_CONFIG/2023/READING.R")

WIDA_NM_CONFIG <- READING_2023.config

### Run analyses
WIDA_NM_SGP <- abcSGP(
		WIDA_NM_Data_LONG,
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "outputSGP"),
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=TRUE,
		sgp.percentiles.baseline=TRUE,
		sgp.projections.baseline=TRUE,
		sgp.projections.lagged.baseline=TRUE,
		get.cohort.data.info=TRUE,
		sgp.target.scale.scores=TRUE,
		save.intermediate.results=FALSE,
		sgp.config=WIDA_NM_CONFIG,
		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=4, BASELINE_PERCENTILES=4, PROJECTIONS=4, LAGGED_PROJECTIONS=4, SGP_SCALE_SCORE_TARGETS=4, GA_PLOTS=1, SG_PLOTS=1)))

### Output & Save results
save(WIDA_NM_SGP, file="Data/WIDA_NM_SGP.Rdata")
