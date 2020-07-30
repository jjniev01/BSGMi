bsgm.countries.fln.Rdata <- paste0("covariates_",bsgm.countries.tag, ".Rdata")
bsgm.countries.fln.json <- paste0("covariates_",bsgm.countries.tag, ".json")
bsgm.countries.LAN.Rdata <- paste0("covariates_",bsgm.countries.tag, ".Rdata")
bsgm.countries.LAN.json <- paste0("covariates_",bsgm.countries.tag, ".json")
bsgm.census.data.fln.Rdata <- paste0("population_data_",bsgm.countries.tag, ".Rdata")
bsgm.census.data.fln.csv<- paste0("population_data_",bsgm.countries.tag, ".csv")
bsgm.covariates.RF.Rdata <- paste0("covariates_RF_",bsgm.countries.tag, ".Rdata")
bsgm.covariates.RF.json<- paste0("covariates_RF_",bsgm.countries.tag, ".json")

bsgm.init.bsgmfit.RData <- paste0("init_bsgmfit_",bsgm.countries.tag, ".Rdata")
bsgm.bsgmfit.RData <- paste0("bsgmfit_",bsgm.countries.tag, ".Rdata")

bsgm.bsgmfit.final.RData <- paste0("bsgmfit_final_",bsgm.countries.tag, ".Rdata")
bsgm.bsgmfit.quant.RData <- paste0("bsgmfit_quant_",bsgm.countries.tag, ".Rdata")


bsgm.predict.density.rf.pred <- paste0("predict_transition_base_rf_pred_",
                                      bsgm.countries.tag, ".tif")
bsgm.predict.density.rf.sd <- paste0("predict_transition_base_rf_sd_",
                                    bsgm.countries.tag, ".tif")

#if (quant_output) {
  bsgm.predict.density.rf.pred_05 <- paste0("predict_transition_base_rf_pred_05",
                                           bsgm.countries.tag, ".tif")
  bsgm.predict.density.rf.pred_50 <- paste0("predict_transition_base_50",
                                           bsgm.countries.tag, ".tif")
  bsgm.predict.density.rf.pred_95 <- paste0("predict_transition_base_90",
                                           bsgm.countries.tag, ".tif")
#}  

bsgm.predict.probability.rf.pred.final <- paste0("transition_prob_base_",
                                                bsgm.countries.tag, ".tif")

bsgm.rst.bsgm.census.tif <- paste0("pop_census_mask_",bsgm.countries.tag, ".tif")