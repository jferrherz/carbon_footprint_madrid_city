#######################################################
## Master script for the computation of the GMRIO of ##
## the city of Madrid (consolidated)                 ##
##                                                   ##
##                                                   ##
## Author: Jacobo Ferrer                             ##
## Date creation: 04/09/2023                         ##
#######################################################

  #### [ General options ]
  set.seed(123535)
  years         <- seq(2013,2019) # This need to be checked every time there is an update to the database
  year_str      <- years[1]
  year_str_io   <- 2010
  year_end      <- years[length(years)]
  year_end_io   <- 2022
  year_str_city <- 2010
  year_end_city <- 2021
  year_ghg_appl <- 2021
  year_sda      <- year_end
  cou_footprint <- "ES30"
  emissions_mod <- "GHG"

setwd(gsub("/rcodes","",
dirname(rstudioapi::getActiveDocumentContext()$path)))
#################### 
# [A] REFERENCE SETTINGS
########################################################
source(file.path(getwd(),"rcodes/project_options.R"))
source(file.path(getwd(),"rcodes/project_install_path.R"))
source(file.path(getwd(),"rcodes/project_functions.R"))
source(file.path(getwd(),"rcodes/project_ref_files.R"))

#################### 
# [B] BASIC INPUTS FOR ESTIMATIONS
########################################################
source(file.path(getwd(),"rcodes/region_suts_to_iots.R"))
source(file.path(getwd(),"rcodes/city_suts_to_iots.R"))
source(file.path(getwd(),"rcodes/figaro_suts_to_niots.R"))
source(file.path(getwd(),"rcodes/city_mrio_series.R"))
source(file.path(getwd(),"rcodes/city_update_series.R"))
source(file.path(getwd(),"rcodes/city_hh_budgets_survey.R"))
source(file.path(getwd(),"rcodes/city_emissions_inventory.R"))

#################### 
# [B] Madrid Regional carbon footprint
########################################################
source(file.path(getwd(),"rcodes/city_gdp_footprint.R"))
source(file.path(getwd(),"rcodes/city_consumption_footprint.R"))

#################### 
# [C] SDA & Policy simulation
########################################################
source(file.path(getwd(),"rcodes/city_structural_decomposition.R"))
source(file.path(getwd(),"rcodes/city_mitigation_gvc_simulation.R"))
source(file.path(getwd(),"rcodes/city_mitigation_simulation.R"))

#################### 
# [D] Detailed results
########################################################
source(file.path(getwd(),"rcodes/city_footprint_results_activity.R"))
source(file.path(getwd(),"rcodes/city_footprint_results_consumption.R"))

##############################################################################
## End of script
##############################################################################