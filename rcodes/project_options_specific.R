##################################################################################################################################
## OPTIONS: SPECIFIC
##################################################################################################################################

  #### [ run code that compares intermediate datasets
  #### with  national accounts]
  compare_datsets   <- "FALSE"
  #### [ create save copy of intermediate datasets ]
  #### ==> set to FALSE by default
  input_datasets    <- TRUE
  #### [ source of basic price vector for matching]
  bp_of_source      <- "FIGARO"
  #### [ recompute the pruchasers to basics datase ]
  pp_to_bp_recomp   <- FALSE
  #### [ select which table is used as base for
  #### the computation of the bridge matrix ]
  #### ==> other option: "classification"
  bridge.matrix     <- "reference"
  #### [ select method to run statistical matching ]
  #### ==> other option: "difference", "ranking"
  method_hotdeck    <- "random"
  #### [ selects sampling method for statistical matching
  #### between with replacement or without replacement of
  #### households from donor dataset]
  #### ==> other options: "with_replace", "without_replace"
  method_replace    <- "with_replace"
  #### [ select which quantile breakdown is to be
  #### used to allocate the gap between micro and 
  #### macro data ]
  income_breakdown  <- "decile"
  #### [ select the quantile distribution bottom to top]
  if(income_breakdown == "decile"){
  #bottom_to_top <- c(0,0,0.04,0.06,0.08,0.12,0.14,0.16,0.18,0.22)}
  bottom_to_top <- c(0,0,0.02,0.04,0.06,0.08,0.12,0.16,0.20,0.32)}
  if(income_breakdown == "quintile"){
  bottom_to_top <- c(0,0.1,0.2,0.3,0.4)}
  #### [ select the quantile distribution top to bottom]
  if(income_breakdown == "decile"){
  #top_to_bottom <- rev(c(0,0,0.04,0.06,0.08,0.12,0.14,0.16,0.18,0.22))}
  top_to_bottom <- rev(c(0,0,0.02,0.04,0.06,0.08,0.12,0.16,0.20,0.32))}
  if(income_breakdown == "quintile"){top_to_bottom <- rev(c(0,0.1,0.2,0.3,0.4))}
  #### [ select the quantile distribution for consumption]
  if(income_breakdown == "decile"){
  hfce_bottom_to_top <- c(0,0,0.04,0.06,0.08,0.12,0.14,0.16,0.18,0.22)}
  #bottom_to_top <- c(0,0,0.02,0.04,0.06,0.08,0.12,0.16,0.20,0.32)}
  if(income_breakdown == "quintile"){hfce_bottom_to_top <- c(0,0.1,0.2,0.3,0.4)}
  #### [ select items items with very low coverage ratios
  #### to implement specific adjustments ]
  na_item_spread  <- c()
  na_item_correct <- c("D7","D7P","D59")
  #### [ select consumption vectors to be used in grossing
  #### up micro observations to national accounts data ]
  HFCE              <- "GASTOMON"
  HFCER             <- "GASTORMON"
  #### [ whether to apply the RESIDENCE principle to consumption]
  #### NOTE: excluded by default due to unreasonable saving totals
  #### [ select which COICOP heading to be excuded, if at all]
  exc_coicop        <-  c()
  residence_exp <- FALSE
  #### [ exclude IMPUTED RENTS from income and consumption]
  exc_imputrent <- FALSE
  if(exc_imputrent == TRUE){
  exc_coicop    <- sort(unique(c(exc_coicop,"042")))}
  #### [ select the intra-quantile distribution mode]
  intra_quantile_dist <- "uniform" #"uniform" #"proport"
  #### [ method of gap allocation ]
  #### NOTE: change of quantile distribution not implemented
  if(intra_quantile_dist == "proport"){grossup_ratio <- "LMM_ratio_group"}
  if(intra_quantile_dist == "uniform"){grossup_ratio <- "LMM_quant_group"}
  #### [ to match National Accounts data which total
  #### is to be used: vintage discrepancies between
  #### FIGARO and supply and use data from Eurostat ]
  macro_target      <- "FIGARO"
  #### [ select which macromagnitudes are treated as
  #### endogenous when creating the V matrix ]
  endo_income       <- "D1"
  #### [ perform checks to income matching, variant
  #### of compare datasets ]
  check_income      <- TRUE

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################