##################################################################################################################################
## [B] BASIC INPUTS FOR ESTIMATIONS
##################################################################################################################################

#################### 
# [A] Emissions vector for FIGARO tables
#########################################################
  if(length(files_path_figaro[files_path_figaro==paste0("intensity_factors_",
  stringr::str_to_lower(emissions_mod),".rds")])==0){
  #### [1] TOTAL Air emissions accounts by NACE Rev. 2 activity [FIGARO]
  #### ==> This dataset reports the emissions of greenhouse gases and air pollutants broken down by 64 industries 
  #### (classified by NACE Rev. 2) plus households. Concepts and principles are the same as in national accounts.
  #### Greenhouse gases and air pollutants emitted by the entire national economy AND those coming from the
  #### rest of the world are covered.
  if(length(files_path_figaro[files_path_figaro==paste0("enviro_figaro.rds")])==0){
  enviro_figaro <- data.frame();for(year_n in seq(year_str_io,year_ghg_appl)){
  enviro_figaro %<>% bind_rows(readr::read_csv(file.path(path_envir,"eurostat_environmental_statistics",
  "figaro_footprint",paste0(stringr::str_to_lower("GHG"),"Footprint_23ed_",year_n,".csv"))) %>% 
  rename("ktGHGF"="obs_value") %>% select(time_period:sto,ktGHGF) %>%  
  left_join(readr::read_csv(file.path(path_envir,"eurostat_environmental_statistics","figaro_footprint",
  paste0(stringr::str_to_lower("CO2"),"Footprint_23ed_",year_n,".csv"))) %>% rename("ktCO2F"="obs_value") %>%
  select(time_period:sto,ktCO2F),by=c("time_period","ref_area","industry","counterpart_area","sto")) %>%
  left_join(readr::read_csv(file.path(path_envir,"eurostat_environmental_statistics","figaro_footprint",
  paste0(stringr::str_to_lower("CH4"),"Footprint_",year_n,".csv"))) %>% rename("ktCH4F"="obs_value") %>%
  select(time_period:sto,ktCH4F),by=c("time_period","ref_area","industry","counterpart_area","sto")) %>%
  left_join(readr::read_csv(file.path(path_envir,"eurostat_environmental_statistics","figaro_footprint",
  paste0(stringr::str_to_lower("HFC"),"Footprint_",year_n,".csv"))) %>% rename("ktHFCF"="obs_value") %>%
  select(time_period:sto,ktHFCF),by=c("time_period","ref_area","industry","counterpart_area","sto")) %>% 
  left_join(readr::read_csv(file.path(path_envir,"eurostat_environmental_statistics","figaro_footprint",
  paste0(stringr::str_to_lower("N2O"),"Footprint_",year_n,".csv"))) %>% rename("ktN2OF"="obs_value") %>%
  select(time_period:sto,ktN2OF),by=c("time_period","ref_area","industry","counterpart_area","sto")) %>% 
  left_join(readr::read_csv(file.path(path_envir,"eurostat_environmental_statistics","figaro_footprint",
  paste0(stringr::str_to_lower("PFC"),"Footprint_",year_n,".csv"))) %>% rename("ktPFCF"="obs_value") %>%
  select(time_period:sto,ktPFCF),by=c("time_period","ref_area","industry","counterpart_area","sto")) %>% 
  left_join(readr::read_csv(file.path(path_envir,"eurostat_environmental_statistics","figaro_footprint",
  paste0(stringr::str_to_lower("NF3_SF6"),"Footprint_",year_n,".csv"))) %>% rename("ktNF3_SF6F"="obs_value") %>%
  select(time_period:sto,ktNF3_SF6F),by=c("time_period","ref_area","industry","counterpart_area","sto")))
  cat(paste0("\n FIGARO application: year ",year_n,"\n"))}
  saveRDS(enviro_figaro,file.path(getwd(),"datasets","enviro_figaro.rds"))} else {
  enviro_figaro <- readRDS(file.path(getwd(),"datasets","enviro_figaro.rds"))}
  
  #### [2] total country footprint equal direct CO2 (not GHG) emissions [== env_ac_co2fp]
  #### ==> Según Eurostat [env_ac_co2fp_linear] The CO2 emissions used as input to the model are actually implicit in the carbon 
  #### footprint dataset, since the emission totals remain the same and these are simply attributed to different categories of final demand. 
  #### To obtain the annual CO2 emissions used as input to the model, broken down by country and economic activity where emissions take place, 
  #### one just needs to filter the year [TIME], country of origin [C_ORIG] and economic activity [NACE_R2] and sum up all the remaining values
  #### [ATTENTION] everything matches when we take CO2 instead of GHG, but for FIGARO there are no GHG estimates for non-European countries
  
    #### [a] load emissions intensities (CO2,GHG), FIGARO final output, FIGARO footprints (CO2)
    intensity_factors_ghg <- enviro_figaro %>% select(time_period:sto,paste0("kt",c("GHG","CO2","CH4","HFC","N2O","PFC","NF3_SF6"),"F")) %>%
    left_join(readxl::read_xlsx(file.path(database_path,"Environmental","eurostat_environmental_statistics",
    "figaro_footprint","important_instruction_for_users.xlsx"),sheet="DictionaryINDUSTRY"),by=c("industry"="INDUSTRY")) %>% 
    filter(!industry=="HH"&time_period>2009) %>% group_by(time_period,ref_area,industry,LABEL_EN) %>% 
    summarize_at(c(paste0("kt",c("GHG","CO2","CH4","HFC","N2O","PFC","NF3_SF6"),"F")),~sum(.,na.rm=TRUE)) %>% ungroup %>% 
    rename("label"="LABEL_EN") %>% mutate_at(c("industry"),~stringr::str_replace_all(.,setNames(c(sort(as.vector(unlist(code_indus64 %>%
    filter(type=="sector") %>% distinct(nace_64))))),c(sort(unique(enviro_figaro %>% filter(!industry=="HH") %>% 
    distinct(industry) %>% unlist %>% as.vector)))))) %>% filter(time_period>2009) %>%
    left_join(readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics","env_ac_ainah_r2_linear.csv"),
    col_names = TRUE)%>% mutate_at(c("nace_r2"),~gsub("-","_",.)) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,
    setNames(c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% unlist %>% as.vector),c(code_indus64 %>% filter(type=="sector") %>% 
    select(eurostat_nace_r2) %>% unlist %>% as.vector)))) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,
    setNames(c("TOTAL","TOTAL_HH","HH_OTH"),c("TO84TAL","TO84TAL_HH","HH_O84TH")))) %>% select(-DATAFLOW,-`LAST UPDATE`,-OBS_FLAG) %>%
    filter(unit=="THS_T"&airpol%in%c("CO2","GHG")&freq=="A") %>% pivot_wider(names_from = "airpol",values_from = "OBS_VALUE") %>%
    rename("ktCO2E"="CO2","ktGHGE"="GHG") %>% mutate_at(c("geo"),~if_else(.=="EL","GR",.)),
    by=c("time_period"="TIME_PERIOD","ref_area"="geo","industry"="nace_r2")) 
    
    #### [b] join with the intensity ratios
    intensity_factors_ghg %<>% left_join(readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics",
    "env_ac_aeint_r2_linear.csv"),col_names = TRUE)%>% mutate_at(c("nace_r2"),~gsub("-","_",.)) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,
    setNames(c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% unlist %>% as.vector),c(code_indus64 %>% filter(type=="sector") %>% 
    select(eurostat_nace_r2) %>% unlist %>% as.vector)))) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,
    setNames(c("TOTAL","TOTAL_HH","HH_OTH"),c("TO84TAL","TO84TAL_HH","HH_O84TH")))) %>% select(-DATAFLOW,-`LAST UPDATE`,-OBS_FLAG) %>%
    filter(unit=="KG_EUR_CP"&airpol%in%c("CO2","GHG")&freq=="A"&na_item=="P1") %>% pivot_wider(names_from = "airpol",values_from = "OBS_VALUE") %>%
    select(-freq,-na_item,-unit) %>% rename("kg_euro_CO2"="CO2","kg_euro_GHG"="GHG") %>% mutate_at(c("geo"),~if_else(.=="EL","GR",.)),
    by=c("time_period"="TIME_PERIOD","ref_area"="geo","industry"="nace_r2"))
    
    #### [c] join the output vector from FIGARO WIOTs
    intensity_factors_ghg %<>% left_join(readRDS(file.path(path_figaro_data,paste0("figaro_ind-by-ind_mrio_",
    as.numeric(str_sub(year_end_io,3,4))+2,"ed.rds"))) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    filter(code%in%c(as.vector(unlist(code_indus64 %>% filter(type=="sector") %>% distinct(nace_64))))) %>% 
    mutate(P1=rowSums(across(AR_A01:ZA_P5M),na.rm=TRUE)) %>% mutate(P3AP6=rowSums(across(AR_P3_S13:ZA_P5M),na.rm=TRUE)) %>% 
    select(cou,code,year,P3AP6,P1) %>% mutate_at(c("year"),~as.double(.)),
    by=c("ref_area"="cou","industry"="code","time_period"="year"))
    
    #### [d] populate missing air emissions accounts values with CO2 emissions from FIGARO application
    intensity_factors_ghg %<>% mutate_at(c("ktCO2E","ktGHGE"),~if_else(is.na(.),NA,.)) %>% select(-freq,-unit) %>%
    rename("year"="time_period","cou"="ref_area","code"="industry") %>% unite(code,"cou","code",sep="_") %>%
      mutate_at(c("kg_euro_CO2"),~ktCO2F/P1) %>% mutate_at(c("kg_euro_GHG"),~ktGHGF/P1) %>% 
      mutate_at(c("kg_euro_CO2","kg_euro_GHG"),~if_else(is.na(.),0,.))
    
    if((intensity_factors_ghg %>% filter(grepl("ES_C10_C12",code)&year==2020) %>% select(ktCO2F,ktCO2E) %>%
    filter(round(ktCO2F,0)==round(ktCO2E,0)) %>% nrow)>0){
    cat("FIGARO and Eurostat's emissions intensity factors for ES and C10_C12 MATCH")
    } else {
    cat("FIGARO and Eurostat's emissions intensity factors for ES and C10_C12 DO NOT match")
    }
    
  # openxlsx::write.xlsx(intensity_dataset,file.path(getwd(),"datasets","emissions_factors.xlsx"))
  saveRDS(intensity_factors_ghg,file.path(getwd(),"datasets",paste0("intensity_factors_ghg.rds")))} else {
  intensity_factors_ghg <- readRDS(file.path(getwd(),"datasets",paste0("intensity_factors_ghg.rds")))
  enviro_figaro <- readRDS(file.path(getwd(),"datasets","enviro_figaro.rds"))
  }

intensity_factors_ghg %>% separate(code,c("cou","code"),sep="_",extra="merge") %>%
mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% group_by(year,cou,code) %>%
summarise_at(c("ktCO2F","ktCO2E","ktGHGE","kg_euro_GHG","kg_euro_CO2","P1","P3AP6"),~sum(.,na.rm=TRUE)) %>%
mutate_at(c("kg_euro_GHG"),~ktGHGE/P1) %>% mutate_at(c("kg_euro_CO2"),~ktCO2E/P1) %>% 
mutate_at(c("kg_euro_GHG","kg_euro_CO2"),~if_else(is.na(.)|is.infinite(.),0,.)) %>% 
filter(cou=="ES") %>% left_join(code_industries %>% mutate(label=name_short) %>% select(code,label),
by=c("code")) %>% bind_rows(intensity_factors_ghg %>% separate(code,c("cou","code"),
sep="_",extra="merge") %>% filter(cou=="ES") %>% filter(!code%in%c(LETTERS))) %>% arrange(year,code) %>%
mutate(na_item="P1",unit="kilograms per euro in current prices") %>% ungroup %>%
select(cou,year,code,label,unit,na_item,kg_euro_CO2,kg_euro_GHG,ktCO2E,ktGHGE,P1) %>%
set_names(c("country","year","nace_code","industry","unit","na_item",
"kg_CO2_euro","kg_GHG_euro","ktCO2","ktGHG","output")) %>%
mutate_if(is.numeric,~round(.,5)) %>%
openxlsx::write.xlsx(file.path(getwd(),"datasets","emissions_factors_complete.xlsx"),
overwrite=TRUE)

if(length(files_path_figaro[files_path_figaro=="city_mrio_env_accounts.rds"])==0){
#################### 
# [B] Emissions and Energy ACOUNTS - EUROSTAT
#########################################################
  #### Source: https://ec.europa.eu/eurostat/cache/metadata/en/env_ac_ainah_r2_sims.htm
  #### Different source statistics:
  #### [1] Air emissions accounts by NACE Rev. 2 activity [ENV_AC_AINAH_R2]
  #### ==> This dataset reports the emissions of greenhouse gases and air pollutants broken down by 64 industries 
  #### (classified by NACE Rev. 2) plus households. Concepts and principles are the same as in national accounts.
  #### Greenhouse gases and air pollutants emitted by the entire national economy are covered.
  env_accounts_ainah_r2 <- readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics","env_ac_ainah_r2_linear.csv"),col_names = TRUE)%>% 
  mutate_at(c("nace_r2"),~gsub("-","_",.)) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c(code_indus64 %>% 
  filter(type=="sector") %>% select(nace_64) %>% unlist %>% as.vector),c(code_indus64 %>% filter(type=="sector") %>% 
  select(eurostat_nace_r2) %>% unlist %>% as.vector)))) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,
  setNames(c("TOTAL","TOTAL_HH","HH_OTH"),c("TO84TAL","TO84TAL_HH","HH_O84TH")))) %>% select(-DATAFLOW,-`LAST UPDATE`) %>%
  filter(unit=="THS_T"&geo=="ES"&airpol=="GHG"&TIME_PERIOD>2009)
    # env_accounts_ainah_r2 %>% filter(geo=="ES"&nace_r2=="TOTAL"&unit=="THS_T"&airpol=="GHG"&TIME_PERIOD%in%c(seq(year_str_io,year_end_city))
    # env_accounts_ainah_r2 %>% filter(geo=="ES"&nace_r2=="TOTAL_HH"&unit=="THS_T"&airpol=="GHG"&TIME_PERIOD%in%c(seq(year_str_io,year_end_city))
    # env_accounts_ainah_r2 %>% filter(geo=="ES"&airpol=="GHG"&unit=="THS_T") %>% group_by(geo,TIME_PERIOD) %>% #filter(nace_r2%in%c("TOTAL")) %>%
    # filter(nace_r2%in%c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% unlist %>% as.vector)) %>%
    # summarize_at(c("OBS_VALUE"),~sum(.,na.rm=TRUE))
  # enviro_vector <- enviro_figaro %>% select(time_period,ref_area,industry,counterpart_area,
  # sto,obs_value) %>% filter(!industry=="HH"&ref_area=="ES") %>% group_by(time_period,ref_area) %>%
  # summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(env_accounts_ainah_r2 %>% 
  # filter(geo=="ES"&nace_r2=="TOTAL"&unit=="THS_T"&airpol=="GHG"&TIME_PERIOD%in%c(seq(year_str_io,year_end_city)) %>%
  # select(geo,TIME_PERIOD,OBS_VALUE) %>% set_names(c("ref_area","time_period","ENV_AC_AINAH_R2")),
  # by=c("ref_area","time_period")) %>% rename("FIGARO_FOOTPRINT" = "obs_value") %>%
  # mutate(ratio = FIGARO_FOOTPRINT/ENV_AC_AINAH_R2)
    
  #### [2] Greenhouse gas emissions by source sector (source: EEA) [ENV_AIR_GGE]
  env_accounts_air_gge_all <- readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics",
  "env_air_gge_linear.csv"),col_names = TRUE) %>% select(-DATAFLOW,-`LAST UPDATE`) %>% filter(unit=="THS_T"&TIME_PERIOD>2009) %>% 
  mutate_at(c("src_crf"),~stringr::str_sub(.,4,-1L)) %>% mutate_at(c("src_crf"),~stringr::str_to_sentence(.))
  env_accounts_air_gge <- env_accounts_air_gge_all %>% filter(unit=="THS_T"&geo=="ES"&airpol=="GHG"&TIME_PERIOD>2009)
  
  #### [3] Air emissions accounts totals bridging to emission inventory totals [ENV_AC_AIBRID_R2]
  #### ==> This data set includes so-called bridging items showing the differences between the national totals as derived from two 
  #### internationally established approaches/methods for reporting emissions of greenhouse gases and air pollutants:
  #### (1)  Air emissions accounts (AEA); (2) National emission inventories under the UNFCCC
  #### Greenhouse gases and air pollutants emitted by the entire national economy are covered.
  #### [ATTENTION] this is important to move from production (accounts) to territories (inventories)
  env_accounts_aibrid_r2 <- readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics",
  "env_ac_aibrid_r2_linear.csv"),col_names = TRUE) %>% select(-DATAFLOW,-`LAST UPDATE`) %>% 
  filter(unit=="THS_T"&geo=="ES"&airpol=="GHG"&TIME_PERIOD>2009)
  
  #### [4] Air emissions intensities by NACE Rev. 2 activity [ENV_AC_AEINT_R2]
  #### ==> This data set presents intensity-ratios relating AEA emissions (see previous) to economic parameters (value added, 
  #### production output) for 64 industries (classified by NACE Rev. 2).
  #### Greenhouse gases and air pollutants emitted by the entire national economy are covered.
  env_accounts_aeint_r2 <- readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics","env_ac_aeint_r2_linear.csv"),
  col_names = TRUE)%>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% 
  unlist %>% as.vector),c(code_indus64 %>% filter(type=="sector") %>% select(eurostat_nace_r2) %>% unlist %>% as.vector)))) %>%
  mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c("TOTAL","TOTAL_HH","HH_OTH"),c("TO84TAL","TO84TAL_HH","HH_O84TH")))) %>%
  select(-DATAFLOW,-`LAST UPDATE`) %>% filter(unit=="THS_T"&geo=="ES"&airpol=="GHG"&TIME_PERIOD>2009)
  
  #### [5] Energy supply and use by NACE Rev. 2 activity [env_ac_pefasu]
  #### ==> This data set presents the physical energy flow accounts (PEFA) (in terajoules) with flows from the environment to the economy (natural inputs),
  #### within the economy (products), and from the economy back to the environment (residuals) using the accounting framework of physical supply and use tables.
  #### PEFA provide information on energy flows arranged in a way fully compatible with concepts, principles, and classifications of national accounts
  env_ac_pefasu <- readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics","env_ac_pefasu_linear.csv"),col_names = TRUE) %>% 
  filter(geo=="ES"&unit=="TJ"&geo=="ES"&TIME_PERIOD>2009)
  env_ac_pefasu %<>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% 
  unlist %>% as.vector),c(as.vector(unlist(env_ac_pefasu %>% filter(!nace_r2%in%c("TOTAL","ENV","NRG_FLOW","CH_INV_PA","ROW_ACT","SD_SU","L68A")) %>% 
  distinct(nace_r2) %>% filter(!grepl("HH",nace_r2)) %>% filter(!nace_r2%in%c(LETTERS[!LETTERS%in%c("B","D","F","I","L","T","U","O","P")]))  %>% 
  distinct(nace_r2))))))) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c("TOTAL","TOTAL_HH","HH_OTH"),
  c("TO84TAL","TO84TAL_HH","HH_O84TH")))) %>% select(-DATAFLOW,-`LAST UPDATE`) %>% filter(nace_r2%in%c(c(code_indus64 %>% 
  filter(type=="sector") %>% select(nace_64) %>% unlist %>% as.vector),"HH_TRA","HH_HEAT","HH_OTH")) %>% 
  filter(prod_nrg%in%c(as.vector(unlist(pefasu_codes %>% filter(digits==2) %>% select(code))))) %>%
  left_join(pefasu_codes,by=c("prod_nrg"="code"))
  
  #### [6] Key indicators of physical energy flow accounts by NACE Rev. 2 activity (env_ac_pefa04)
  #### ==> This data set presents the physical energy flow accounts (PEFA) (in terajoules) with flows from the environment to the economy (natural inputs),
  #### within the economy (products), and from the economy back to the environment (residuals) using the accounting framework of physical supply and use tables.
  #### PEFA provide information on energy flows arranged in a way fully compatible with concepts, principles, and classifications of national accounts
  env_ac_pefa04 <- readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics","env_ac_pefa04_linear.csv"),col_names = TRUE) %>% 
  filter(geo=="ES"&unit=="TJ"&geo=="ES"&TIME_PERIOD>2009)
  # env_ac_pefa04 %<>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% 
  # unlist %>% as.vector),c(as.vector(unlist(env_ac_pefa04 %>% filter(!nace_r2%in%c("TOTAL","ENV","NRG_FLOW","CH_INV_PA","ROW_ACT","SD_SU","L68A")) %>% 
  # distinct(nace_r2) %>% filter(!grepl("HH",nace_r2)) %>% filter(!nace_r2%in%c(LETTERS[!LETTERS%in%c("B","D","F","I","L","T","U","O","P")]))  %>% 
  # distinct(nace_r2))))))) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c("TOTAL","TOTAL_HH","HH_OTH"),
  # c("TO84TAL","TO84TAL_HH","HH_O84TH")))) %>% select(-DATAFLOW,-`LAST UPDATE`) %>% filter(nace_r2%in%c(c(code_indus64 %>% 
  # filter(type=="sector") %>% select(nace_64) %>% unlist %>% as.vector),"HH_TRA","HH_HEAT","HH_OTH")) %>% 
  # filter(prod_nrg%in%c(as.vector(unlist(pefasu_codes %>% filter(digits==2) %>% select(code))))) %>%
  # left_join(pefasu_codes,by=c("prod_nrg"="code"))
  
  #### [7] Physical energy flow accounts totals bridging to energy balances totals (env_ac_pefa05)
  #### ==> This data set presents the physical energy flow accounts (PEFA) (in terajoules) with flows from the environment to the economy (natural inputs),
  #### within the economy (products), and from the economy back to the environment (residuals) using the accounting framework of physical supply and use tables.
  #### PEFA provide information on energy flows arranged in a way fully compatible with concepts, principles, and classifications of national accounts
  env_ac_pefa05 <- readr::read_csv(file.path(database_path,"Environmental","eurostat_environmental_statistics","env_ac_pefa05_linear.csv"),col_names = TRUE) %>% 
  filter(geo=="ES"&unit=="TJ"&geo=="ES"&TIME_PERIOD>2009)
  # env_ac_pefa05 %<>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% 
  # unlist %>% as.vector),c(as.vector(unlist(env_ac_pefa05 %>% filter(!nace_r2%in%c("TOTAL","ENV","NRG_FLOW","CH_INV_PA","ROW_ACT","SD_SU","L68A")) %>% 
  # distinct(nace_r2) %>% filter(!grepl("HH",nace_r2)) %>% filter(!nace_r2%in%c(LETTERS[!LETTERS%in%c("B","D","F","I","L","T","U","O","P")]))  %>% 
  # distinct(nace_r2))))))) %>% mutate_at(c("nace_r2"),~stringr::str_replace_all(.,setNames(c("TOTAL","TOTAL_HH","HH_OTH"),
  # c("TO84TAL","TO84TAL_HH","HH_O84TH")))) %>% select(-DATAFLOW,-`LAST UPDATE`) %>% filter(nace_r2%in%c(c(code_indus64 %>% 
  # filter(type=="sector") %>% select(nace_64) %>% unlist %>% as.vector),"HH_TRA","HH_HEAT","HH_OTH")) %>% 
  # filter(prod_nrg%in%c(as.vector(unlist(pefasu_codes %>% filter(digits==2) %>% select(code))))) %>%
  # left_join(pefasu_codes,by=c("prod_nrg"="code"))
  
#################### 
# [C] inventory Bridge Matrix IPCC/SNAP sectors to NACE activities
#########################################################
  if(length(files_path_figaro[files_path_figaro=="city_accounts_madrid.rds"])==0){
  #### [FIRST] Cleaning the AEA Annex classification matching table
  #### [a] initial cleaning of the Annex dataset
  snap_nfr_crf_nace_r2 <- readxl::read_xls(file.path(getwd(),"IPCC - NACE rev. 2",
  "Annex1-AEA-Manual-(SNAP-CRFNFR-NACE2)-v2 0.xls"),sheet = "Annex1 SNAP97 - CRFNFR - Nace2",
  col_names = TRUE,skip=7) %>% select(-11:-14) %>% set_names(c("code_snap","label_snap",
  "code_nfr_crf","label_nfr_cfr","code_nace_r2","flag_nace_r2","label_nace_r2","code_nace_r1",
  "flag_nace_r1","label_nace_r1")) %>% mutate_at(c("code_snap"),~gsub(" ","",.)) %>% 
  mutate_at(c("code_snap"),~stringr::str_pad(.,2,side="left","0")) %>%
  mutate(code_main = stringr::str_sub(code_snap,1,2)) %>%
  mutate(digits_snap = nchar(code_snap))
  
  #### [b] SNAP - NACE ==> linkage of codes and labels with the main SNAP sectors (01-11)
  snap_nace_r2 <- snap_nfr_crf_nace_r2 %>% left_join(snap_nfr_crf_nace_r2 %>% filter(digits_snap%in%c(1,2)) %>%
  rename("label_main"="label_snap") %>% distinct(code_main,label_main),by=c("code_main")) %>%
  mutate(code_ipcc = NA,label_ipcc=NA) %>% select(code_main,label_main,everything()) %>% 
  filter(!digits_snap%in%c(1,2)) %>% mutate_at(c("label_main"),~stringr::str_to_sentence(.)) %>%
  filter(code_main%in%c(paste0("0",1:9),"10","11"))
  
  #### [c] CFR - NACE ==> linkage of codes and labels with the main SNAP sectors (01-11) and 
  #### filter of the code list by the closest IPCC (NFR/CFR) code to match all the available ones
  nfr_crf_nace_r2 <- snap_nfr_crf_nace_r2 %>% left_join(snap_nfr_crf_nace_r2 %>% filter(digits_snap%in%c(1,2)) %>%
  rename("label_main"="label_snap") %>% distinct(code_main,label_main),by=c("code_main")) %>%
  mutate(code_ipcc = code_nfr_crf,label_ipcc=label_nfr_cfr) %>% select(code_main,label_main,
  code_ipcc,label_ipcc,everything()) %>% filter(!digits_snap%in%c(1,2)) %>% 
  mutate_at(c("label_main"),~stringr::str_to_sentence(.)) %>%
  filter(!grepl("Source",code_ipcc)) #&!is.na(code_ipcc)
  
  #### [d] function to clean NACE rev.2 codes
  clean_codes_ipcc <- function(data){
    data %<>% distinct(code_main,label_main,code_ipcc,label_ipcc,code_nace_r2) %>%
    mutate_at(c("code_nace_r2"),~gsub("2035","20,35",gsub("\\::",NA,gsub("\\?+::",NA,gsub("\\(","",gsub("\\)","",gsub(" ","",
    gsub(" ,",",",gsub("~","",gsub(", ",",",gsub(" : ",":",gsub("; ",",",gsub(" ;",",",gsub("-",":",
    gsub(" or ",",",gsub("\\;",",",gsub(" and/or ",",",.))))))))))))))))) %>% mutate_at(c("code_nace_r2"),
    ~gsub("H.heating","HH_HEAT",gsub("H.other","HH_OTH",gsub("H.transport","HH_TRA",gsub("H.others",
    "HH_OTH",gsub("H.transp\\+01:99","HH_TRA",.))))))
    
    data %<>% filter(!grepl(":",code_nace_r2)) %>% bind_rows(data %>% filter(grepl(":",code_nace_r2)) %>% 
    separate(code_nace_r2,c("code_nace_r2",paste0("extra",1:4)),extra = "merge",sep=",") %>% 
    pivot_longer(`code_nace_r2`:`extra4`,names_to = "code_nace_r2_extra") %>% filter(!is.na(value)) %>%
    separate(value,c("start","end"),sep=":",extra="merge") %>% mutate_at(c("end"),~if_else(is.na(.),start,.)) %>%
    mutate_at(c("start","end"),~as.numeric(.)) %>% mutate(cols = map2(start, end, `:`)) %>% unnest_longer(cols) %>%
    select(code_main,label_main,code_ipcc,label_ipcc,cols) %>% mutate_at(c("cols"),~as.character(.)) %>% 
    rename("code_nace_r2"="cols") %>% mutate_at(c("code_nace_r2"),~stringr::str_pad(.,2,side="left","0"))) %>% 
    separate(code_nace_r2,c("code_nace_r2",paste0("extra",1:3)),extra = "merge",sep=",") %>%
    pivot_longer(`code_nace_r2`:`extra3`,names_to = "code_nace_r2") %>% filter(!is.na(value))
    
    #### [ATTENTION] this is to expand the "H.transp\\+01:99" variable [totals match, validated: 05/02/24]
    data %<>% bind_rows(data %>% distinct(value) %>% filter(!grepl("HH",value)) %>% arrange(value) %>% mutate(code_main="07") %>%
    left_join(data %>% filter(code_main=="07") %>% distinct(code_main,.keep_all = TRUE) %>% select(-value) %>%
    mutate_at(c("label_ipcc"),~paste0("Road transport, all")) %>% mutate_at(c("code_ipcc"),~paste0("1.A.3.b")),
    by=c("code_main")) %>% select(names(data)))
  }
  
  #### [e] final cleaning of the codes
  snap_nace_r2 %<>% clean_codes_ipcc %>% select(code_main,label_main,value) %>% 
  rename("code_nace_r2"="value") %>% distinct(code_main,label_main,code_nace_r2) %>% 
  arrange(code_nace_r2)
  nfr_crf_nace_r2 %<>% clean_codes_ipcc %>% select(code_main,label_main,code_ipcc,label_ipcc,value) %>% 
  rename("code_nace_r2"="value") %>% distinct(code_main,label_main,code_ipcc,label_ipcc,code_nace_r2) %>% 
  arrange(code_nace_r2)
  
  #### [SECOND] Cleaning the AEA Annex classification matching table
  accounts_nuts1_code <- as.vector(unlist(env_accounts_air_gge %>% distinct(src_crf) %>% 
  mutate(digits=nchar(src_crf)) %>% select(src_crf)))
  ipcc_nace_match_codes <- nfr_crf_nace_r2 %>% mutate(code_ipcc_v1 = stringr::str_sub(code_ipcc,1,7)) %>% 
  mutate(code_ipcc_v2 = stringr::str_sub(code_ipcc,1,5)) %>% mutate(code_ipcc_v3 = stringr::str_sub(code_ipcc,1,3)) %>%
  mutate_at(c("code_ipcc","code_ipcc_v1","code_ipcc_v2","code_ipcc_v3"),~stringr::str_to_sentence(gsub('\\.', '',.))) %>% 
  filter(code_ipcc_v1%in%c(unique(accounts_nuts1_code))| code_ipcc_v2%in%c(unique(accounts_nuts1_code))|
  code_ipcc_v3%in%c(unique(accounts_nuts1_code))) %>% arrange(code_ipcc) %>% 
  mutate(code_match = if_else(code_ipcc_v1%in%c(unique(accounts_nuts1_code)),code_ipcc_v1,NA)) %>%
  mutate(code_match = if_else(code_ipcc_v2%in%c(unique(accounts_nuts1_code)),code_ipcc_v2,code_match)) %>%
  mutate(code_match = if_else(code_ipcc_v3%in%c(unique(accounts_nuts1_code)),code_ipcc_v2,code_match)) %>%
  select(-code_ipcc_v1,-code_ipcc_v2,-code_ipcc_v3) %>% distinct(code_main,code_match,code_nace_r2) %>%
  filter(!code_nace_r2%in%c("easy","medium","verycomplex"))
  
    #################### 
    # [==> Correction by the city inventory
    #########################################################
    #### [a] According to the city there are no 01 emissions
    ipcc_nace_match_codes %<>% filter(!code_main%in%c("01"))

#################### 
# [D] Households' direct emissions using HBS
#########################################################
  #### [a] Bottom-up computation of emissions by households using HBS
  # budget_survey_hh_city_5d <- readRDS(file.path(getwd(),"datasets","epf_g_b16b.rds")) %>%
  # bind_rows(readRDS(file.path(getwd(),"datasets","epf_g_b06b.rds"))) %>% filter(ANOENC%in%c(seq(year_str_io,year_end_city))
  #saveRDS(budget_survey_hh_city_5d %>% arrange(ANOENC),file.path(getwd(),"datasets","epf_g_0616_d5_grupo04_07.rds"))
  if(sum(objects()=="budget_survey_hh_city_5d")==0){budget_survey_hh_city_5d <- readRDS(file.path(getwd(),
  "datasets","epf_g_0616_d5_grupo04_07.rds"))}
  budget_survey_hh_city_5d %<>% left_join(readxl::read_xlsx(file.path(getwd(),"datasets","hbs_emissions_table.xlsx"),
  col_names = TRUE) %>% mutate_at(c("CODIGOd3","CODIGOd4","CODIGO"),~stringr::str_remove_all(.,fixed("."))) %>%
  pivot_longer(`2007`:`2022`,names_to = "ANOENC",values_to = "INTENSRATIO") %>% distinct(ANOENC,CODIGO,INTENSRATIO,CATEGORIA) %>%
  mutate_at(c("ANOENC"),~as.double(.)),by=c("ANOENC","CODIGO")) %>% filter(!is.na(INTENSRATIO)&!is.na(CANTIDAD)) %>%
  select(ANOENC,NUMERO,FACTOR,CATEGORIA,CODIGO,CODIGOd3,CANTIDAD,INTENSRATIO) %>% 
  mutate(kgCO2direct=CANTIDAD*INTENSRATIO) %>% ungroup()
    #### [ATTENTION] we exclude "other dwellings" on the assumption that they are not located in the city
    budget_survey_hh_city_5d %<>% filter(!grepl("OTRAS",CATEGORIA)) %>% group_by(ANOENC,NUMERO,FACTOR,CODIGOd3) %>% 
    summarize_at(c("kgCO2direct"),~sum(.,na.rm=TRUE)) %>% pivot_wider(names_from = "CODIGOd3",
    values_from = "kgCO2direct") %>% rename("HH_HEAT"="045","HH_TRA"="072")
  
  #### [b] Derive HH_TRA and HH_HEAT values from HBS
  budget_survey_direct_CO2 <- budget_survey_hh_city %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>%
  distinct(ANOENC,NUMERO,FACTOR,CCAA,CAPROV) %>% left_join(budget_survey_hh_city_5d %>% rename("FACTOR0"="FACTOR") %>%
  select(ANOENC:FACTOR0,HH_HEAT,HH_TRA) %>% ungroup(),by=c("ANOENC","NUMERO")) %>%
  mutate(kgCO2TOT=rowSums(across(`HH_HEAT`:`HH_TRA`),na.rm=TRUE)) %>% group_by(ANOENC) %>% 
  summarise_at(c("HH_HEAT","HH_TRA"),~sum(.*FACTOR,na.rm=TRUE)/1e06)
  
#################### 
# [E] Emissions Inventory of Madrid
#########################################################
  #### [A] === [INVENTORY COMUNIDAD y AYUNTAMIENTO DE MADRID]
  #### GHG: CO2 + N2O + CH4 + HFC + PFC + NF3 + SF6
  enviro_madrid <- data.frame()
  temp_hh_other <- data.frame()
  #### [METHOD - Conversion through correspondence tables]
  for(year_n in seq(year_str_io,year_end_city)){
      #### [A] select the emissions classification (IPCC/SNAP)
      
        #### [1] create bridge matrix from CRF to SNAP (normalized by SNAP)
        bridge_snap_crf <- ipcc_nace_match_codes %>% distinct(code_main,code_match) %>% left_join(env_accounts_air_gge %>% 
        filter(TIME_PERIOD==year_n) %>% distinct(src_crf,OBS_VALUE),by=c("code_match"="src_crf")) %>% arrange(code_main)
        bridge_snap_crf %<>% pivot_wider(names_from = "code_main",values_from = "OBS_VALUE") %>% left_join(bridge_snap_crf %>% 
        distinct(code_match,OBS_VALUE),by=c("code_match")) %>% mutate(TOT=rowSums(across(`02`:`10`),na.rm=TRUE)) %>% 
        mutate_at(c(unique(bridge_snap_crf$code_main)),~(./TOT)*OBS_VALUE) %>% select(-OBS_VALUE,-TOT) %>%
        pivot_longer(`02`:`10`) %>% pivot_wider(names_from = "code_match",values_from = "value") %>% 
        mutate(TOT=rowSums(across(`1a4`:`4g`),na.rm=TRUE)) %>% mutate_at(c(unique(bridge_snap_crf$code_match)),~./TOT) %>%
        select(-TOT) %>% column_to_rownames("name") %>% mutate_all(~if_else(is.na(.)|is.nan(.),0,.))
    
        #### [2] create bridge matrix from CRF to NACE (normalized by CRF)
        bridge_crf_nace_r2 <- ipcc_nace_match_codes %>% left_join(code_nace_r2 %>% distinct(letter,nace_3d,nace_r2,industry),
        by=c("code_nace_r2"="nace_3d")) %>% distinct(code_match,nace_r2) %>% left_join(env_accounts_ainah_r2 %>% 
        filter(TIME_PERIOD==year_n) %>% distinct(nace_r2,OBS_VALUE),by=c("nace_r2")) %>% arrange(code_match) %>%
          #### [ATTENTION] serious problems allocating no-combustion activities to HH_OTH
          #### results allocate 46% of total emissions to this sector to households other
          filter(!grepl("HH",nace_r2)) %>% filter(!nace_r2=="U") #mutate_at(c("OBS_VALUE"),~if_else(grepl("HH",nace_r2),0,.))
        bridge_crf_nace_r2 %<>% pivot_wider(names_from = "code_match",values_from = "OBS_VALUE") %>% arrange(nace_r2) %>%
        left_join(bridge_crf_nace_r2 %>% distinct(nace_r2,OBS_VALUE),by=c("nace_r2")) %>% mutate(TOT=rowSums(across(`1a2`:`4g`),
        na.rm=TRUE)) %>% mutate_at(c(unique(bridge_crf_nace_r2$code_match)),~(./TOT)*OBS_VALUE) %>%
        mutate(TOT=rowSums(across(`1a2`:`4g`),na.rm=TRUE)) %>% select(-OBS_VALUE) %>% ungroup() %>%
        mutate_at(c(unique(bridge_crf_nace_r2$code_match)),~./sum(.,na.rm=TRUE)) %>% select(-TOT) %>%
        mutate_at(c(unique(bridge_crf_nace_r2$code_match)),~if_else(is.na(.),0,.)) %>% filter(!is.na(nace_r2)) %>%
        arrange(factor(nace_r2,levels=c(as.vector(unlist(code_nace_r2 %>% distinct(nace_r2)))))) %>%
        column_to_rownames("nace_r2") %>% mutate_all(~if_else(is.na(.)|is.nan(.),0,.)) 
        #rownames_to_column("nace_r2") %>% filter(!grepl("HH",nace_r2)) %>% column_to_rownames("nace_r2")
        
        #### [3] reorder columns to make sure they match & create bridge matrix from SNAP to NACE
        #### [ATTENTION] if HH eliminated, then one IPCC sector is missing
        bridge_snap_crf %<>% select(all_of(names(bridge_crf_nace_r2)))
        if(sum(names(bridge_snap_crf)==names(bridge_crf_nace_r2))==length(names(bridge_crf_nace_r2))){
        print(paste0(year_n,": IPCC column sectors are ordered CORRECTLY")) } else {
        print(paste0(year_n,": IPCC column sectors are ordered INcorrectly"))}
        bridge_snap_nace <- as.matrix(bridge_snap_crf)%*%t(as.matrix(bridge_crf_nace_r2))
      
      #### [B] Load the city's inventory and substract households' direct emissions from the ES-HBS and residence principle
      #### [==> Using the air emissions accounts totals bridging to emission inventory totals [env_ac_aibrid_r2],
      #### we can adjust approximatedly by the residence principle.
      #### This data set includes bridging items showing the differences between the national totals as derived from two 
      #### internationally established approaches/methods for reporting emissions of greenhouse gases and air pollutants:
      
        #### [1] load the city's inventory and redistribute SNAP 11 proportionally
        #### [==> SNAP 11 is nature and has no established linkage plus it's rather small
        city_inventory <- readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Inventario",
        "inventario_madrid_2021.xlsx")) %>% set_names(c("year",paste0("0",2:9),"10","11","TOT")) %>%
        filter(year==year_n) %>% select(-year,-TOT) %>% t %>% as.data.frame() %>% rownames_to_column("code") 
        city_inventory_original <- city_inventory
        #### [ATTENTION] I proportionally allocate emissions from SNAP 11
        city_inventory %<>% filter(!code=="11") %>% left_join(city_inventory %>% mutate_at(c("V1"),~sum(.,na.rm=TRUE)) %>%
        rename("V2"="V1"),by=c("code")) %>% mutate_at(c("V1"),~.*(V2/sum(.,na.rm=TRUE))) %>% select(code,V1) %>%
        column_to_rownames("code")
        
        #### [ATTENTION] Should I exclude air emissions as the Barajas % on the import item
        #### [2] Adjust the residence principle: use bridge items to change the totals from the inventory 
        #### to distribute proportionally but making emphasis in other transportation means
        bridge_accounts <- env_accounts_aibrid_r2 %>% filter(TIME_PERIOD%in%c(seq(year_str_io,year_end_city))) %>% filter(grepl("AEMIS_RES",indic_env)|grepl("AEMIS_TER",indic_env)) %>%
        filter(!grepl("LULUCF",indic_env)) %>% filter(!grepl("FWTR",indic_env)) %>% filter(!grepl("WTR",indic_env)) %>% 
        filter(!indic_env%in%c("AEMIS_TER","AEMIS_RES")) %>% select(indic_env,TIME_PERIOD,OBS_VALUE) %>% 
        mutate(indic_env_short=stringr::str_sub(indic_env,7,9)) %>% mutate_at(c("indic_env"),~stringr::str_sub(indic_env,-4L,-1L)) %>% 
        mutate_at(c("indic_env"),~stringr::str_remove_all(.,fixed("_"))) %>% mutate_at(c("indic_env"),~if_else(.=="NRES","ABR",.)) %>% 
        pivot_wider(names_from = "indic_env_short",values_from = "OBS_VALUE") %>% mutate(BALANCE=RES-TER) %>% filter(TIME_PERIOD==year_n) %>% 
        left_join(enviro_figaro %>% filter(ref_area=="ES"&time_period==year_n) %>% 
        mutate_at(c("industry"),~stringr::str_replace_all(.,setNames(c(sort(as.vector(unlist(code_iots_city %>%
        filter(type=="sector") %>% distinct(nace_r2_64))))),c(sort(unique(enviro_figaro$industry)[-!unique(enviro_figaro$industry)=="HH"]))))) %>% 
        group_by(time_period) %>% summarize_at(c("ktGHGF"),~sum(.,na.rm=TRUE)),by=c("TIME_PERIOD"="time_period")) %>%
        mutate(`proportion`=BALANCE/ktGHGF) %>% mutate_at(c("indic_env"),~case_when(.=="ABR"~"Total fuel",
        .=="ATR"~"Air transport",.=="LTR"~"Land transport"))
        bridge_accounts %<>% left_join(readxl::read_xlsx(file.path(getwd(),"datasets","air_travel_barajas.xlsx")) %>%
        select(YEAR,`%`) %>% mutate(type="Air transport"),by=c("TIME_PERIOD"="YEAR","indic_env"="type")) %>%
        mutate_at(c("proportion"),~if_else(indic_env=="Air transport",`%`,0.15)) %>% select(-`%`) %>%
        filter(!indic_env=="Total fuel") %>% mutate(BALANCET=sum(BALANCE,na.rm=TRUE)) %>% 
        mutate(city_balance=proportion*BALANCE) %>% mutate(code=case_when(indic_env=="Air transport"~"08",
        indic_env=="Land transport"~"07")) %>% select(code,city_balance)
        #### selecting the total fuel adjustment including fisheries and water transport since likely
        #### vessel firms are located in Madrid
        city_inventory %<>% as.data.frame() %>% rownames_to_column("code") %>% left_join(bridge_accounts,by=c("code")) %>% 
        mutate_at(c("V1"),~if_else(is.na(city_balance),.,.+city_balance)) %>% select(code,V1)
        #### [ATTENTION] this is highly missleading since Madrid concentrates a large part of
        #### Spanish company's headquarters, and hence they own no small amount of emissions
      
        #### [3] substract from the inventory the totals the emisions derived from households
        #### This relates to SNAP 02 (non-industrial combustion), SNAP 07 (road transport) and SNAP 06 (disolvents)
        city_inventory %<>% left_join(budget_survey_direct_CO2 %>% mutate_at(c("HH_TRA"),~.*0.7) %>% 
        select(ANOENC:HH_TRA) %>% filter(ANOENC==year_n) %>%
        pivot_longer(`HH_HEAT`:`HH_TRA`,names_to = "nace_r2",values_to = "HH") %>% select(-ANOENC) %>%
        mutate_at(c("nace_r2"),~case_when(.=="HH_HEAT"~"02",.=="HH_TRA"~"07")),by=c("code"="nace_r2")) %>%
          #### [ATTENTION] ad hoc correction
          mutate_at(c("HH"),~if_else(is.na(HH),0,.)) %>% mutate_at(c("V1"),~.-HH) %>% 
          select(-HH) %>% column_to_rownames("code") %>% as.matrix()
        # env_accounts_ainah_r2 %>% filter(TIME_PERIOD==year_n) %>% distinct(nace_r2,OBS_VALUE) %>% 
        # filter(nace_r2%in%c(as.vector(unlist(code_indus64 %>% filter(type=="sector") %>% select(nace_64))),
        # "HH_TRA","HH_HEAT","HH_OTH")) %>% mutate_at(c("OBS_VALUE"),~./sum(.,na.rm=TRUE)) %>% filter(grepl("HH",nace_r2))
        
      #### [D] Apply certain ad hoc 
      #### === [ATTENTION] ad hoc corrections === ####
      
        #### [1] redistribute SNAP 06 to households at 70%
        bridge_snap_nace %<>% as.data.frame %>% rownames_to_column("code") %>% 
        mutate_if(is.numeric,~if_else(code=="06"&.>0,.*.7,.)) %>% column_to_rownames("code")
        
        #### [2] almost eliminate AGRICULTURE (A) from the bridge matrix
        bridge_snap_nace %<>% as.data.frame %>% rownames_to_column("code") %>% 
        mutate_at(c("A01","A02","A03"),~.*0.1) %>% column_to_rownames("code")
      
        #### [3] eliminate MINING (B)
        bridge_snap_nace %<>% as.data.frame %>% rownames_to_column("code") %>% 
        mutate_at(c("B"),~.*0) %>% column_to_rownames("code")
      
      #### [E] modify energy production (E35) to fit an "energy sink"
      #### === [ATTENTION] ad hoc corrections === ####
      #### The energetic specificities of the city are the following: few sources of primary energy
      #### The city lacks fossil fuel reserves. No large energy transformation plants for power 
      #### generation or fuel production (refineries). Reduced electricity generation. 
      #### (Pérez et al., 2019, A methodology for the development of urban energy balances)
      ####       (D35) Electricity, gas, steam and air conditioning supply
      ####     (E36) Water collection, treatment and supply
      #### (E37-E39) Sewerage, waste management, remediation activities
      bridge_snap_nace %<>% as.data.frame %>% rownames_to_column("code") %>% 
      mutate_at(c("D35"),~.*0.05) %>% column_to_rownames("code")
        
      #### [F] Derive and aggregate to city abridged NACE rev.2 classification
        
        #### [1] derive the city's accounts using the adjusted inventory and the adjusted bridge matrix
        bridge_snap_nace %<>% as.data.frame %>% rownames_to_column("code") %>% mutate(TOT=rowSums(across(`A01`:`T`),
        na.rm=TRUE)) %>% mutate_if(is.numeric,~if_else(code!="06",./TOT,.)) %>% select(-TOT) %>% column_to_rownames("code")
        city_accounts  <- as.vector(city_inventory)%*%as.matrix(bridge_snap_nace) %>%
        as.data.frame %>% pivot_longer(`A01`:`T`,names_to="nace_r2",values_to="obs_value")
        temp_hh_other_year <- as.data.frame(sum(city_inventory)-sum(city_accounts$obs_value)) %>% 
        set_names("HH_OTH") %>% mutate(ANOENC=year_n)
        
        #### [2] aggregate the accounts down to city's classification digits
        temp_city_accounts <- city_accounts %>% left_join(code_iots_city %>% filter(type=="sector") %>% 
        distinct(nace_code,nace_r2_64),by=c("nace_r2"="nace_r2_64")) %>% mutate_at(c("nace_code"),~if_else(is.na(.),
        nace_r2,.)) %>% group_by(nace_code) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>%
        arrange(factor(nace_code,levels=c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% 
        distinct(nace_code))),as.vector(unlist(code_nace_r2 %>% filter(letter=="HH") %>% select(nace_r2)))))) %>%
        rename("nace_r2"="nace_code","ktCO2"="obs_value");temp_city_accounts_pre <- temp_city_accounts
        #### check that results are reasonable temp_city_accounts %>% mutate_at(c("ktCO2"),~./sum(.,na.rm=TRUE)) %>% print(n=31)
      
     #### [G] perform a final adjustment to reweight emissions by the city's output share vis-a-vis the national one
    if(city_accounts_adjust==TRUE){
        #### [1] correct the city accounts by the difference in country/city output shares
        temp_city_accounts %<>% filter(!grepl("HH",nace_r2)) %>% mutate(total=sum(ktCO2,na.rm=TRUE)) %>%
        left_join(city_mrio_industry_final %>% ungroup %>% select(code,year,P1) %>%
        separate(code,c("cou","nace_r2"),sep = "_",extra = "merge") %>% filter(cou%in%c("ES","ES30")) %>%
        pivot_wider(names_from = "cou",values_from = "P1") %>% filter(year==year_n) %>%
        mutate_at(c("ES","ES30"),~./sum(.,na.rm=TRUE)) %>% mutate(scale_factor=ES30/ES) %>%
        select(nace_r2,scale_factor),by=c("nace_r2")) %>% mutate(ktCO2_to_scale=ktCO2*scale_factor) %>%
        mutate(total=total-sum(ktCO2,na.rm=TRUE)) %>% select(nace_r2,ktCO2_to_scale) %>%
        rename("ktCO2"="ktCO2_to_scale") %>%
        #### [b] correct the levels back to previous scale
        left_join(temp_city_accounts %>% filter(!grepl("HH",nace_r2)) %>% mutate(ktCO2T=sum(ktCO2,na.rm=TRUE)) %>%
        select(nace_r2,ktCO2T),by=c("nace_r2")) %>% mutate(scale=sum(ktCO2,na.rm=TRUE)/ktCO2T) %>%
        mutate_at(c("ktCO2"),~./scale) %>%
        #### [c] append household emissions
        bind_rows(temp_city_accounts %>% filter(grepl("HH",nace_r2))) %>%
        select(nace_r2,ktCO2)

          temp_city_accounts %>% left_join(temp_city_accounts_pre,by=c("nace_r2")) %>%
          set_names(c("nace_r2","adjusted","unadjusted")) %>% pivot_longer(`adjusted`:`unadjusted`,
          names_to = "factor",values_to = "ktCO2") %>%
          ggplot(aes(x=ktCO2, y=nace_r2,fill=factor)) + geom_bar(stat="identity", position=position_dodge()) +
          ggtitle("Emissions distribution by industry before and after adjustment") +
          facet_wrap(~factor,scales = "free_x",ncol = 2) + xlab("ktCO2-eq") + ylab("") +
          theme(legend.position = "none",text = element_text(size = 15),axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())
    }

        temp_city_accounts %>% ggplot(aes(x=ktCO2, y=nace_r2)) + 
        geom_bar(stat="identity", position=position_dodge()) +
        ggtitle("Emissions distribution by industry before and after adjustment") +
        xlab("ktCO2-eq") + ylab("") + theme(legend.position = "none",text = element_text(size = 15),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
        
    if(round(sum(city_accounts$obs_value)+sum(temp_hh_other_year$HH_OTH),10)==round(sum(city_inventory),10)){
    print(paste0(year_n,": The total from the city inventory MATCH the total from the city accounts (minus households)")) } else {
    print(paste0(year_n,": The total from the city inventory DO NOT match the total from the city accounts (minus households)"))}
    temp_hh_other %<>% bind_rows(temp_hh_other_year)
    enviro_madrid %<>% bind_rows(temp_city_accounts %>% mutate(year=year_n))
  }
  enviro_madrid %<>% bind_rows(budget_survey_direct_CO2 %>% left_join(temp_hh_other,by=c("ANOENC")) %>% 
  pivot_longer(`HH_HEAT`:`HH_OTH`,names_to = "nace_r2",values_to = "ktCO2") %>% rename("year"="ANOENC") %>% 
  select(nace_r2,ktCO2,year)) %>% arrange(year)
  enviro_madrid %>% group_by(year) %>% summarize_at(c("ktCO2"),~sum(.,na.rm=TRUE))
  saveRDS(enviro_madrid,file.path(getwd(),"datasets","city_accounts_madrid.rds"))
} else {
  city_acounts_madrid <- readRDS(file.path(getwd(),"datasets","city_accounts_madrid.rds")) %>% as_tibble
}
  
#### needed changes: lower energy production (B_E), elimiante emissions from agriculture mostly, and larger transport emissions
#################### 
# [G] MRIO Emissions Accounts including local inventory
#########################################################
  #### No SNAP 11 code because all nature and no activity
  #### [a] aggregate FIGARO vectors down to 28 industries
  tem_enviro_figaro <- intensity_factors_ghg %>% separate(code,c("cou","code"),sep = "_",extra = "merge") %>%
  left_join(code_iots_city %>% distinct(nace_code,nace_r2_64),by=c("code"="nace_r2_64")) %>% group_by(cou,year,nace_code) %>% 
  summarize_at(c(paste0("kt",emissions_mod,"F"),"P1"),~sum(.,na.rm=TRUE)) %>% rename("nace_r2"="nace_code") %>% 
  mutate(factor=!!sym(paste0("kt",emissions_mod,"F"))/P1) %>% select(cou,year,nace_r2,factor,
  !!sym(paste0("kt",emissions_mod,"F")),P1) %>% filter(year%in%c(seq(year_str_io,year_end_city))) %>% ungroup
  
  #### [b] expand the accounts vector with city information NATION
  tem_enviro_figaro %<>% filter(!cou=="ES") %>% bind_rows(intensity_factors_ghg %>% separate(code,c("cou","code"),
  sep = "_",extra = "merge") %>% filter(cou=="ES") %>% left_join(code_iots_city %>% distinct(nace_code,nace_r2_64),
  by=c("code"="nace_r2_64")) %>% group_by(cou,year,nace_code) %>% summarize_at(c(paste0("kt",emissions_mod,"F"),"P1"),
  ~sum(.,na.rm=TRUE)) %>% rename("nace_r2"="nace_code") %>% filter(year%in%c(seq(year_str_io,year_end_city))) %>% select(year,cou,nace_r2,
  paste0("kt",emissions_mod,"F"),P1) %>% left_join(city_acounts_madrid,by=c("year","nace_r2")) %>% select(-P1) %>%
  mutate_at(c(paste0("kt",emissions_mod,"F")),~.-ktCO2) %>% left_join(city_mrio_industry_final %>% 
  select(code,year,P1) %>% filter(grepl("ES",code)) %>% separate(code,c("cou","code"),sep = "_",extra = "merge"),
  by=c("cou","year","nace_r2"="code")) %>% mutate(factor=!!sym(paste0("kt",emissions_mod,"F"))/P1) %>%
  select(cou,year,nace_r2,factor,!!sym(paste0("kt",emissions_mod,"F")),P1)) %>% ungroup
  
  #### [c] expand the accounts vector with city information CITY
  tem_enviro_figaro %<>% bind_rows(city_acounts_madrid %>% mutate(cou="ES30") %>% select(cou,year,nace_r2,ktCO2) %>%
  filter(!grepl("HH",nace_r2)) %>% left_join(city_mrio_industry_final %>% ungroup %>% select(code,year,P1) %>% 
  separate(code,c("cou","nace_r2"),sep = "_",extra = "merge"),by=c("cou","year","nace_r2")) %>% 
  mutate(factor = ktCO2/P1) %>% mutate_at(c("factor"),~if_else(is.na(.),0,.)) %>% rename(!!sym(paste0("kt",emissions_mod,"F")):="ktCO2") %>% 
  select(cou:nace_r2,factor,!!sym(paste0("kt",emissions_mod,"F")),P1)) %>% ungroup %>% arrange(cou,year,nace_r2) %>% 
  mutate_at(c("factor"),~if_else(cou=="ES"&nace_r2=="T",0,.))
  
  #### [d] correct some indeterminate ratios (NACE T)
  tem_enviro_figaro %<>% mutate_at(c("factor"),~if_else(is.na(.)|is.nan(.),0,.)) %>% 
  mutate_at(c("factor"),~if_else(is.infinite(.),0,.))
  
  tem_enviro_figaro %>% filter(cou%in%c("ES")) %>% select(year,nace_r2,factor) %>% 
  rename("Rest of the Nation"="factor") %>% left_join(tem_enviro_figaro %>% filter(cou%in%c("ES30")) %>%
  select(year,nace_r2,factor) %>% rename("City"="factor"),by=c("year","nace_r2")) %>% 
  filter(year==seq(year_str_io,year_end_city)[length(seq(year_str_io,year_end_city))]) %>%
  pivot_longer(`Rest of the Nation`:`City`,names_to = "assumption",values_to = "obs_value") %>%
  ggplot(aes(x=obs_value, y=nace_r2, fill=assumption)) + geom_bar(stat="identity", position=position_dodge()) +
  #ggtitle("Emissions distribution by sector in the city of Madrid and the rest of the nation") +
  facet_wrap(~assumption,scales = "free_x",ncol = 2) + xlab("kg CO2-eq per unit of output") + ylab("") +
  theme(legend.position = "none",text = element_text(size = 15),axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank())
  ggsave(file.path(getwd(),"paper","graphics",paste0("intensity_ratios_2019.pdf")),height = 6,width = 12)
  
  if(vector_assumption=="national"){ tem_enviro_figaro %<>% filter(!cou=="ES30") %>%
  bind_rows(tem_enviro_figaro %>% filter(cou=="ES") %>% mutate_at(c("cou"),~paste0(.,"30"))) %>%
  arrange(cou,nace_r2,year)}
  
  #### [e] check and substract city from rest of nation emissions factors
  # tem_enviro_figaro %>% filter(cou%in%c("ES","ES30")) %>% select(cou:nace_r2,factor) %>%
  # pivot_wider(names_from = "cou",values_from = "factor") %>% print(n=28)
  # tem_enviro_figaro %>% filter(cou%in%c("ES","ES30")) %>% group_by(cou,year) %>%
  # summarize_at(c("ktCO2","output"),~sum(.,na.rm=TRUE)) %>% mutate(factor=ktCO2/output) %>%
  # select(cou,year,factor) %>%  pivot_wider(names_from = "cou",values_from = "factor")
  
  saveRDS(tem_enviro_figaro %>% unite(code,"cou","nace_r2"),
  file.path(getwd(),"datasets","city_mrio_env_accounts.rds"))
} else {
  
  city_acounts_madrid <- readRDS(file.path(getwd(),"datasets","city_accounts_madrid.rds")) %>% as_tibble
  city_mrio_env_accounts <- readRDS(file.path(getwd(),"datasets","city_mrio_env_accounts.rds")) %>% as_tibble
  
}
##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################