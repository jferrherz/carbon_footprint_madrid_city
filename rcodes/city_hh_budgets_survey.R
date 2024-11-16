##################################################################################################################################
## [B] BASIC INPUTS FOR ESTIMATIONS - HOUSEHOLD BUDGET SURVEY
##################################################################################################################################
  
if(length(files_path_figaro[files_path_figaro=="budget_survey_hh_city.rds"])==0){
  ####################
  # [A] LOAD ES-HBS SURVEY
  #########################################################
  budget_survey_hh <- readRDS(file.path(getwd(),"datasets","epf_full_b9716_g3d.rds"))
  # ===> transform back the shares into levels for aggregation
  budget_survey_hh %<>% filter(ANOENC%in%c(seq(year_str_io,year_end_city))) %>%
  mutate_at(c("GASTO"),~.*GASTOT) %>% mutate_at(c("GASTOR"),~.*GASTORT) %>%
  mutate_at(c("GASTOMON"),~.*GASTOMONT) %>% mutate_at(c("GASTORMON"),~.*GASTORMONT) %>%
  as_tibble
  
  #################### 
  # [B] MACRO TARGETS
  #########################################################
  # macroeconomi consumption data from Eurostat
  # Euro series (CP MEUR ) are derived from transmitted national currency series using historic exchange rates
  consump_es <- readr::read_csv(file.path(path_eurostat,"nama_10_co3_p3_linear.csv"),show_col_types = FALSE) %>% 
  select(unit,coicop,geo,TIME_PERIOD,OBS_VALUE) %>% rename("cou"="geo") %>% filter(TIME_PERIOD>=1995) %>%
  spread(TIME_PERIOD,OBS_VALUE) %>% select("unit","coicop","cou",paste0(seq(1995,(year_end_io+1)))) %>% 
  mutate_at(c("coicop"),~stringr::str_sub(.,start = 3)) %>% mutate(var = "P3_S14") %>% 
  select(unit,coicop,var,cou,everything()) %>% rename("coicop"="coicop","na_item"="var") %>% 
  filter(cou=="ES"&unit=="CP_MEUR") %>% mutate_at(c("coicop"),~gsub("TAL","TOTAL",.))
  #consump_es %>% filter(coicop=="TOTAL") %>% select(!!sym(paste0(year_str_io)))
  
  #################### 
  # [C] POPULATION ADJUSTMENT
  #########################################################
  # Aligning the population concept as a ratio of the HFCE to NA. These ratios can
  # be interpreted as scaling coefficients of the average representative household
  # - Euro series for total populations
  population_na  <- readr::read_csv(file.path(path_eurostat,"demo_pjan_linear.csv"),
  show_col_types = FALSE) %>% filter(geo=="ES"&age=="TOTAL"&freq=="A",sex=="T") %>% 
  select(-DATAFLOW,-`LAST UPDATE`,-OBS_FLAG,-freq,-age,-sex) %>% mutate(na_item="pop_total") %>%
  rename("cou" = "geo") %>% select(unit,na_item,cou,everything()) %>% filter(TIME_PERIOD > (year_str_io-1)) %>%
  rename("NatAcc"="OBS_VALUE","year"="TIME_PERIOD")
  population_matched <- budget_survey_hh %>% filter(ANOENC%in%c(seq(year_str_io-1,max(budget_survey_hh$ANOENC)))) %>% 
  distinct(ANOENC,NUMERO,NMIEMB,FACTOR) %>% group_by(ANOENC) %>% summarize(pop_total = sum(NMIEMB*FACTOR,na.rm=TRUE)) %>% 
  mutate(na_item="pop_total") %>% rename("BudSrv"="pop_total","year"="ANOENC")
  population_ratio <- population_na %>% left_join(population_matched, by = c("year","na_item")) %>%
  mutate(pop_ratio = NatAcc/BudSrv)
        
  # - Join the HBS dataset with the ratio at the population level. We do this on the gross-up factors
  # since it does not affect relative quantities but only the level.
  budget_survey_hh_na <- budget_survey_hh %>% left_join(population_ratio %>% select(year,pop_ratio),
  by = c("ANOENC"="year")) %>% mutate_at(c("FACTOR"),~.*pop_ratio)
  # check: budget_survey_hh_na %>% group_by(ANOENC) %>% distinct(ANOENC,NUMERO,NMIEMB,FACTOR) %>%
  # summarize(pop_total = sum(NMIEMB*FACTOR,na.rm=TRUE)) %>% left_join(population_na, by = c("ANOENC"="year"))

  ####################
  # [D] ALIGNING HH SPENDING DATA AND NATIONAL ACCOUNTS
  #########################################################
  # Survey data does not follow National Accounts (NA), on which the IO framework is mostly based.
  # It is necessary to adapt the data from surveys to the national accounting principles to be able 
  # to use it correctly in macroeconomic modelling
    # - HFCE data are part of the NA and their totals are consistent (excluding vintages issues) with 
    #   the total HHFC in the CPA reported in the Supply and Use tables and IO tables
    # - However, the aggregates and the structure HFCE resulting from summing up the HFCE in consumption
    #   surveys does not match  the aggregate HFCE of NA, with differences ranging from 50% to 97% (Amores, 2018)
    # - To make the microdata of the consumption surveys consistent with the HFCE in NA, the authros propose
  # These coefficients are used to align the consumption profile extracted from the 
  # survey to the NA accounting standard. Each category of consumer profiles from the survey 
  # is uprated/downrated by multiplying it category-wise against such coefficients
  # The existence of unmatched COICOP categories between HFCE in NA and consumption surveys 
  # makes a stepwise procedure necessary
      
        #### [0] select the consumption columns to make computation easier
        budget_survey_hh_coicop <- budget_survey_hh_na %>% select(ANOENC,
        NUMERO,starts_with("CODIGO"),GASTO,GASTOR,CANTIDAD,FACTOR)
        
        #### [a] First step: aligning the COICOP categories that are present in both NA and surveys
        # - Euro series (CP MEUR ) are derived from transmitted national currency series using historic exchange rates
        consump_na_3d <- readr::read_csv(file.path(path_eurostat,"nama_10_co3_p3_linear.csv"),show_col_types = FALSE) %>% 
        select(unit,coicop,geo,TIME_PERIOD,OBS_VALUE) %>% rename("cou"="geo") %>% filter(TIME_PERIOD>=1995) %>%
        spread(TIME_PERIOD,OBS_VALUE) %>% select("unit","coicop","cou",paste0(seq(paste0(year_str_io),paste0(year_end_io+1)))) %>% 
        mutate_at(c("coicop"),~stringr::str_sub(.,start = 3)) %>% mutate(var = "P3_S14") %>% 
        mutate_at(c("coicop"),~if_else(.=="122_127","122",.)) %>% select(unit,coicop,var,cou,everything()) %>%
        rename("coicop"="coicop","na_item"="var") %>% filter(cou=="ES"&unit=="CP_MEUR") %>% filter(!coicop=="TAL") %>%
        gather(year,value,!!sym(paste0(year_str_io)):!!sym(paste0(year_end_io+1))) %>% mutate(digits = nchar(coicop)) %>% 
        mutate_at(c("coicop"),~stringr::str_pad(.,3,side = c("right"),pad = 0)) %>% separate(coicop,c("coicop"),sep = "_",
        extra = "drop") %>% filter(year>(year_str_io-1)&digits==3)
        # [ATTENTION] ===> duplicate CP122 to cover CP127 and correct the percentages since CP122 is missing in HBS
        consump_na_3d %<>% filter(!coicop=="122") %>%
        bind_rows(consump_na_3d %>% filter(coicop=="122") %>% mutate(coicop="122") %>% mutate_at(c("value"),~.*0.01)) %>%
        bind_rows(consump_na_3d %>% filter(coicop=="122") %>% mutate(coicop="127") %>% mutate_at(c("value"),~.*0.99)) %>%
        arrange(year,coicop)
        
        #### [b] Second step: calculate the ratio of the sample to the macro data
        budget_survey_hh_uprate_3d <- budget_survey_hh_coicop %>% group_by(ANOENC,CODIGOd3) %>% 
        summarize_at(c("GASTO","GASTOR","CANTIDAD"),~sum(.*FACTOR,na.rm=TRUE)) %>% ungroup()
        # - Important to join to match NA so that NAs are obtain for missing sectors in survey
        budget_survey_hh_3d_ratio <- consump_na_3d %>% select(coicop,year,value) %>%
        left_join(budget_survey_hh_uprate_3d %>% mutate_at(c("ANOENC"),~as.character(ANOENC)) %>% select(ANOENC:GASTO), 
        by = c("coicop"="CODIGOd3","year"="ANOENC")) %>% mutate_at(c("GASTO"),~./1000000) %>% 
        mutate(coicop_ratio = value/GASTO) %>%
        # ===> I eliminate narcotics: a) no information from 2016; b) outlier; c) no comparable item
        mutate_at(c("coicop_ratio"),~if_else(coicop=="023",0,.))
        
        #### [c] Third step: gross up consumption data by COICOP category
        # - Join the HBS dataset with the ratio at the category level, which affects relative quantities
        budget_survey_hh_3d <- budget_survey_hh_coicop %>% left_join(budget_survey_hh_3d_ratio %>% select(coicop,year,coicop_ratio) %>% 
        # [ATTENTION] eliminate the missing sectors from the survey data #filter(!is.na(coicop_ratio)) %>% 
        mutate_at(c("year"),~as.integer(year)),by = c("ANOENC"="year","CODIGOd3"="coicop")) %>% 
        # [ATTENTION] grossing up of the expenditure data
        mutate_at(c("coicop_ratio"),~if_else(is.na(.),1,.)) %>%
        mutate_at(c("GASTO","GASTOR","CANTIDAD"),~.*coicop_ratio)
        # check: budget_survey_hh_3d %>% group_by(ANOENC,CODIGOd3) %>% summarize(GASTO=sum(GASTO*FACTOR,na.rm=TRUE)/1000000)
        # check: budget_survey_hh_3d %>% group_by(ANOENC) %>% summarize_at(c("GASTO"),~sum(GASTO*FACTOR,na.rm=TRUE)/1000000)
        # check: consump_na_3d %>% group_by(year) %>% summarize_at(c("value"),~sum(value,na.rm=TRUE))
        
        #### [d] Fourth step: distribute proportionally the two missing sectors
        # - (023) proportional allocation of narcotics at the HH level
        budget_survey_hh_3d %<>% left_join(budget_survey_hh_3d_ratio %>% group_by(year) %>%
        mutate(coicop_ratio_narcotics = 1+(value/sum(value,na.rm=TRUE))) %>% filter(coicop=="023") %>%
        select(coicop_ratio_narcotics,year) %>% mutate_at(c("year"),~as.numeric(year)),by = c("ANOENC"="year")) %>%
        mutate_at(c("GASTO","GASTOR","CANTIDAD"),~.*coicop_ratio_narcotics) %>% #filter(!CODIGOd3=="023")
        mutate_at(c("GASTO","GASTOR","CANTIDAD"),~if_else(CODIGOd3=="023",0,.))
        # check: budget_survey_hh_3d %>% group_by(ANOENC,CODIGOd3) %>% summarize(GASTO = sum(GASTO*FACTOR,na.rm=TRUE)/1000000)
        # - (128) proportional allocation of narcotics at the HH level
        budget_survey_hh_3d %<>% left_join(budget_survey_hh_3d %>% group_by(ANOENC,CODIGOd3) %>% 
        summarize_at(c("GASTO"),~sum(.,na.rm=TRUE)) %>% ungroup %>% group_by(ANOENC) %>% mutate(Total = sum(GASTO,na.rm=TRUE)) %>%
        mutate(coicop_ratio_remittances = 1+(GASTO/Total)) %>% filter(CODIGOd3=="128") %>%
        select(coicop_ratio_remittances,ANOENC) %>% mutate_at(c("ANOENC"),~as.numeric(.)),by = c("ANOENC")) %>%
        mutate_at(c("GASTO","GASTOR","CANTIDAD"),~.*coicop_ratio_remittances) %>% #filter(!CODIGOd3=="128")
        mutate_at(c("GASTO","GASTOR","CANTIDAD"),~if_else(CODIGOd3=="128",0,.))
        # check: budget_survey_hh_3d %>% group_by(ANOENC,CODIGOd3) %>% summarize(GASTO = sum(GASTO*FACTOR,na.rm=TRUE)/1000000)
        #### [ATTENTION] I keep the "ghost" (0s) sectors to avoid excluding some fringe households and affect the totals. Ideally this
        #### should be checked, but it may not introduce very large bias
        
        #### [e] Fifth step: match the NA data
        # - ratio of adjusted survey to NA data
        miss_ratio <- budget_survey_hh_3d %>% group_by(ANOENC) %>% summarize(GASTOT = sum(sum(GASTO*FACTOR,na.rm=TRUE)/1000000)) %>% 
        left_join(consump_na_3d %>% group_by(year) %>% summarize(value = sum(value,na.rm=TRUE)) %>% mutate(year = as.integer(year)),
        by = c("ANOENC"="year")) %>% mutate(miss_ratio = value/GASTOT) %>% ungroup()
        
        #### [f] Sixth step: final grossing up 
        # - upscalling of the survey data to finally match NA
        budget_survey_hh_3d %<>% left_join(miss_ratio %>% select(ANOENC,miss_ratio),
        by = c("ANOENC")) %>% mutate_at(c("GASTO"),~.*miss_ratio) %>% filter(ANOENC%in%c(seq(year_str_io,year_end_io)))
        # check: budget_survey_hh_3d %>% group_by(ANOENC) %>% summarize(spend_total = sum(GASTO*FACTOR,na.rm=TRUE)/1000000)
  
  ####################
  # [D] APPENDING DEMOGRAPHIC VARIABLES AND CITY SUBSETTING
  #########################################################
  if(sum(objects()=="ipc_g3d_set")==0){
  ipc_g3d_set <- readRDS(file.path(getwd(),"datasets",'ipc_g3d_set.rds')) 
  ipc_g3d_set %<>% mutate(BASE=!!sym(paste0(year_str_io))) %>% 
  mutate_at(c(names(ipc_g3d_set)[c(-1:-3)]),~.*100/BASE) %>% select(-BASE)}
  
  budget_survey_hh_city_all <- budget_survey_hh_3d %>% select(ANOENC:FACTOR) %>%
  #### [a] include all the socio-dmeographic variables [exclude FACTORS, since different]
  left_join(budget_survey_hh %>% select(-all_of(c("GASTO","GASTOR","CANTIDAD")),
  -all_of(c("CODIGOd2","CODIGOd3","CODIGO")),-FACTOR,-starts_with("IPC"),-CATEGORIA) %>% 
  distinct(ANOENC,NUMERO,.keep_all = TRUE),by=c("ANOENC","NUMERO"))
  #### [b] include inflation variables
  budget_survey_hh_city_all %<>% left_join(ipc_g3d_set %>% select(CODIGO,CATEGORIA,
  !!sym(paste0(year_str_io)):!!sym(paste0(year_end_io))) %>% filter(!CODIGO=="000") %>%
  pivot_longer(!!sym(paste0(year_str_io)):!!sym(paste0(year_end_io)),names_to = "ANOENC",
  values_to = "IPC3d") %>% rename("CODIGOd3"="CODIGO") %>% mutate_at(c("ANOENC"),
  ~as.double(.)),by=c("ANOENC","CODIGOd3")) %>% left_join(ipc_g3d_set %>% 
  select(CODIGO,CATEGORIA,!!sym(paste0(year_str_io)):!!sym(paste0(year_end_io))) %>% 
  filter(CODIGO=="000") %>% pivot_longer(!!sym(paste0(year_str_io)):!!sym(paste0(year_end_io)),
  names_to = "ANOENC",values_to = "IPC3d") %>% rename("CODIGOd3"="CODIGO") %>% 
  mutate_at(c("ANOENC"),~as.double(.)) %>% select(ANOENC,IPC3d) %>%
  rename("IPC"="IPC3d"),by=c("ANOENC"))
  
  #### [c] adjustment to spending and income variables
  budget_survey_hh_city_all %<>% mutate_at(c("GASTOR"),~GASTO/(IPC3d/100)) %>% group_by(ANOENC,NUMERO) %>% 
  mutate_at(c("GASTOT"),~sum(GASTO,na.rm=TRUE)) %>% mutate_at(c("GASTORT"),~sum(GASTOR,na.rm=TRUE)) %>% ungroup %>% 
  mutate_at(c("GASTOEQV"),~GASTOT/UC2) %>% mutate_at(c("GASTOREQV"),~GASTORT/UC2) %>%
  mutate(IMPEXACR=IMPEXAC/IPC,IMPEXACREQV=IMPEXACR/UC2) %>%
  select(ANOENC:NUMERO,FACTOR,CODIGOd3,CATEGORIA,GASTO,GASTOEQV,GASTOT,
  IPC3d,GASTOR,GASTOREQV,GASTORT,everything())
  
  if(round(mean(as.vector(unlist(budget_survey_hh_city_all %>% group_by(ANOENC) %>% 
  summarize(spend_total = sum(GASTO*FACTOR,na.rm=TRUE)/1000000) %>%
  left_join(consump_na_3d %>% group_by(year) %>% summarize_at(c("value"),
  ~sum(value,na.rm=TRUE)) %>% mutate_at(c("year"),~as.double(.)),by=c("ANOENC"="year")) %>% 
  mutate(ratio=spend_total/value) %>% select(ratio)))),5)==1){
  print(paste0("National Accounts and surve totals MATCH"))} else {
  print(paste0("National Accounts and surve totals DO NOT match"))}
  # population check ES: budget_survey_hh_city_all %>% distinct(ANOENC,NUMERO,
  # FACTOR,NMIEMB) %>% group_by(ANOENC) %>% summarize(sum(FACTOR*NMIEMB))
  
  budget_survey_hh_city_all %<>% left_join(readRDS(file.path(getwd(),"datasets",
  "epf_h_b2006.rds")) %>% select(ANOENC,NUMERO,CCAA,CAPROV) %>% 
  bind_rows(readRDS(file.path(getwd(),"datasets","epf_h_b2016.rds")) %>%
  select(ANOENC,NUMERO,CCAA,CAPROV)),by=c("ANOENC","NUMERO","CCAA"))
  
  #### [d] subset to city level (ES30: Madrid to Madrid city)
  budget_survey_hh_city <- budget_survey_hh_city_all %>% filter(CCAA==13&CAPROV==1&DENSIDAD==1&ANOENC%in%c(seq(year_str_io,year_end_io)))
  budget_survey_hh_ror  <- budget_survey_hh_city_all %>%  filter(CCAA==13&CAPROV!=1&DENSIDAD==1&ANOENC%in%c(seq(year_str_io,year_end_io)))
  # population check Madrid: budget_survey_hh_city %>% distinct(ANOENC,NUMERO,
  # FACTOR,NMIEMB) %>% group_by(ANOENC) %>% summarize(sum(FACTOR*NMIEMB))
  saveRDS(budget_survey_hh_city,file.path(getwd(),"datasets","budget_survey_hh_city.rds"))
  saveRDS(budget_survey_hh_ror,file.path(getwd(),"datasets","budget_survey_hh_ror.rds"))
  budget_survey_hh_city %>% distinct(ANOENC,NUMERO,FACTOR,NMIEMB) %>% group_by(ANOENC) %>% summarize(sum(FACTOR*NMIEMB)) 
  budget_survey_hh_ror %>% distinct(ANOENC,NUMERO,FACTOR,NMIEMB) %>% group_by(ANOENC) %>% summarize(sum(FACTOR*NMIEMB)) 
  } else{
  budget_survey_hh_city <- readRDS(file.path(getwd(),"datasets","budget_survey_hh_city.rds"))
  budget_survey_hh_ror  <- readRDS(file.path(getwd(),"datasets","budget_survey_hh_ror.rds"))
  }

if(length(files_path_figaro[files_path_figaro=="epf_g_0616_d5_grupo04_07.rds"])==0){
    for(base in c("2006","2016")){
    assign(paste0('epf_g_b',str_sub(base,3,4)),
    readRDS(file.path(getwd(),"datasets",paste0('epf_g_b',base,'.rds'))) %>%
    mutate(CODIGOd2=stringr::str_sub(CODIGO,start = 1L,end = 2)) %>%
    mutate(CODIGOd3=stringr::str_sub(CODIGO,start = 1L,end = 3)) %>%
    mutate(CODIGOd4=stringr::str_sub(CODIGO,start = 1L,end = 4)) %>%
    mutate_at(c('GASTO','CANTIDAD','GASTOMON','GASTNOM1','GASTNOM2',
    'GASTNOM3','GASTNOM4','GASTNOM5'),~./FACTOR))}
  
    budget_survey_hh_city_5d <- epf_g_b06 %>% select(ANOENC,NUMERO,FACTOR,CODIGO,GASTO,CANTIDAD,
    CODIGOd2,CODIGOd3,CODIGOd4) %>% bind_rows(epf_g_b16 %>% select(ANOENC,NUMERO,
    FACTOR,CODIGO,GASTO,CANTIDAD,CODIGOd2,CODIGOd3,CODIGOd4)) %>% filter(ANOENC%in%c(seq(year_str_io,year_end_io))) %>%
    filter(CODIGOd2%in%c("04","07"))
    saveRDS(budget_survey_hh_city_5d,file.path(getwd(),"datasets","epf_g_0616_d5_grupo04_07.rds"))
  } else{
  budget_survey_hh_city_5d <- readRDS(file.path(getwd(),"datasets","epf_g_0616_d5_grupo04_07.rds"))
  }
##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################