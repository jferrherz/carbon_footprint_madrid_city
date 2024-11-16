##################################################################################################################################
## NON-EU COUNTRY GHG INTENSITY FACTOS PRE-FIGARO APPLICATION
##################################################################################################################################

#################### 
# [F] GHG for non-EU countries
#########################################################
  if(length(files_path_figaro[files_path_figaro=="intensity_factors_ghg.rds"])==0){
  #### [ OECD ] incomplete non-EU countries (whose GHG emissions match Eurostat)
  oecd_air_emissions_accounts <- readr::read_csv(file.path(database_path,"Environmental","OECD.SDD.NAD.SEEA,DSD_AEA@DF_AEA,1.0+all.csv"),
  col_names = TRUE) %>% filter(FREQ=="A"&UNIT_MEASURE=="T_CO2E",METHODOLOGY=="EMISSIONS_SEEA",DECIMALS==0&!POLLUTANT=="CO2_BIO") %>% 
  select(-STRUCTURE:-ACTION,-MEASURE,-`Measure`,-starts_with("Frequency"),-starts_with("unit"),-starts_with("Adjustment"),
  -SOURCE,-`Time period`,-METHODOLOGY,-Methodology,-`Observation value`,-starts_with("Decimals"),-OBS_STATUS,-Source) %>% 
  rename("cou"="REF_AREA","code"="ACTIVITY","code_name"="Economic activity","air_emiss"="POLLUTANT","air_emiss_name"="Pollutants",
  "scope"="ACTIVITY_SCOPE","scope_name"="Activity scope","year"="TIME_PERIOD","obs_value"="OBS_VALUE","cou_name"="Reference area") %>% 
  arrange(code) %>% left_join(code_indus64 %>% filter(type=="sector") %>% select(nace_64,nace_figaro) %>%
  mutate_at(c("nace_figaro"),~stringr::str_replace_all(.,setNames(c("D","O","P"),c("D35","O84","P85")))),by=c("code"="nace_figaro")) %>%
  mutate(type=if_else(is.na(nace_64),NA,"sector")) %>% mutate_at(c("code"),~if_else(is.na(nace_64),.,nace_64)) %>% select(-nace_64) %>%
  mutate_at(c("type"),~if_else(code%in%c("HH","HH_HEAT","HH_OTH","HH_TR"),"household",.)) %>% filter(type%in%c("sector","household")) %>%
  filter(year%in%c(2010:2021)) %>% arrange(code,cou,year) %>% mutate_at(c("cou"),~stringr::str_sub(.,1,2)) %>%
  mutate_at(c("cou"),~if_else(cou_name=="Austria","AT",.)) %>% mutate_at(c("cou"),~if_else(cou_name=="Estonia","EE",.))%>%
  mutate_at(c("cou"),~if_else(cou_name=="Slovenia","SI",.)) %>% mutate_at(c("cou"),~if_else(cou_name=="Korea","KR",.)) %>% 
  filter(air_emiss=="GHG") %>% ungroup %>% arrange(code,cou,year) %>% mutate_at(c("year"),~as.double(.))
  oecd_air_emissions_accounts %<>% select(-cou) %>% left_join(oecd_air_emissions_accounts %>% distinct(cou,cou_name) %>% 
  left_join(code_countries %>% distinct(CODE,NAME) %>% mutate_at(c("NAME"),~if_else(CODE=="CZ","Czechia",.)) %>%
  mutate_at(c("NAME"),~if_else(CODE=="SV","Slovak Republic",.)) %>% mutate_at(c("NAME"),~if_else(CODE=="KO","Korea",.)),
  by=c("cou_name"="NAME")) %>% mutate_at(c("CODE"),~if_else(is.na(CODE),cou,.)) %>% select(CODE,cou_name) %>% 
  rename("cou"="CODE"),by=c("cou_name")) %>% select(cou,everything())
  
  #### [ EDGAR ]
  edgar_air_emissions_inventories_co2 <- readxl::read_xlsx(file.path(database_path,"Environmental","EDGAR","NUTS1","IEA_EDGAR_CO2_1970_2022.xlsx"),
  col_names = TRUE,sheet="IPCC 2006",skip=9) %>% pivot_longer(`Y_1970`:`Y_2022`,names_to = "year",values_to = "OBS_VALUE") %>%
  separate(year,c("year","TIME_PERIOD"),sep = "_",extra = "merge") %>% select(-year) %>% filter(fossil_bio=="fossil") %>%
  left_join(readr::read_csv(file.path(database_path,"Classifications","country_code_iso_3166_all.csv"),col_names = TRUE) %>%
  select(starts_with("alpha") %>% set_names(c("cou","Country_code_A3"))),by=c("Country_code_A3")) %>% 
  mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_to_lower(stringr::str_remove_all(.,fixed(".")))) %>% 
  mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_sub(.,1,3))
  
  edgar_air_emissions_inventories_co2 %<>% bind_rows(edgar_air_emissions_inventories_co2 %>% filter(!cou%in%c(as.vector(unlist(intensity_factors_co2 %>% 
  separate(code,c("cou","code"),sep = "_",extra = "merge") %>% distinct(cou))))) %>% group_by(cou,TIME_PERIOD) %>%
  mutate(weight=sum(OBS_VALUE,na.rm=TRUE)) %>% group_by(TIME_PERIOD) %>% mutate_at(c("weight"),~./sum(.,na.rm=TRUE)) %>%
  group_by(ipcc_code_2006_for_standard_report,ipcc_code_2006_for_standard_report_name,TIME_PERIOD) %>% 
  summarize_at(c("OBS_VALUE"),~sum(.,na.rm=TRUE)) %>% mutate(IPCC_annex="Non-Annex_I",
  C_group_IM24_sh="Rest of the World",Country_code_A3="FIGW1",Name="RoW",Substance="CO2",fossil_bio="fossil",
  cou="FIGW1") %>% select(all_of(names(edgar_air_emissions_inventories_co2))))
  
  edgar_air_emissions_inventories_ghg <- readxl::read_xlsx(file.path(database_path,"Environmental","EDGAR","NUTS1","EDGAR_AR5_GHG_1970_2022.xlsx"),
  col_names = TRUE,sheet="IPCC 2006",skip=9) %>% pivot_longer(`Y_1970`:`Y_2022`,names_to = "year",values_to = "OBS_VALUE") %>%
  separate(year,c("year","TIME_PERIOD"),sep = "_",extra = "merge") %>% select(-year) %>% filter(fossil_bio=="fossil") %>%
  left_join(readr::read_csv(file.path(database_path,"Classifications","country_code_iso_3166_all.csv"),col_names = TRUE) %>%
  select(starts_with("alpha") %>% set_names(c("cou","Country_code_A3"))),by=c("Country_code_A3")) %>% 
  mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_to_lower(stringr::str_remove_all(.,fixed(".")))) %>% 
  mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_sub(.,1,3))
  
  edgar_air_emissions_inventories_ghg %<>% bind_rows(edgar_air_emissions_inventories_ghg %>% filter(!cou%in%c(as.vector(unlist(intensity_factors_co2 %>% 
  separate(code,c("cou","code"),sep = "_",extra = "merge") %>% distinct(cou))))) %>% group_by(cou,TIME_PERIOD) %>%
  mutate(weight=sum(OBS_VALUE,na.rm=TRUE)) %>% group_by(TIME_PERIOD) %>% mutate_at(c("weight"),~./sum(.,na.rm=TRUE)) %>%
  group_by(ipcc_code_2006_for_standard_report,ipcc_code_2006_for_standard_report_name,TIME_PERIOD) %>% 
  summarize_at(c("OBS_VALUE"),~sum(.,na.rm=TRUE)) %>% mutate(IPCC_annex="Non-Annex_I",
  C_group_IM24_sh="Rest of the World",Country_code_A3="FIGW1",Name="RoW",Substance="CO2",fossil_bio="fossil",
  cou="FIGW1") %>% select(all_of(names(edgar_air_emissions_inventories_ghg))))
  
  #### [a] create transformation matrix from CRF/NFR to NACE using average of EU countries
  #### and applying a RAS to the column and row totals upscale to match level since they 
  #### differ in the residence/territory principle
  #### ==> upscale the inventory totals to preserve dimension of resident emissions
  accounts_nuts1_code <- edgar_air_emissions_inventories_ghg %>% distinct(ipcc_code_2006_for_standard_report) %>% 
  mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_to_lower(stringr::str_remove_all(.,fixed(".")))) %>% 
  mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_sub(.,1,3)) %>% 
  mutate(ipcc_code_v2 =stringr::str_sub(ipcc_code_2006_for_standard_report,1,2)) %>%
  pivot_longer(`ipcc_code_2006_for_standard_report`:`ipcc_code_v2`,names_to = "column",
  values_to = "ipcc_code") %>% select(-column) %>% distinct(ipcc_code)
  
  nfr_crf_nace_r2_match <- nfr_crf_nace_r2 %>% mutate_at(c("code_ipcc"),~stringr::str_to_sentence(gsub('\\.', '',.))) %>% 
  mutate(code_ipcc_v1 = stringr::str_sub(code_ipcc,1,7)) %>% mutate(code_ipcc_v2 = stringr::str_sub(code_ipcc,1,5)) %>% 
  mutate(code_ipcc_v3 = stringr::str_sub(code_ipcc,1,3)) %>% mutate(code_ipcc_v4 = stringr::str_sub(code_ipcc,1,2)) %>% 
  pivot_longer(`code_ipcc_v1`:`code_ipcc_v4`,names_to = "vector",values_to = "code_match") %>% 
  filter(code_match%in%c(as.vector(unlist(accounts_nuts1_code)))) %>% select(-code_main:-label_main)
  
  #### [1] juntar la distribución en CO2 de las cuentas con los inventarios, pero EDGAR CO2, no GHG
  edgar_nace_r2_country <- data.frame()
  for(cou_n in as.vector(unlist(intensity_factors_co2 %>% filter(year%in%c(years)&is.na(ktGHGE)) %>% 
    separate(code,c("cou","code"),sep = "_",extra = "merge") %>% distinct(cou)))){
    cat(paste0("\n",cou_n,": "))
    edgar_nace_r2_year <- data.frame()
    for(year_n in years){
      #### [1] create bridge matrix from CRF to NACE (normalized by CRF)
      bridge_crf_nace_r2 <- nfr_crf_nace_r2_match %>% left_join(code_nace_r2 %>% distinct(letter,nace_3d,nace_r2,industry),by=c("code_nace_r2"="nace_3d")) %>% 
      distinct(code_match,nace_r2) %>% filter(!is.na(nace_r2)) %>% left_join(edgar_air_emissions_inventories_co2 %>% 
      filter(TIME_PERIOD==year_n&cou==cou_n) %>% distinct(cou,TIME_PERIOD,ipcc_code_2006_for_standard_report,OBS_VALUE) %>% 
      mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_to_lower(stringr::str_remove_all(.,fixed(".")))) %>% 
      mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_sub(.,1,3)) %>% 
      mutate_at(c("ipcc_code_2006_for_standard_report"),~if_else(.%in%c("3c2","3c3"),"3c",.)) %>% 
      group_by(ipcc_code_2006_for_standard_report) %>% summarise_at(c("OBS_VALUE"),~sum(.,na.rm=TRUE)) %>% 
      mutate(TOT_INV=sum(OBS_VALUE,na.rm=TRUE)),by=c("code_match"="ipcc_code_2006_for_standard_report")) %>% 
      left_join(intensity_factors_co2 %>% select(year,code,kt_CO2F) %>% separate(code,c("cou","code"),sep = "_",extra = "merge") %>% 
      filter(year==year_n&cou==cou_n) %>% mutate(TOT_ACC=sum(kt_CO2F,na.rm=TRUE)) %>% select(-year,-cou),
      by=c("nace_r2"="code")) %>% mutate(scale_factor = TOT_INV/TOT_ACC) %>% select(-TOT_ACC,-TOT_INV) %>% 
      rename("inventory_co2"="OBS_VALUE") %>% left_join(edgar_air_emissions_inventories_ghg %>% 
      filter(TIME_PERIOD==year_n&cou==cou_n) %>% distinct(cou,TIME_PERIOD,ipcc_code_2006_for_standard_report,OBS_VALUE) %>% 
      mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_to_lower(stringr::str_remove_all(.,fixed(".")))) %>% 
      mutate_at(c("ipcc_code_2006_for_standard_report"),~stringr::str_sub(.,1,3)) %>% 
      mutate_at(c("ipcc_code_2006_for_standard_report"),~if_else(.%in%c("3c2","3c3"),"3c",.)) %>% 
      group_by(ipcc_code_2006_for_standard_report) %>% summarise_at(c("OBS_VALUE"),~sum(.,na.rm=TRUE)) %>% 
      mutate(TOT_INV=sum(OBS_VALUE,na.rm=TRUE)),by=c("code_match"="ipcc_code_2006_for_standard_report")) %>%
      mutate_at(c("scale_factor"),~if_else(is.na(.),max(.,na.rm=TRUE),.)) %>%
      mutate_at(c("OBS_VALUE","inventory_co2"),~./scale_factor) %>% 
      rename("inventory_ghg"="OBS_VALUE","accounts"="kt_CO2F") %>% 
      group_by(code_match) %>% mutate_at(c("accounts"),~./sum(.,na.rm=TRUE)) %>% 
      ungroup %>% mutate_at(c("inventory_ghg"),~.-inventory_co2) %>%
      filter(!is.na(inventory_ghg))
      #### [ATTENTION] serious problems allocating no-combustion activities to HH_OTH
      #### results allocate 46% of total emissions to this sector to households other
      
      #### [2]
      bridge_crf_nace_r2 %<>% pivot_wider(names_from = "nace_r2",values_from = "accounts") %>% arrange(code_match) %>%
      mutate_at(c(as.vector(unlist(code_indus64 %>% filter(type=="sector") %>% select(nace_64)))),~.*inventory_ghg) %>%
      summarize_at(c(as.vector(unlist(code_indus64 %>% filter(type=="sector") %>% select(nace_64)))),~sum(.,na.rm=TRUE)) %>%
      mutate(cou=cou_n,year=year_n) %>% pivot_longer(`A01`:`U`,names_to = "code",values_to = "obs_value") %>%
      mutate_at(c("obs_value"),~if_else(is.na(.)|is.nan(.),0,.)) %>% mutate_at(c("obs_value"),~if_else(is.infinite(.),0,.)) %>% 
      left_join(intensity_factors_co2 %>% separate(code,c("cou","code"),sep = "_",extra = "merge") %>% 
      filter(year==year_n&cou==cou_n) %>% select(cou,year,code,kt_CO2F),by=c("cou","year","code")) %>% 
      mutate_at(c("obs_value"),~kt_CO2F+.)
      edgar_nace_r2_year %<>% bind_rows(bridge_crf_nace_r2)
      cat(paste0(year_n,", "))
    }
  cat(paste0("Finished"))
  edgar_nace_r2_country %<>% bind_rows(edgar_nace_r2_year)
  }
  saveRDS(edgar_nace_r2_country,file.path(getwd(),"datasets","edgar_ghg_accounts.rds"))
  
  intensity_factors_ghg <- intensity_factors_co2 %>% filter(year%in%c(years)) %>% left_join(edgar_nace_r2_country %>% 
  unite(code,"cou","code",sep="_"),by=c("code","year","kt_CO2F")) %>% rename("ktGHGF"="obs_value") %>% 
  mutate(cou_miss = if_else(is.na(ktGHGE),"Y","N")) %>% filter(year%in%c(years)) %>%
  mutate_at(c("ktGHGE"),~if_else(is.na(ktGHGE),ktGHGF,.)) %>% mutate_at(c("kg_euro_CO2"),~kt_CO2F/P1) %>% 
  mutate_at(c("kg_euro_GHG"),~ktGHGF/P1) %>% rename("ktCO2F"="kt_CO2F")
  intensity_factors_ghg %<>% mutate_at(c("ktGHGF"),~if_else(is.na(.),ktGHGE,.))
  
  #### [ATTENTION] there are a few rare cases where GHG is lower than CO2 [CH, NO in 3 sectors each]
  #### this is not coming from my data but Eurostat's
  # intensity_factors_ghg %>% separate(code,c("cou","code"),sep = "_",extra = "merge") %>%
  # filter(!grepl("U",code)&!grepl("T",code)) %>%
  # filter(kg_euro_GHG<kg_euro_CO2) %>% distinct(cou,code) %>% print(n=71)
  
  if(as.vector(unlist(intensity_factors_ghg %>% filter(kg_euro_GHG<kg_euro_CO2) %>% nrow))>0){
  print(paste0("Some GHG factors are smaller than CO2 factors: revision advised"))} else {
  print(paste0("All GHG factors are smaller than CO2 factors"))}
  
  saveRDS(intensity_factors_ghg,file.path(getwd(),"datasets","intensity_factors_ghg.rds"))
} else {
  intensity_factors_ghg <- readRDS(file.path(getwd(),"datasets","intensity_factors_ghg.rds")) %>% as_tibble
}
##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################