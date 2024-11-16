##################################################################################################################################
## Block C - DETAILED RESULTS
##################################################################################################################################
if(sum(objects()=="budget_survey_hh_city_GHG")==0){budget_survey_hh_city_GHG <- readRDS(file.path(getwd(),
"datasets","budget_survey_hh_city_GHG.rds"))}
if(sum(objects()=="budget_survey_hh_city")==0){budget_survey_hh_city <- readRDS(file.path(getwd(),
"datasets","budget_survey_hh_city.rds"))}
budget_survey_hh_city_GHG %<>% mutate_at(c("REGTEN"),~case_when(.%in%c(1)~"Owner (full)",
.%in%c(2)~"Owner (partial)",.%in%c(3:4)~"Renteer",.%in%c(5:6)~"Free user"))
  
  #################### 
  # [A] FIGURES
  #########################################################
  library(latex2exp);library(RColorBrewer)
  #### [a] plot of spending related emissions
  temp_plot_coicop <- city_emissions_coicop %>% filter(year==year_end_city) %>% pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",
  values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
  left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,2)) %>%group_by(cou,code,ref_area,year) %>%
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(code_coicop_d2,by=c("code"="coicop_g2d")) %>%
  rename("heading"="coicop_g2d_heading_short") %>% rename("P3_S14"="obs_value") %>% ungroup()
  
    temp_plot_coicop %>% ggplot(aes(x = P3_S14, y = reorder(heading,P3_S14),fill=
    factor(ref_area,levels=c(rev(unique(ref_area)[c(3,4,2,6,1,5)]))))) + 
    geom_bar(stat="identity") + scale_fill_brewer(palette="YlOrRd") + labs(fill='') + ylab("COICOP groups (2 digits)") + 
    xlab(TeX("kt$\\CO_2$")) + guides(fill=guide_legend(nrow=2)) +
    theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "bottom")
  
  ggsave(file.path(getwd(),"paper","graphics",paste0("emisiones_consumption_madrid_area_ES30_",
  year_end_city,".pdf")),height = 5,width = 10)
  
  #################### 
  # [B] ANALYSIS
  #########################################################
  #### [a] geographica distribution of emissions
  library(kableExtra)
  temp_plot_coicop <- city_emissions_coicop %>% pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",
  values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
  left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,2)) %>%group_by(cou,code,ref_area,year) %>%
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(code_coicop_d2,by=c("code"="coicop_g2d")) %>%
  rename("heading"="coicop_g2d_heading_short") %>% rename("P3_S14"="obs_value") %>% ungroup()
  
  CAGR_formula <- function(FV,PV,yrs) {values <- ((FV/PV)^(1/yrs)-1);return(values)}
  table_coicop <- temp_plot_coicop %>% group_by(code,heading,year) %>%  summarise_at(c("P3_S14"),~sum(.,na.rm=TRUE)) %>% ungroup %>%
  left_join(budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% group_by(ANOENC,CODIGOd2) %>%
  summarise_at(c("GASTO"),~sum(.*FACTOR,na.rm=TRUE)/1e06),by=c("year"="ANOENC","code"="CODIGOd2")) %>%
  mutate(Factor=P3_S14/GASTO) %>% rename("Emissions"="P3_S14","Spending"="GASTO","Heading"="heading","COICOP"="code") %>% 
  group_by(year) %>% mutate_at(c("Emissions","Spending"),~.*100/sum(.,na.rm=TRUE)) %>% ungroup %>%
  mutate_at(c("Emissions","Spending"),~round(.,1)) %>%  mutate_at(c("Factor"),~round(.,3))
  
  library(kableExtra)
  table_coicop %>% left_join(table_coicop %>% select(COICOP:year,Factor) %>% 
  pivot_wider(names_from = "year",values_from = "Factor") %>%
  mutate(Growth=CAGR_formula(!!sym(paste0(year_end_city)),!!sym(paste0(year_str_io)),length(seq(year_str_io,year_end_city))),
  check = !!sym(paste0(year_str_io)) * (1 + Growth) ^ length(seq(year_str_io,year_end_city))) %>% select(-check) %>%
  pivot_longer(!!sym(paste0(year_str_io)):!!sym(paste0(year_end_city)),names_to = "year",values_to = "factor") %>% 
  mutate_at(c("year"),~as.double(.)) %>% select(-factor),by=c("COICOP",
  "Heading","year")) %>% mutate_at(c("Growth"),~round(.*100,1)) %>%
  filter(year==year_end_city) %>% select(-year) %>% rename("code"="COICOP") %>%
  mutate_at(c("Heading"),~if_else(code=="05","Furnishings",.)) %>%
  mutate_at(c("Heading"),~if_else(code=="11","Restaurants & Hotels",.)) %>%
  mutate_at(c("Heading"),~paste0(code," ",Heading)) %>% select(-code) %>% 
  rename("ktCO_2"="Emissions","Ratio"="Factor","Expenditure category"="Heading") %>%
  select(`Expenditure category`,`Ratio`,everything()) %>%
  kbl(booktabs = T,"latex") %>% kable_styling(latex_options = c("hold_position"),font_size = 8)
  
  temp_3d_coicop <- city_emissions_coicop %>% pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",
  values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
  left_join(code_countries %>% mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(cou,code,ref_area,year) %>%
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(code_coicop_d3 %>% 
  select(coicop_g3d,ecoicop_g3d_heading),by=c("code"="coicop_g3d")) %>%
  rename("heading"="ecoicop_g3d_heading") %>% rename("P3_S14"="obs_value") %>% ungroup()
  
  table_3d_coicop <- temp_3d_coicop %>% group_by(code,heading,year) %>%  summarise_at(c("P3_S14"),
  ~sum(.,na.rm=TRUE)) %>% ungroup %>% left_join(budget_survey_hh_city %>% 
  filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% group_by(ANOENC,CODIGOd3) %>%
  summarise_at(c("GASTO"),~sum(.*FACTOR,na.rm=TRUE)/1e06),by=c("year"="ANOENC","code"="CODIGOd3")) %>%
  mutate(Factor=P3_S14/GASTO) %>% rename("Emissions"="P3_S14","Spending"="GASTO",
  "Heading"="heading","COICOP"="code") %>%  group_by(year) %>% mutate_at(c("Emissions","Spending"),
  ~.*100/sum(.,na.rm=TRUE)) %>% ungroup %>% mutate_at(c("Emissions","Spending"),~round(.,1)) %>%  
  mutate_at(c("Factor"),~round(.,3))
  
  table_3d_coicop %<>% left_join(table_3d_coicop %>% select(COICOP:year,Factor) %>% 
  pivot_wider(names_from = "year",values_from = "Factor") %>%
  mutate(Growth=CAGR_formula(!!sym(paste0(year_end_city)),!!sym(paste0(year_str_io)),length(seq(year_str_io,year_end_city))),
  check = !!sym(paste0(year_str_io)) * (1 + Growth) ^ length(seq(year_str_io,year_end_city))) %>% select(-check) %>%
  pivot_longer(!!sym(paste0(year_str_io)):!!sym(paste0(year_end_city)),names_to = "year",values_to = "factor") %>% 
  mutate_at(c("year"),~as.double(.)) %>% select(-factor),by=c("COICOP",
  "Heading","year")) %>% mutate_at(c("Growth"),~round(.*100,1))
  
  kgGHG_euro_gdp_factor <- city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>%  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% filter(cou=="ES30") %>% 
  select(cou,code,obs_value,year) %>% group_by(cou,code,year) %>% summarize_at(c("obs_value"),
  ~sum(.,na.rm=TRUE)) %>% left_join(city_mrio_industry_final %>% 
  separate(code,c("cou","code"),sep="_",extra="merge") %>% 
  filter(code%in%c(as.vector(unlist(code_iots_city %>% 
  filter(type=="sector") %>% distinct(nace_code))))) %>% select(cou,code,GDP,year) %>% 
  filter(cou=="ES30") %>% mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% group_by(cou,code,year) %>% summarize_at(c("GDP"),~sum(.,na.rm=TRUE)) %>% mutate_at(c("year"),~as.character(.)),
  by=c("cou","code","year")) %>% mutate(kgGHGgdp=obs_value/GDP) %>% select(cou:year,kgGHGgdp) %>% ungroup %>% 
  pivot_wider(names_from = "year",values_from = "kgGHGgdp") %>% mutate(`2010_b`=!!sym(paste0(year_str_io))) %>% 
  mutate_at(c(as.character(year_str_io,year_end_city)),~./`2010_b`) %>% select(-`2010_b`) %>%
  mutate(type=if_else(!!sym(paste0(year_end_city))>0.85,"stagnant","progressive")) %>% filter(!code=="T") %>% 
  mutate(growth=(!!sym(paste0(year_end_city))-!!sym(paste0(year_str_io)))/!!sym(paste0(year_str_io))) %>% mutate_at(c("code"),~if_else(code=="B","B_E",.)) 
    
  #################### 
  # [C] TABLES
  #########################################################
  #### [a] average income by group
  table_full_co2 <- budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  group_by(ANOENC,QUNTLG) %>% summarise_at(c("IMPEXACEQV"),~weighted.mean(.,FACTOR,na.rm=TRUE)  %>% round(0)) %>% 
  set_names(c("year","group","Income")) %>% mutate_at(c("group"),~paste0("Q",.)) %>% pivot_wider(names_from = "group",values_from = "Income") %>%
  left_join(budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  group_by(ANOENC,EDADG) %>% summarise_at(c("IMPEXACEQV"),~weighted.mean(.,FACTOR,na.rm=TRUE)  %>% round(0)) %>% 
  set_names(c("year","group","Income")) %>% pivot_wider(names_from = "group",values_from = "Income"),by=c("year")) %>%
  left_join(budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  group_by(ANOENC,ESTUDIOG) %>% summarise_at(c("IMPEXACEQV"),~weighted.mean(.,FACTOR,na.rm=TRUE)  %>% round(0)) %>% 
  set_names(c("year","group","Income")) %>% filter(!is.na(group)) %>% pivot_wider(names_from = "group",values_from = "Income"),by=c("year")) %>%
  left_join(budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  group_by(ANOENC,SEXOG) %>% summarise_at(c("IMPEXACEQV"),~weighted.mean(.,FACTOR,na.rm=TRUE)  %>% round(0)) %>% 
  set_names(c("year","group","Income")) %>% pivot_wider(names_from = "group",values_from = "Income"),by=c("year")) %>%
  left_join(budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  group_by(ANOENC,REGTEN) %>% summarise_at(c("IMPEXACEQV"),~weighted.mean(.,FACTOR,na.rm=TRUE)  %>% round(0)) %>% 
  set_names(c("year","group","Income")) %>% pivot_wider(names_from = "group",values_from = "Income"),by=c("year")) %>%
  left_join(budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  group_by(ANOENC) %>% summarise_at(c("IMPEXACEQV"),~weighted.mean(.,FACTOR,na.rm=TRUE)  %>% round(0)) %>% set_names(c("year","Total")),by=c("year")) %>%
  mutate(COICOP="Equivalized disposable income") %>%
              
  #### [b] averages by group
  bind_rows(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,.keep_all = TRUE) %>%        
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% group_by(ANOENC,QUNTLG) %>%
  summarise_at(c("kgGHGTOT"),~weighted.mean(./UC2,FACTOR,na.rm=TRUE) %>% round(1)) %>% set_names(c("year","group","kgGHG")) %>%
  mutate_at(c("group"),~paste0("Q",.)) %>% pivot_wider(names_from = "group",values_from = "kgGHG") %>%
  left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,.keep_all = TRUE) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% group_by(ANOENC,EDADG) %>%
  summarise_at(c("kgGHGTOT"),~weighted.mean(./UC2,FACTOR,na.rm=TRUE) %>% round(1)) %>% set_names(c("year","group","kgGHG")) %>%
  pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year")) %>%
  left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,.keep_all = TRUE) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% group_by(ANOENC,ESTUDIOG) %>% filter(!is.na(ESTUDIOG)) %>%
  summarise_at(c("kgGHGTOT"),~weighted.mean(./UC2,FACTOR,na.rm=TRUE) %>% round(1)) %>% set_names(c("year","group","kgGHG")) %>%
  pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year")) %>%
  left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,.keep_all = TRUE) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% group_by(ANOENC,SEXOG) %>% filter(!is.na(SEXOG)) %>%
  summarise_at(c("kgGHGTOT"),~weighted.mean(./UC2,FACTOR,na.rm=TRUE) %>% round(1)) %>% set_names(c("year","group","kgGHG")) %>%
  pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year")) %>% 
  left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,.keep_all = TRUE) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% group_by(ANOENC,REGTEN) %>% filter(!is.na(SEXOG)) %>%
  summarise_at(c("kgGHGTOT"),~weighted.mean(./UC2,FACTOR,na.rm=TRUE) %>% round(1)) %>% set_names(c("year","group","kgGHG")) %>%
  pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year")) %>% 
  left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,.keep_all = TRUE) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% group_by(ANOENC) %>%
  summarise_at(c("kgGHGTOT"),~weighted.mean(./UC2,FACTOR,na.rm=TRUE) %>% round(1)) %>% set_names(c("year","Total")),
  by=c("year")) %>% mutate(COICOP="Average kgGHG")) %>% 
  
  #### [c] averages by group and COICOP (2 digits)
  bind_rows(budget_survey_hh_city_GHG %>% group_by(ANOENC,QUNTLG,CODIGOd2) %>% 
  summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
  mutate_at(c("group"),~paste0("Q",.)) %>% pivot_wider(names_from = "group",values_from = "kgGHG") %>%
  left_join(budget_survey_hh_city_GHG %>% group_by(ANOENC,EDADG,CODIGOd2) %>% 
  summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
  pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year","COICOP")) %>%
  left_join(budget_survey_hh_city_GHG %>% group_by(ANOENC,ESTUDIOG,CODIGOd2) %>% 
  summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% filter(!is.na(ESTUDIOG)) %>%
  set_names(c("year","group","COICOP","kgGHG")) %>% pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year","COICOP")) %>% 
  left_join(budget_survey_hh_city_GHG %>% group_by(ANOENC,SEXOG,CODIGOd2) %>% 
  summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
  pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year","COICOP")) %>% 
  left_join(budget_survey_hh_city_GHG %>% group_by(ANOENC,REGTEN,CODIGOd2) %>% 
  summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
  pivot_wider(names_from = "group",values_from = "kgGHG"),by=c("year","COICOP")) %>% 
  left_join(budget_survey_hh_city_GHG %>% group_by(ANOENC,CODIGOd2) %>% 
  summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>%
  set_names(c("year","COICOP","Total")),by=c("year","COICOP"))) %>% 
  
  #### [d] direct emissions
  bind_rows(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,QUNTLG,HH_HEAT,HH_TRA,HH_OTH) %>%
  pivot_longer(`HH_HEAT`:`HH_OTH`,names_to = "HH",values_to = "kgGHG") %>% group_by(ANOENC,QUNTLG,HH) %>% 
  summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
  mutate_at(c("group"),~paste0("Q",.)) %>% pivot_wider(names_from = "group",values_from = "kgGHG") %>%
  mutate_at(c("COICOP"),~case_when(.=="HH_HEAT"~"Heating",.=="HH_TRA"~"Transport",.=="HH_OTH"~"Other")) %>%
    
    left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,EDADG,HH_HEAT,HH_TRA,HH_OTH) %>%
    pivot_longer(`HH_HEAT`:`HH_OTH`,names_to = "HH",values_to = "kgGHG") %>% group_by(ANOENC,EDADG,HH) %>% 
    summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
    pivot_wider(names_from = "group",values_from = "kgGHG") %>%
    mutate_at(c("COICOP"),~case_when(.=="HH_HEAT"~"Heating",.=="HH_TRA"~"Transport",.=="HH_OTH"~"Other")),by=c("year","COICOP")) %>%
    
    left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,ESTUDIOG,HH_HEAT,HH_TRA,HH_OTH) %>%
    pivot_longer(`HH_HEAT`:`HH_OTH`,names_to = "HH",values_to = "kgGHG") %>% group_by(ANOENC,ESTUDIOG,HH) %>% 
    summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
    filter(!is.na(group)) %>% pivot_wider(names_from = "group",values_from = "kgGHG") %>%
    mutate_at(c("COICOP"),~case_when(.=="HH_HEAT"~"Heating",.=="HH_TRA"~"Transport",.=="HH_OTH"~"Other")),by=c("year","COICOP")) %>%
    
    left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,SEXOG,HH_HEAT,HH_TRA,HH_OTH) %>%
    pivot_longer(`HH_HEAT`:`HH_OTH`,names_to = "HH",values_to = "kgGHG") %>% group_by(ANOENC,SEXOG,HH) %>% 
    summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
    pivot_wider(names_from = "group",values_from = "kgGHG") %>%
    mutate_at(c("COICOP"),~case_when(.=="HH_HEAT"~"Heating",.=="HH_TRA"~"Transport",.=="HH_OTH"~"Other")),by=c("year","COICOP")) %>%
    
    left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,REGTEN,HH_HEAT,HH_TRA,HH_OTH) %>%
    pivot_longer(`HH_HEAT`:`HH_OTH`,names_to = "HH",values_to = "kgGHG") %>% group_by(ANOENC,REGTEN,HH) %>% 
    summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% set_names(c("year","group","COICOP","kgGHG")) %>%
    pivot_wider(names_from = "group",values_from = "kgGHG") %>%
    mutate_at(c("COICOP"),~case_when(.=="HH_HEAT"~"Heating",.=="HH_TRA"~"Transport",.=="HH_OTH"~"Other")),by=c("year","COICOP")) %>%
  
    left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,UC2,NUMERO,FACTOR,HH_HEAT,HH_TRA,HH_OTH) %>%
    pivot_longer(`HH_HEAT`:`HH_OTH`,names_to = "HH",values_to = "kgGHG") %>% group_by(ANOENC,HH) %>% 
    summarise_at(c("kgGHG"),~round(weighted.mean(./UC2,FACTOR,na.rm=TRUE),1)) %>% 
    set_names(c("year","COICOP","Total")) %>% mutate_at(c("COICOP"),~case_when(.=="HH_HEAT"~"Heating",
    .=="HH_TRA"~"Transport",.=="HH_OTH"~"Other")),by=c("year","COICOP")) %>%
    
  arrange(year,factor(COICOP,levels=c("Heating","Transport","Other")))) %>% 
  filter(year==year_end_city) %>% ungroup %>%
  
  #### [e] intensity ratios
  # left_join(city_emissions_coicop %>% pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",
  # values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
  # mutate_at(c("code"),~stringr::str_sub(.,1,2)) %>%group_by(cou,code,year) %>%
  # summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("P3_S14"="obs_value") %>% left_join(code_coicop_d2 %>%
  # select(coicop_g2d,coicop_g2d_heading_short) %>% set_names(c("coicop_g2d","heading")),by=c("code"="coicop_g2d")) %>% 
  # group_by(code,heading,year) %>%  summarise_at(c("P3_S14"),~sum(.,na.rm=TRUE)) %>% ungroup %>%
  # left_join(budget_survey_hh_city %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% group_by(ANOENC,CODIGOd2) %>%
  # summarise_at(c("GASTO"),~sum(.*FACTOR,na.rm=TRUE)/1e06),by=c("year"="ANOENC","code"="CODIGOd2")) %>%
  # mutate(Factor=P3_S14/GASTO) %>% rename("Emissions"="P3_S14","Spending"="GASTO","Heading"="heading","COICOP"="code") %>% 
  # group_by(year) %>% mutate_at(c("Emissions","Spending"),~.*100/sum(.,na.rm=TRUE)) %>% ungroup %>%
  # mutate_at(c("Emissions","Spending"),~round(.,1)) %>%  mutate_at(c("Factor"),~round(.,3)) %>% select(-Heading),by=c("COICOP","year")) %>%
  
  #### [f] formatting
  left_join(code_coicop_d2 %>% distinct(coicop_g2d,coicop_g2d_heading_short),by=c("COICOP"="coicop_g2d")) %>% 
  mutate_at(c("coicop_g2d_heading_short"),~if_else(is.na(.),"",.)) %>% 
  mutate_at(c("COICOP"),~paste0(COICOP," ",coicop_g2d_heading_short)) %>% 
  select(COICOP,`Q1`:`Q5`,`15-34`:`+71`,everything(),-year) %>%
  mutate_at(c("COICOP"),~if_else(.=="Transport ","Transport activity",.)) %>%
  mutate_at(c("COICOP"),~if_else(.=="Heating ","Heating/cooling",.)) %>%
  mutate_at(c("coicop_g2d_heading_short"),~if_else(.=="",COICOP,.))
  
  table_full_co2 %>% select(COICOP,`Q1`:`Q5`,`15-34`:`+71`,`Total`) %>% 
  mutate_at(c("COICOP"),~if_else(.=="Equivalized disposable income ","Equivalized income",.))%>% 
  mutate_at(c("COICOP"),~if_else(.=="05 Furnishings & maintenance","05 Furnishings",.)) %>%
  mutate(across(where(is.numeric),~if_else(COICOP=="Other ",round(.,0),round(.,0)))) %>%
  kbl(booktabs = T,"latex") %>% kable_styling(latex_options = c("hold_position"),font_size = 9)
  
  #### [g] gini coefficients
  gini_series <- budget_survey_hh_city_GHG %>% distinct(ANOENC,NUMERO,IMPEXAC,IMPURENT,UC2,
  IMPEXACEQV,GASTOEQV,GASTOT,GASTOMONT,kgGHGTOT,QUNTLG,FACTOR) %>% group_by(ANOENC) %>% 
  mutate_at(c("IMPEXACEQV"),~(IMPEXAC+IMPURENT)/UC2) %>% mutate(GASTOEQV=(GASTOT-IMPURENT)/UC2) %>% 
  mutate(GASTOMEQV=GASTOMONT/UC2) %>% mutate(CARBONEQV=kgGHGTOT/UC2) %>% 
  mutate_at(c("GASTOEQV"),~if_else(is.na(.),0,.)) %>%
  summarise_at(c("IMPEXACEQV","GASTOEQV","GASTOMEQV","CARBONEQV"),~DescTools::Gini(.,FACTOR,unbiased=FALSE)) %>% 
  ungroup %>% set_names(c("year","gini_income","gini_expenditure","gini_monetary","gini_emissions")) %>% 
  pivot_longer(`gini_income`:`gini_emissions`,names_to="Gini",values_to="obs_value")  %>%
  filter(!Gini=="gini_expenditure") %>% filter(!Gini=="gini_income") %>%
  mutate_at(c("Gini"),~case_when(.=="gini_monetary"~"Monetary expenditure",
  .=="gini_emissions"~"kgCO2e"))
  gini_series %>%
    ggplot(aes(y = obs_value, x = year,group=Gini,color=Gini)) + 
     geom_line()  + labs(fill='') + ylab("") + scale_colour_brewer(palette="Dark2") +
     xlab("") + ylab(TeX("Gini coefficient")) + guides(fill=guide_legend(nrow=2)) +
     scale_x_discrete(breaks = seq(2010, 2021, by = 2)) + theme_gray() +
     theme(text = element_text(size = 25),axis.ticks.x=element_blank(),
     axis.ticks.y=element_blank(),legend.position = "bottom",legend.title = element_blank())
  
  budget_survey_hh_city_GHG %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`HH_OTH`),na.rm=TRUE)) %>% group_by(ANOENC) %>%
  summarise_at(c("kgGHGTOT"),~sum(.,na.rm=TRUE)/sum(FACTOR/1e03,na.rm=TRUE) %>% round(1)) %>% 
  set_names(c("year","Total"))
  
  city_acounts_madrid %>% filter(grepl("HH",nace_r2)) %>% 
  pivot_wider(names_from="nace_r2",values_from="ktCO2")
  
  budget_survey_hh_city_GHG %>% group_by(ANOENC) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  summarize_at(c("HH_HEAT","HH_TRA","HH_OTH"),~sum(.*FACTOR,na.rm=TRUE)/1e06) %>%
  mutate(TOT=rowSums(across(`HH_HEAT`:`HH_OTH`),na.rm=TRUE))
  
    #   coicop_reduction_top50_totc <- as.vector(unlist(readRDS(file.path(datasets_path,
    # "city_emissions_coicop_gva_sim.rds")) %>% as_tibble() %>% filter(shock==0.5) %>% 
    # group_by(code) %>% transmute(kgGHG_sim=rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>% 
    # ungroup %>% mutate(CODIGOd2=stringr::str_sub(code,1,2)) %>% 
    # summarise_at(c("kgGHG_sim"),~sum(.,na.rm=TRUE))))
  
##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################