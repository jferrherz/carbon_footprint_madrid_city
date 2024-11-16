##################################################################################################################################
## REFERENCE FILES
##################################################################################################################################
  
  ######### [LEGACY]
  #### [A] CLASSIFICATIONS - Supply and Use FIGARO tables
  ##############################################################
  code_fiot64 <- readxl::read_xlsx(file.path(path_figaro,"Description-FIGARO-tables.xlsx"),skip = 10, col_names = TRUE, sheet = "Codes")
  cout_fiot64 <- readxl::read_xlsx(file.path(path_figaro,"Description-FIGARO-tables.xlsx"),skip = 10, col_names = TRUE, sheet = "Countries") %>%
  unite(`non FIGARO countries`,`...4`,c("non FIGARO countries","...4")) %>% unite(`FIGARO countries`,`...2`,c("FIGARO countries","...2")) %>%
  gather(key,value) %>% mutate(figaro = if_else((key=="non FIGARO countries"|key=="...4"),"no","yes")) %>% 
  separate(value,c("country","cou"), sep = "_") %>% dplyr::select(-key) %>% filter(!cou=="NA") %>%
  mutate(cou = if_else(cou=="EL","GR",cou)) %>% mutate(cou = if_else(cou=="UK","GB",cou))
  
  ind_ifiot64 <- readxl::read_xlsx(file.path(path_figaro,"Description-FIGARO-tables.xlsx"), 
  skip = 10, col_names = TRUE, sheet = "IndustriesDetailsFIGAROCountry") %>%
  set_names(c("Code","64 breakdown for FIGARO countries","Code","21 breakdown for FIGARO countries,
  projected years","Relation")) %>% dplyr::select(1:2) %>% set_names(c("code","industry"))
  ind_nfiot64 <- readxl::read_xlsx(file.path(path_figaro,"Description-FIGARO-tables.xlsx"), 
  skip = 10, col_names = TRUE, sheet = "IndutriesDetailsnonFIGARO") %>%
  set_names(c("Code","30 breakdown for non FIGARO countries","Code","17 breakdown for non FIGARO countries, 
  projected years")) %>% dplyr::select(1:2) %>% set_names(c("code","industry"))
  
  #################### 
  #### [B] CLASSIFICATIONS - FIGARO
  #########################################################
  code_indus64 <- readxl::read_xlsx(file.path(getwd(),"datasets","industry_classification.xlsx"),col_names = TRUE,sheet = "industries")
  code_produ64 <- readxl::read_xlsx(file.path(getwd(),"datasets","industry_classification.xlsx"),col_names = TRUE,sheet = "products")
  cou_eu       <- cout_fiot64 %>% filter(figaro=="yes"&cou!="US") %>% dplyr::select(cou) %>% unlist
  countries    <- cout_fiot64 %>% dplyr::select(cou)%>% unlist
  
  #### [INDUSTRIES] ####
  code_nace_r2 <- readxl::read_xlsx(file.path(database_path,"FIGARO","nace_r2_classification.xlsx"), col_names = TRUE,
  sheet = "nace_r2") %>% separate(code,c("code","name"),sep=" - ",extra = "merge") %>% mutate(letter = stringr::str_sub(code,1,1)) %>%
  mutate_at(c("code"),~if_else(letter!="H",stringr::str_to_sentence(gsub('\\.', '',.)),.)) %>% mutate(digits=nchar(code)) %>%
  mutate_at(c("digits"),~if_else(letter=="H",3,.)) %>% mutate(nace_3d = stringr::str_sub(code,2,3)) %>% filter((code=="HH"|digits==3)) %>% mutate_at(c("nace_3d"),~if_else(letter%in%c("A","B"),
  stringr::str_sub(.,1,1),.)) %>% mutate_at(c("letter"),~if_else(nace_3d=="H_","HH",.)) %>%
  mutate_at(c("nace_3d"),~if_else(letter=="HH","100",.)) %>% mutate_at(c("nace_3d"),
  ~if_else(letter!="H",str_pad(.,2,side = "left",pad = "0"),.))
  
  code_nace_r2 %<>% left_join(code_indus64 %>% filter(type=="sector") %>% distinct(nace_64) %>% mutate(nace_r2= nace_64) %>%
  separate(nace_64,c("start","end"),sep="_",extra = "merge") %>% pivot_longer(`start`:`end`,names_to = "nace_location",
  values_to = "nace_64") %>% filter(!is.na(nace_64)) %>% mutate(letter = stringr::str_sub(nace_64,1,1)) %>% 
  mutate(nace_3d = stringr::str_sub(nace_64,2,3)) %>% select(-nace_location) %>% full_join(code_nace_r2 %>% 
  distinct(letter,nace_3d),by=c("nace_3d","letter")) %>% arrange(letter,nace_3d) %>% mutate_at(c("nace_r2","nace_64"),~
  if_else(is.na(.)&letter!="HH",zoo::na.locf(.,na.rm = TRUE),.)) %>% select(-nace_64),by=c("nace_3d","letter")) %>% 
  left_join(code_indus64 %>%filter(type=="sector") %>% distinct(nace_64,industry),by=c("nace_r2"="nace_64")) %>% 
  select(nace_r2,letter,nace_3d,industry,code,name) %>% mutate_at(c("nace_r2","nace_3d"),~if_else(letter=="HH",
  code,.)) %>% mutate_at(c("industry"),~if_else(is.na(.)&letter=="HH",name,.))
  
  #### [A10 INDUSTRY NAMES]
  code_industries <- readxl::read_xlsx(file.path(getwd(),"datasets","Country_Codes_and_Names.xlsx"),sheet = "nace_r2_d2")
  
  #### [COUNTRIES] ####
  code_countries <- readxl::read_xlsx(file.path(getwd(),"datasets","Country_Codes_and_Names.xlsx"),sheet = 1) %>%
  mutate(cou=CODE) %>% mutate_at(c("cou"),~if_else(.=="CN_X_HK","CN",.)) %>% mutate_at(c("AREA"),~if_else(.=="ES","Rest of Spain",.)) %>%
  bind_rows(data.frame(AREA="Madrid city",CODE="ES30",cou="ES30",`NAME`="Madrid city")) %>% 
  mutate_at(c("NAME"),~if_else(is.na(.),cou,.)) %>% mutate(ref_area=case_when(AREA=="European Union (EU)" ~ "European Union",
  AREA=="EU candidate countries" ~ "Rest of the World",AREA=="European Free Trade Association (EFTA)" ~ "Rest of the World",
  AREA=="Other European countries" ~ "Rest of the World",AREA=="EU candidate countries" ~ "Rest of the World",
  AREA=="Non-European countries" ~ "Rest of the World")) %>% mutate_at(c("ref_area"),~if_else(is.na(.),AREA,.)) %>% 
  mutate_at(c("ref_area"),~if_else(cou=="ES","Rest of Spain",.)) %>%
  mutate_at(c("NAME"),~if_else(CODE=="DE","Germany",.))
  
  #pefasu_codes <- readxl::read_xlsx(file.path(getwd(),"datasets","pefa_energy_info.xlsx"),col_names = TRUE,sheet = "prod_nrg")
  
  #################### 
  #### [C] CLASSIFICATIONS - REgion/City of Madrid
  #########################################################
  #### source: https://www.madrid.org/iestadis/fijas/estructu/economicas/contabilidad/estructumio.htm
  #### cross-classification of regional SUTs and National Accounts
  code_industries <- readxl::read_xlsx(file.path(getwd(),"datasets","Country_Codes_and_Names.xlsx"),sheet = "nace_r2_d2")
  code_iots_mad  <- readxl::read_xlsx(file.path(getwd(),"datasets","classification_iots_mad.xlsx"),sheet = "Tabla")
  code_iots_city <- readxl::read_xlsx(file.path(getwd(),"datasets","classification_iots_mad.xlsx"),sheet = "City") %>%
  filter(digits_full>2)
  
  code_coicop_d2 <- readxl::read_xlsx(file.path(getwd(),"datasets","epf_enlace_codigos_b9722.xlsx"),sheet = "Enlace_series (2d)")
  code_coicop_d3 <- readxl::read_xlsx(file.path(getwd(),"datasets","epf_enlace_codigos_b9722.xlsx"),sheet = "Enlace_series (3d)")
    
  #################### 
  # [CO2] Linkage betweenb energy and CO2 accounts
  #########################################################
  # factores_ghg <- readxl::read_xlsx(file.path(getwd(),"datasets","pefa_energy_info.xlsx"),
  # col_names = TRUE,sheet = "ghg_factor") %>% left_join( readxl::read_xlsx(file.path(getwd(),
  # "datasets","pefa_energy_info.xlsx"),col_names = TRUE,sheet = "ghg_2017_factor",skip = 1) %>%
  # select(GHG,Fuel,`kg/TJ`),by=c("gases"="GHG","ipcc_labels"="Fuel")) %>% filter(!is.na(`kg/TJ`)) %>%
  # mutate_at(c("kg/TJ"),~.*co2eq)

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################