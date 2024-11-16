##################################################################################################################################
## Block C - SIMULATION
##################################################################################################################################
  
#################### 
# [A] Importing municipal classification match
#########################################################
code_indus64 <- readxl::read_xlsx(file.path(getwd(),"datasets","industry_classification.xlsx"), col_names = TRUE,sheet = "industries")
code_produ64 <- readxl::read_xlsx(file.path(getwd(),"datasets","industry_classification.xlsx"), col_names = TRUE,sheet = "products")
code_countries <- readxl::read_xlsx(file.path(getwd(),"datasets","Country_Codes_and_Names.xlsx"),sheet = 1) %>%
mutate(cou=CODE) %>% mutate_at(c("cou"),~if_else(.=="CN_X_HK","CN",.)) %>% mutate_at(c("AREA"),~if_else(.=="ES","Rest of Spain",.)) %>%
bind_rows(data.frame(AREA="Madrid city",CODE="ES30",cou="ES30",`NAME`="Madrid city")) %>% 
mutate_at(c("NAME"),~if_else(is.na(.),cou,.)) %>% mutate(ref_area=case_when(AREA=="European Union (EU)" ~ "European Union",
AREA=="EU candidate countries" ~ "Rest of the World",AREA=="European Free Trade Association (EFTA)" ~ "Rest of the World",
AREA=="Other European countries" ~ "Rest of the World",AREA=="EU candidate countries" ~ "Rest of the World",
AREA=="Non-European countries" ~ "Rest of the World")) %>% mutate_at(c("ref_area"),~if_else(is.na(.),AREA,.)) %>% 
mutate_at(c("ref_area"),~if_else(cou=="ES","Rest of Spain",.))
code_coicop_d2 <- readxl::read_xlsx(file.path(getwd(),"datasets","epf_enlace_codigos_b9722.xlsx"),sheet = "Enlace_series (2d)")
code_coicop_d3 <- readxl::read_xlsx(file.path(getwd(),"datasets","epf_enlace_codigos_b9722.xlsx"),sheet = "Enlace_series (3d)")
code_iots_mad <- readxl::read_xlsx(file.path(getwd(),"datasets","classification_iots_mad.xlsx"),sheet = "Tabla")
code_iots_city <- readxl::read_xlsx(file.path(getwd(),"datasets","classification_iots_mad.xlsx"),sheet = "City") %>%
filter(digits_full>2)

    code_nace_r2 <- readxl::read_xlsx(file.path(database_path,"FIGARO","nace_r2_classification.xlsx"), col_names = TRUE,
    sheet = "nace_r2") %>% separate(code,c("code","name"),sep=" - ",extra = "merge") %>% mutate(letter = stringr::str_sub(code,1,1)) %>%
    mutate_at(c("code"),~if_else(letter!="H",stringr::str_to_sentence(gsub('\\.', '',.)),.)) %>% mutate(digits=nchar(code)) %>%
    mutate_at(c("digits"),~if_else(letter=="H",3,.)) %>% mutate(nace_3d = stringr::str_sub(code,2,3)) %>% filter((code=="HH"|digits==3)) %>% 
    mutate_at(c("nace_3d"),~if_else(letter%in%c("A","B"),stringr::str_sub(.,1,1),.)) %>% mutate_at(c("letter"),~if_else(nace_3d=="H_","HH",.)) %>%
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
    
#################### 
# [B] COMPUTE MATRICES C AND H
#########################################################
if(length(files_path_figaro[files_path_figaro=="matrix_c_set.rds"])==0){
  matrix_c_year <- data.frame();matrix_h_year <- data.frame()
  for(year_n in years){
    #### [A] building blocks
    #### [1] Compute the technical coefficients matrix (industry-by-industry)
    temp_matrix_z <- city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,`AR_A`:`ZA_T`) %>% unite(code,"cou","code",
    sep="_") %>% column_to_rownames("code") %>% as.matrix
    if(length(as.vector(colnames(temp_matrix_z)==nrow(temp_matrix_z)))==nrow(temp_matrix_z)){
    print(paste0(year_n,": The order of rows and columns MATCH")) } else {
    print(paste0(year_n,": The order of rows and columns DO NOT"))}
    
    temp_output <- city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,P1) %>% unite(code,"cou","code",
    sep="_") %>% mutate_at(c("P1"),~1/.) %>% mutate_at(c("P1"),~replace_na(.,0)) %>%
    column_to_rownames("code") %>% unlist %>% as.vector
    
    temp_matrix_a <- (temp_matrix_z%*%diag(temp_output)) %>% as.data.frame %>%
    mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.)) %>% set_rownames(c(rownames(temp_matrix_z))) %>%
    set_colnames(c(colnames(temp_matrix_z))) %>% as.matrix
    temp_matrix <- solve(diag(nrow(temp_matrix_a)) - temp_matrix_a)
    
    #### [2] P3_S14 final demand vector to gross up emisions
    temp_demand <- city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,!!sym(paste0(sda_vector))) %>%
    unite(code,"cou","code",sep="_") %>% arrange(factor(code,levels=c(colnames(temp_matrix))))
    temp_demand %<>% column_to_rownames("code") %>% unlist %>% as.vector
    
    #### [3] Compute the emissions vector
    emissn_vect <- city_mrio_env_accounts %>% filter(year==year_n) %>% ungroup %>%
    select(code,factor) %>% arrange(factor(code,levels=c(colnames(temp_matrix))))  %>% 
    mutate_at(c("factor"),~if_else(is.nan(.)|is.infinite(.),0,.))
    if(length(as.vector(unlist(emissn_vect$code))==colnames(temp_matrix))==nrow(temp_matrix)){
    print(paste0(year_n,": The order of country and codes MATCH between emissions vector and matrix")) } else {
    print(paste0(year_n,": The order of country and codes DO NOT match between emissions vector and matrix"))}
    emissn_vect %<>% column_to_rownames("code") %>% t %>% as.vector
    
    #### [4] total emissions
    temp_emissions <- diag(as.vector(emissn_vect))%*%as.matrix(temp_matrix)%*%diag(temp_demand) %>% 
    as.data.frame %>% set_names(c(as.vector(colnames(temp_matrix)))) %>% mutate(code = as.vector(colnames(temp_matrix))) %>% 
    select(code,everything()) %>% ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter",values_to="value") %>% unite(code,"cou","code",sep="_") %>%
    pivot_wider(names_from="code",values_from="value") %>% separate(counter,c("cou","code"),sep="_",extra="merge") %>%
    mutate(kgC02_eq = rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>% mutate(year=paste0(year_n)) #%>% select(cou,code,kgC02_eq)
    print(temp_emissions %>% group_by(cou) %>% filter(cou=="ES30") %>% mutate(kgC02_eq_city = 
    rowSums(across(`ES30_A`:`ES30_T`),na.rm=TRUE)) %>% summarize(sum(`kgC02_eq`),sum(`kgC02_eq_city`)) %>%
    mutate(shock=shock_n))
        
    #### [C] Structural decomposition - Next level decomposition of L and F [Xu and Dietzenbacher, 2014]
    temp_matrix_h <- temp_matrix_a %>% as.data.frame %>% rownames_to_column("code") %>% as_tibble %>%
    separate(code,c("cou","code"),sep="_",extra="merge") %>% select(cou,code) %>% left_join(temp_matrix_a %>% 
    as.data.frame %>% rownames_to_column("code") %>% as_tibble %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    group_by(code) %>% summarize_at(c(colnames(temp_matrix_a)),~sum(.,na.rm=TRUE)),by=c("code")) %>%
    unite(code,"cou","code",sep="_") %>% column_to_rownames("code") %>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    temp_matrix_c <- temp_matrix_a %>% as.data.frame %>% rownames_to_column("code") %>% as_tibble %>%
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter_code",values_to="obs_value") %>% left_join(temp_matrix_h %>% 
    as.data.frame %>% rownames_to_column("code") %>% as_tibble %>% pivot_longer(`AR_A`:`ZA_T`,names_to="counter_code",
    values_to="obs_value"),by=c("code","counter_code")) %>% rename("A"="obs_value.x","H"="obs_value.y") %>%
    mutate_at(c("A"),~./`H`) %>% select(-`H`) %>% pivot_wider(names_from = "counter_code",values_from = "A") %>%
    column_to_rownames("code") %>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    
    if(sum(hadamard.prod(as.matrix(temp_matrix_c),as.matrix(temp_matrix_h)))==sum(temp_matrix_a)){
    print(paste0("The technical coefficients matrix MATCH the Hadamard product of their components"))} else {
    print(paste0("The technical coefficients matrix DO NOT match the Hadamard product of their components"))}
    if(sum(temp_matrix)==sum(solve(diag(nrow(temp_matrix_h)) - hadamard.prod(as.matrix(temp_matrix_c),as.matrix(temp_matrix_h))))){
    print(paste0("The Leontief inverse MATCH the sum of the Hadamard product of their components"))} else {
    print(paste0("The Leontief inverse DO NOT match the sum of the Hadamard product of their components"))}
    
  matrix_c_year %<>% bind_rows(temp_matrix_c %>% rownames_to_column("code") %>% mutate(year=year_n))
  matrix_h_year %<>% bind_rows(temp_matrix_h %>% rownames_to_column("code") %>% mutate(year=year_n))
  } 
  saveRDS(matrix_c_year,file.path(getwd(),"datasets","matrix_c_set.rds"))
  saveRDS(matrix_h_year,file.path(getwd(),"datasets","matrix_h_set.rds"))
} else{
  matrix_c_set <- readRDS(file.path(getwd(),"datasets","matrix_c_set.rds")) %>% as_tibble()
  matrix_h_set <- readRDS(file.path(getwd(),"datasets","matrix_h_set.rds")) %>% as_tibble()
}
    
#################### 
# [C] SIMULATION OF CHANGES TO CONSUMPTION STRUCTURE
#########################################################
    
  #### [ OPTIONS ]
  all_cou      <- c(as.vector(unlist(matrix_c_set %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% distinct(cou))))
  domestic_cou <- all_cou[all_cou%in%c(as.vector(unlist(code_countries %>% select(ref_area,CODE) %>% 
  filter(ref_area%in%c("European Union","Rest of Spain","Madrid city")) %>% select(CODE))))]
  inside_cou   <- all_cou[all_cou%in%c("US")]
  outside_cou  <- all_cou[all_cou%in%c("CN")]
  
  all_cou      <- c(as.vector(unlist(matrix_c_set %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% distinct(cou))))
  sim_scenarios <- list("US"="US","EU"=c(all_cou[!all_cou%in%c(all_cou[!all_cou%in%c(c(all_cou[all_cou%in%c(as.vector(unlist(code_countries %>% select(ref_area,CODE) %>% 
  filter(ref_area%in%c("European Union","Rest of Spain","Madrid city")) %>% select(CODE))))]))])]),"WEST"=c(all_cou[!all_cou%in%c(all_cou[!all_cou%in%c(inside_cou,
  c(all_cou[all_cou%in%c(as.vector(unlist(code_countries %>% select(ref_area,CODE) %>% filter(ref_area%in%c("European Union","Rest of Spain","Madrid city")) %>% select(CODE))),
  "CA","US","JP")]))])]))

if(length(files_path_figaro[files_path_figaro=="trade_shock_sim.rds"])==0){
  for(sim_n in names(sim_scenarios)){
    domestic_cou <- sim_scenarios[[sim_n]]
    inside_cou   <- domestic_cou;outside_cou  <- all_cou[!all_cou%in%c(inside_cou,domestic_cou)]
    trade_shock_sim <- data.frame()
    for(shock_n in c(1,rev(seq(0.1,0.9,0.1)))){
      temp_matrix_c_set <- matrix_c_set %>% filter(year==year_sda) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
      left_join(code_countries %>% select(ref_area,CODE),by=c("cou"="CODE")) %>% filter(year==year_sda) %>% pivot_longer(`AR_A`:`ZA_T`,
      names_to="counter_code",values_to="obs_value") %>% separate(counter_code,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
      left_join(code_countries %>% select(ref_area,CODE) %>% rename("counter_area"="ref_area"),by=c("counter_cou"="CODE")) %>% 
      mutate(shock_area = case_when(cou%in%c(inside_cou)~"importer",cou%in%c(outside_cou)~"exporter",cou%in%c(all_cou[!all_cou%in%c(inside_cou,outside_cou)])~"other")) %>%
      mutate(count_area = case_when(counter_cou%in%c(domestic_cou)~"domestic",counter_cou%in%c(all_cou[!all_cou%in%c(domestic_cou)])~"other"))
        
      temp_matrix_c_set %<>% mutate_at(c("obs_value"),~if_else(shock_area=="exporter"&count_area=="domestic",.*shock_n,.)) %>% 
      mutate(counter_cou2 = counter_cou) %>% unite(counter_code,"counter_cou","counter_code",sep="_") %>% rename("counter_cou"="counter_cou2") %>%
      arrange(code,counter_code) %>% group_by(code,counter_code) %>% mutate(excess = 1-sum(obs_value,na.rm=TRUE)) %>% 
      group_by(code,counter_code) %>% mutate(total_to_add = if_else(shock_area=="importer"&count_area=="domestic",obs_value,0)) %>% group_by(code,counter_code) %>%
      mutate(scale_factor=if_else(shock_area=="importer"&count_area=="domestic",excess/sum(total_to_add,na.rm=TRUE),NA)) %>% ungroup %>%
      mutate_at(c("obs_value"),~if_else(shock_area=="importer"&count_area=="domestic",.+(.*scale_factor),.)) %>% ungroup %>%
      unite(code,"cou","code",sep="_") %>% select(code,obs_value,counter_code) %>% mutate_at(c("obs_value"),
      ~if_else(is.nan(.)|is.infinite(.),0,.)) %>% mutate_at(c("obs_value"),~if_else(is.na(.),0,.)) %>% 
      arrange(factor(code,levels=c(unique(matrix_c_set$code))),factor(counter_code,levels=c(unique(matrix_c_set$code)))) %>% 
      pivot_wider(names_from = "counter_code",values_from = "obs_value")
      
      if(sum(matrix_c_set %>% filter(year==year_sda) %>% ungroup %>% select(-code,-year) %>% summarize_all(~sum(.,na.rm=TRUE)) %>% select(1:10) %>% 
      unlist)==sum(temp_matrix_c_set %>% ungroup %>% select(-code) %>% summarize_all(~sum(.,na.rm=TRUE)) %>% select(1:10) %>% unlist)){
      print(paste0(shock_n," shock: The column sums MATCH between the original and modified MRIOT"))} else {
      print(paste0(shock_n," shock: The column sums MATCH between the original and modified MRIOT"))}
      
      if(shock_n==1){
        if(sum(temp_matrix_a)==sum(matrixcalc::hadamard.prod(as.matrix(temp_matrix_c_set %>% column_to_rownames("code")),
        as.matrix(matrix_h_set %>% filter(year==year_sda) %>% column_to_rownames("code") %>% select(-year))))){
        print(paste0(shock_n," shock: The sum of the original C and new C matrix MATCH"))} else {
        print(paste0(shock_n," shock: The sum of the original C and new C matrix DO NOT match"))}
        
        if(sum(temp_matrix_a)==sum(matrixcalc::hadamard.prod(as.matrix(temp_matrix_c_set %>% column_to_rownames("code")),
        as.matrix(matrix_h_set %>% filter(year==year_sda) %>% column_to_rownames("code") %>% select(-year))))){
        print(paste0(shock_n," shock: The mean colsums of the original C and new C matrix MATCH"))} else {
        print(paste0(shock_n," shock: The mean colsums of the original C and new C matrix DO NOT match"))}
      }
      
      trade_shock_sim %<>% bind_rows(temp_matrix_c_set %>% mutate(shock=shock_n,
      year=year_sda,inside=paste0(inside_cou,collapse = ", "),outside=paste0(outside_cou,collapse = ", "),
      domestic=paste0(domestic_cou,collapse = ", ")))
    }
  print(paste0("Completed: ",sim_n))
  saveRDS(trade_shock_sim,file.path(getwd(),"datasets",paste0("trade_shock_matrix_c_sim_",sim_n,".rds")))
  }
} else{
  matrix_c_sim <- readRDS(file.path(getwd(),"datasets",paste0("trade_shock_matrix_c_sim_",sim_n,".rds"))) %>% as_tibble()
}

#################### 
# [D] NEW EMISSIONS VECTOR
#########################################################
sim_scenarios <- list("US"="US","EU"=c(all_cou[!all_cou%in%c(all_cou[!all_cou%in%c(c(all_cou[all_cou%in%c(as.vector(unlist(code_countries %>% select(ref_area,CODE) %>% 
filter(ref_area%in%c("European Union","Rest of Spain","Madrid city")) %>% select(CODE))))]))])]),"WEST"=c(all_cou[!all_cou%in%c(all_cou[!all_cou%in%c(inside_cou,
c(all_cou[all_cou%in%c(as.vector(unlist(code_countries %>% select(ref_area,CODE) %>% filter(ref_area%in%c("European Union","Rest of Spain","Madrid city")) %>% select(CODE))),
"CA","US","JP")]))])]))

if(length(files_path_figaro[files_path_figaro=="sim_co2_p3_s14_EU.rds"])==0){
  for(sim_n in names(sim_scenarios)){
  data_io_year_city <- data.frame()
  matrix_c_sim <- readRDS(file.path(getwd(),"datasets",paste0("trade_shock_matrix_c_sim_",sim_n,".rds"))) %>% as_tibble()
   for(shock_n in c(1,rev(seq(0.1,0.9,0.1)))){
    #### [A] building blocks
    #### [1] Compute the technical coefficients matrix (industry-by-industry)
    temp_matrix_a <- matrixcalc::hadamard.prod(as.matrix(matrix_c_sim %>% filter(shock==shock_n) %>% 
    column_to_rownames("code") %>% select(-shock,-year,-inside,-domestic,-outside)),as.matrix(matrix_h_set %>% 
    filter(year==year_sda) %>% column_to_rownames("code") %>% select(-year)))
    temp_matrix_l <- solve(diag(nrow(temp_matrix_a)) - temp_matrix_a)
    
    #### [2] P3_S14 final demand vector to gross up emisions
    temp_demand <- city_mrio_industry_final %>% filter(year==year_sda) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,!!sym(paste0(sda_vector))) %>%
    unite(code,"cou","code",sep="_") %>% arrange(factor(code,levels=c(colnames(temp_matrix_l))))
    temp_demand %<>% column_to_rownames("code") %>% unlist %>% as.vector
    
    #### [3] Compute the emissions vector
    emissn_vect <- city_mrio_env_accounts %>% filter(year==year_sda) %>% ungroup %>%
    select(code,factor) %>% arrange(factor(code,levels=c(colnames(temp_matrix_l)))) %>% 
    mutate_at(c("factor"),~if_else(is.nan(.)|is.infinite(.),0,.))
    if(length(as.vector(unlist(emissn_vect$code))==colnames(temp_matrix_l))==nrow(temp_matrix_l)){
    print(paste0(year_sda,": The order of country and codes MATCH between emissions vector and matrix")) } else {
    print(paste0(year_sda,": The order of country and codes DO NOT match between emissions vector and matrix"))}
    emissn_vect %<>% column_to_rownames("code") %>% unlist
    
    #### [4] total emissions
    temp_emissions <- diag(as.vector(emissn_vect))%*%as.matrix(temp_matrix_l)%*%diag(temp_demand) %>% 
    as.data.frame %>% set_names(c(as.vector(colnames(temp_matrix_l)))) %>% mutate(code = as.vector(rownames(temp_matrix_l))) %>% 
    select(code,everything()) %>% ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter",values_to="value") %>% unite(code,"cou","code",sep="_") %>%
    pivot_wider(names_from="code",values_from="value") %>% separate(counter,c("cou","code"),sep="_",extra="merge") %>%
    mutate(kgC02_eq = rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>% mutate(year=paste0(year_sda))
    print(temp_emissions %>% group_by(cou) %>% filter(cou=="ES30") %>% mutate(kgC02_eq_city = 
    rowSums(across(`ES30_A`:`ES30_T`),na.rm=TRUE)) %>% summarize(sum(`kgC02_eq`),sum(`kgC02_eq_city`)) %>%
    mutate(shock=shock_n))
   
    data_io_year_city %<>% bind_rows(temp_emissions %>% mutate(shock=shock_n,year=year_sda,
    inside=paste0(inside_cou,collapse = ", ")))
   } 
  print(paste0("Completed: ",sim_n))
  saveRDS(data_io_year_city,file.path(getwd(),"datasets",paste0("sim_co2_p3_s14_",sim_n,".rds")))
  }
} else{
  sim_co2_p3_s14 <- readRDS(file.path(getwd(),"datasets","sim_co2_p3_s14_",sim_n,".rds")) %>% as_tibble()
}
    sim_summary <- data.frame()
    for(sim_n in names(sim_scenarios)){
      temp_sim <- readRDS(file.path(getwd(),"datasets",paste0("sim_co2_p3_s14_",sim_n,".rds"))) %>% as_tibble() %>% 
      group_by(cou,shock) %>% filter(cou=="ES30") %>% mutate(kgC02_eq_city = rowSums(across(`ES30_A`:`ES30_T`),na.rm=TRUE)) %>% 
      summarize(kgC02_eq = sum(`kgC02_eq`)) %>% arrange(-shock) %>% mutate(sim = sim_n)
      sim_summary %<>% bind_rows(temp_sim)
    }
    
  library(wesanderson);library(latex2exp)
  sim_summary %>% filter(shock%in%c(1,0.8,0.6,0.4,0.2)) %>%
    ggplot(aes(y=`kgC02_eq`/1e03,x=factor(shock,levels=c(1,rev(seq(0.1,0.9,0.1)))),group=sim,fill=sim)) + 
    geom_bar(position="dodge", stat="identity",color="white") + scale_fill_brewer(palette="Dark2") +
    labs(fill='') + xlab("Simulated import shock") + ylab(TeX("Mt$\\CO_2$")) + 
    guides(fill=guide_legend(nrow=3,reverse=TRUE)) +
    theme(text = element_text(size = 20),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "none",
    panel.border=element_blank(),legend.margin=margin(0,0,0,0))
    ggsave(file.path(getwd(),"paper","graphics",paste0("simulation_trade_CO2_reduction.pdf")),height = 5,width = 8)
    
  ### BAR PLOT WITH THE THREE SCENARIOS FOR 10, 30, 50, 70, 90
  ### three plots in the same graph in three rows
  ### create scenario changing the 
    
#################### 
# [E] NEW EMISSIONS VECTOR COICOP
#########################################################
if(length(files_path_figaro[files_path_figaro=="city_emissions_coicop_gva_sim.rds"])==0){
  city_emissions_coicop_gva_sim <- data.frame()
  bridge_coicop_cpa <- readRDS(file.path(getwd(),"datasets","bridge_coicop_cpa.rds")) %>% as_tibble()
  bridge_cpa_coicop <- readRDS(file.path(getwd(),"datasets","bridge_cpa_coicop.rds")) %>% as_tibble()
  city_emissions_p3_s14 <- readRDS(file.path(getwd(),"datasets",paste0("sim_co2_p3_s14_",sim_n,"_ES30.rds"))) %>% as_tibble()
  for(shock_n in c(1,rev(seq(0.1,0.9,0.1)))){
    #### [a] retrieve the consumption vector
    temp_vector <- city_emissions_p3_s14 %>% filter(shock==shock_n&cou==cou_footprint) %>% select(-year) %>% 
    unite(code,"cou","code",sep="_") %>% select(-kgC02_eq,-shock,-inside) %>% column_to_rownames("code")
    #### [b] move from NACE to CPA
    temp_emissions <- t(t(sut.to.iot(year_sda,"",'TM',"B","city"))%*%as.matrix(temp_vector)) %>%
    set_colnames(c(paste0("CPA_",unique(city_emissions_p3_s14$code))))
    if((round(sum(temp_emissions)/sum(temp_vector),3)*100)==100){
    print(paste0(shock_n,": The CPA and NACE totals MATCH" )) } else {
    print(paste0(shock_n,": The CPA and NACE totals DO NOT match"))}
  
    #### [c] transform into PRODUCER's prices
    temp_share <- temp_emissions %>% as.data.frame %>% mutate_at(c(paste0("CPA_",
    unique(city_emissions_p3_s14$code))),~./sum(.,na.rm=TRUE)) %>% mutate_all(~if_else(is.nan(.),0,.))
    temp_emissions %<>% as.data.frame %>% rownames_to_column("code") %>%
    separate(code,c("cou","code"),sep="_",extra="merge") %>% select(-cou,-code) %>%
    summarize_all(~sum(.,na.rm=TRUE)) %>% pivot_longer(`CPA_A`:`CPA_T`,names_to = "code",
    values_to = "P3_S14") %>% column_to_rownames("code") %>% ungroup;temp_emissions_cpa <-temp_emissions
    
    #### [d] transform to producers prices to reallocate transport emissions
    temp_emissions %<>% rownames_to_column("code") %>% set_names(c("cpa_code","bp_com")) %>%
    left_join(tax_trade_margins("to_producers",year_sda) %>% ungroup %>% filter(year==year_sda),by=c("cpa_code")) %>% as_tibble %>%
      #### [1] start adding up the negative sum of T&TM margins to avoid negative totals
      mutate(`-ttm` = if_else(`%ttm`<0,bp_com*`%ttm_t&t`,0)) %>% mutate_at(c("%ttm"),~if_else(`%ttm`<0,0,.)) %>%
      #### [2] re-normalize the positive ttm shares to match the negative totals
      mutate(`%ttm` = if_else(`%ttm`>=0,`%ttm`/sum(`%ttm`,na.rm=TRUE),`%ttm`)) %>% 
      #### [3] fin the positive totals by multiplying the new shares by the absolute value of negative ttm total
      mutate(`ttm`  = if_else(`%ttm`>0,`%ttm`*abs(sum(`-ttm`,na.rm=TRUE)),`-ttm`)) %>% 
      #### [4] derive the provisional factory prices and then for those sectors with "0" purchasers,
      #### that is, those with no consumption, we correct the total to be zero
      mutate(fp_com = bp_com+`ttm`) %>% mutate_at(c("ttm"),~if_else(flag_0==1,.-(bp_com+`ttm`),.)) %>% 
      #### [5] finally we redistribute the leftover sum (minimal) and recalculate factory prices
      mutate_at(c("fp_com"),~sum(fp_com-(bp_com+`ttm`),na.rm=TRUE)*fp_com/sum(fp_com,na.rm=TRUE)) %>%
      mutate(fp_com = bp_com+`ttm`) %>% mutate_at(c("fp_com"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>%
      select(cpa_code,fp_com) %>% rename("P3_S14"="fp_com") %>% column_to_rownames("cpa_code")
      if((round(sum(temp_emissions)/sum(temp_emissions_cpa),3)*100)==100){
      print(paste0(shock_n,": Emissions total before and after redistributing ttm margins MATCH" )) } else {
      print(paste0(shock_n,": Emissions total before and after redistributing ttm margins DO NOT match"))}
    
    #### [e] redistribute the new totals across geographical origins and transform into consumption by purpose
    temp_emissions <- as.matrix(temp_share)%*%diag(c(as.matrix(temp_emissions)))%*%as.matrix(bridge_cpa_coicop %>% 
    filter(year==year_n) %>% select(-year) %>% column_to_rownames("CODIGO"))
    cat(paste0("The COICOP total is ",round(sum(temp_emissions)/sum(temp_vector),3)*100,"% of CPA total"))
   
    #### [f] create dataset
    city_emissions_coicop_gva_sim %<>% bind_rows(temp_emissions %>% t() %>% as.data.frame() %>%
    rownames_to_column("code") %>% mutate(cou="ES30") %>% select(cou,code,everything()) %>% 
    mutate(shock=shock_n) %>% as_tibble)
  }
  saveRDS(city_emissions_coicop_gva_sim,file.path(getwd(),"datasets","city_emissions_coicop_gva_sim.rds")) 
} else{
  city_emissions_coicop_gva_sim <- readRDS(file.path(getwd(),"datasets","city_emissions_coicop_gva_sim.rds")) %>% as_tibble()
}
    
    library(RColorBrewer);library(wesanderson);library(latex2exp)
    mycolors <- rev(colorRampPalette(brewer.pal(3,"RdYlBu"))(3))
    city_emissions_coicop_gva_sim %>% filter(shock==0.5) %>% group_by(code) %>%
    transmute(kgCO2_sim=rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>%
    mutate(CODIGOd2=stringr::str_sub(code,1,2)) %>% group_by(CODIGOd2) %>% 
    summarise_at(c("kgCO2_sim"),~sum(.,na.rm=TRUE)) %>% left_join(emissions_spending_sim %>% 
    filter(ANOENC==2019&group==5) %>% mutate(CODIGOd2=stringr::str_sub(CODIGOd3,1,2)) %>%
    group_by(CODIGOd2) %>% summarise_at(c("kgCO2","kgCO2new"),~sum(.,na.rm=TRUE)),by=c("CODIGOd2")) %>%
    left_join(code_coicop_d2 %>% distinct(coicop_g2d,coicop_g2d_heading_short) %>%
    rename("heading"="coicop_g2d_heading_short"),by=c("CODIGOd2"="coicop_g2d")) %>%
    pivot_longer(`kgCO2_sim`:`kgCO2new`,names_to = "type",values_to = "kgCO2") %>%
    mutate_at(c("type"),~case_when(.=="kgCO2"~"Current consumption structure",
    .=="kgCO2new"~"Generalized median consumption structure",
    .=="kgCO2_sim"~"Global supply chain restructuring")) %>%
    ggplot(aes(x = kgCO2, y = reorder(heading,kgCO2),group=type,fill=type)) + 
    geom_bar(position="dodge",stat="identity",color="white") + 
    scale_fill_manual(values = c("#9CCDE2","#D22B26","#ffc425")) + 
    labs(fill='') + ylab("COICOP groups (2 digits)") + 
    xlab(TeX("kt$\\CO_2$")) + guides(fill=guide_legend(nrow=2,reverse=FALSE)) +
    theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "bottom",
    panel.border=element_blank(),legend.margin=margin(0,0,0,0))
    ggsave(file.path(getwd(),"paper","graphics",paste0("three_scenarios_coicop_CO2_reduction.pdf")),
    height = 5,width = 10)

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################