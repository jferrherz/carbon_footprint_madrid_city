##################################################################################################################################
## Block C - IEM - MADRID (CIUDAD)
##################################################################################################################################
if(sum(objects()=="pp_to_bp")==0){pp_to_bp <- readRDS(file.path(getwd(),"datasets","purchasers_to_basic.rds"))}
if(sum(objects()=="bridge_gras_g3d")==0){bridge_gras_g3d <- readRDS(file.path(getwd(),"datasets","bridge_matrix.rds"))}

#################### 
# [B] Matrix transformations
#########################################################
if(length(files_path_figaro[files_path_figaro=="city_vector_p3_s14.rds"])==0){
 if(sum(objects()=="budget_survey_hh_city")==0){budget_survey_hh_city <- readRDS(file.path(getwd(),"datasets","budget_survey_hh_city.rds"))}
 s14_vector <- code_iots_city %>% filter(type=="sector") %>% distinct(nace_code) %>% rename("code"="nace_code")
   
 if(length(files_path_figaro[files_path_figaro=="bridge_coicop_cpa.rds"])==0){
   bridge_coicop_cpa_vector <- data.frame()
   bridge_cpa_coicop_vector <- data.frame()
   for(year_n in seq(year_str_io,year_end_city)){
    #### [a] transform the vector at NACE basic prices to COICOP purchasers
    #### [==> COICOP to CPA
    bridge_cpa_coicop <- bridge_gras_g3d %>% filter(ANOENC==year_n) %>% select(-ANOENC) %>% 
    mutate(CODIGO=c(as.vector(unlist(code_indus64 %>% filter(type=="sector") %>% select(nace_64))),"P3_S14")) %>%
    filter(!CODIGO%in%c("U","P3_S14")) %>% left_join(code_iots_city %>% filter(type=="sector") %>% 
    distinct(nace_code,nace_r2_64),by=c("CODIGO"="nace_r2_64")) %>% group_by(nace_code) %>%
    summarize_at(c(sort(names(bridge_gras_g3d %>%select(-CODIGO,-ANOENC)))),~sum(.,na.rm=TRUE)) %>%
    rename("CODIGO"="nace_code")
      #### This is an important step because we do not have information on the household activities sector (T) in
      #### the tables, so when coming back and forth from COICOP totals don?t match
      bridge_cpa_coicop %<>% mutate_at(c(names(bridge_cpa_coicop %>% select(-CODIGO))),~if_else(CODIGO=="T",0,.)) %>%
      mutate(scale_factor=sum(TOT)) %>% mutate(TOT_T=unlist(bridge_cpa_coicop %>% filter(CODIGO=="T") %>% select(TOT))) %>%
      mutate_at(c("scale_factor"),~(scale_factor+TOT_T)/scale_factor) %>% select(-TOT_T) %>% 
      mutate_at(c(names(bridge_cpa_coicop %>% select(-CODIGO))),~.*scale_factor) %>% 
      mutate_at(c("TOT"),~rowSums(across(`011`:`127`),na.rm=TRUE)) %>% select(-scale_factor) %>%
      column_to_rownames("CODIGO")
    bridge_cpa_coicop %<>% mutate_all(~./TOT) %>% select(-TOT) %>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.)) %>% 
    rownames_to_column("CODIGO") %>% mutate_at(c("CODIGO"),~paste0("CPA_",.)) %>% mutate(year=year_n)
    #### [==> CPA to COICOP
    bridge_coicop_cpa <- bridge_gras_g3d %>% filter(ANOENC==year_n) %>% select(-ANOENC) %>% 
    mutate(CODIGO=c(as.vector(unlist(code_indus64 %>% filter(type=="sector") %>% select(nace_64))),"P3_S14")) %>%
    left_join(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code,nace_r2_64),by=c("CODIGO"="nace_r2_64")) %>% 
    group_by(nace_code) %>% summarize_at(c(sort(names(bridge_gras_g3d %>%select(-CODIGO,-ANOENC)))),~sum(.,na.rm=TRUE)) %>%
    rename("CODIGO"="nace_code")
      #### This is an important step because we do not have information on the household activities sector (T) in
      #### the tables, so when coming back and forth from COICOP totals don?t match
      bridge_coicop_cpa %<>% filter(!is.na(CODIGO)) %>%
      mutate_at(c(names(bridge_coicop_cpa %>% select(-CODIGO))),~if_else(CODIGO=="T",0,.)) %>%
      mutate(scale_factor=sum(TOT)) %>% mutate(TOT_T=unlist(bridge_coicop_cpa %>% filter(CODIGO=="T") %>% select(TOT))) %>%
      mutate_at(c("scale_factor"),~(scale_factor+TOT_T)/scale_factor) %>% select(-TOT_T) %>% 
      mutate_at(c(names(bridge_coicop_cpa %>% select(-CODIGO,-TOT))),~.*scale_factor) %>% 
      mutate_at(c("TOT"),~rowSums(across(`011`:`127`),na.rm=TRUE)) %>% select(-scale_factor) 
      bridge_coicop_cpa %<>% bind_rows(bridge_coicop_cpa %>% summarize_at(c(names(bridge_coicop_cpa %>%
      select(-CODIGO,-TOT))),~sum(.,na.rm=TRUE)) %>% mutate(CODIGO=paste0("P3_S14"),TOT=0) %>% select(CODIGO,everything()))
    bridge_coicop_cpa %<>% pivot_longer(`011`:`TOT`,names_to = "COICOP",values_to = "obs_value") %>%
    filter(!COICOP%in%c("TOT")) %>% pivot_wider(names_from = "CODIGO",values_from = "obs_value") %>% #rename("P3_S14"="NA") %>%
    column_to_rownames("COICOP") %>% mutate_all(~./P3_S14) %>% select(-P3_S14) %>% t %>% as.data.frame %>% 
    rownames_to_column("CODIGO") %>% mutate_at(c("CODIGO"),~paste0("CPA_",.)) %>% mutate(year=year_n)
    bridge_coicop_cpa_vector %<>% bind_rows(bridge_coicop_cpa)
    bridge_cpa_coicop_vector %<>% bind_rows(bridge_cpa_coicop)
   } 
   saveRDS(bridge_coicop_cpa_vector,file.path(getwd(),"datasets","bridge_coicop_cpa.rds")) 
   saveRDS(bridge_cpa_coicop_vector,file.path(getwd(),"datasets","bridge_cpa_coicop.rds")) 
  } else{
    bridge_coicop_cpa <- readRDS(file.path(getwd(),"datasets","bridge_coicop_cpa.rds")) %>% as_tibble()
    bridge_cpa_coicop <- readRDS(file.path(getwd(),"datasets","bridge_cpa_coicop.rds")) %>% as_tibble()
  }
  
  for(year_n in seq(year_str_io,year_end_city)){
  #### [b] from COICOP to CPA
  year_ciot_n <- years[which.min(abs(years - year_n))]
  p3_s14 <- budget_survey_hh_city %>% filter(ANOENC==year_n&!CODIGOd3=="128") %>% select(CODIGOd3,NUMERO,GASTO,FACTOR) %>%
  group_by(CODIGOd3) %>% summarize_at(c("GASTO"),~sum(GASTO*FACTOR,na.rm=TRUE)/1e06) %>%
  column_to_rownames("CODIGOd3") %>% rename("P3_S14"="GASTO")
  p3_s14 <- as.matrix(bridge_coicop_cpa %>% filter(year==year_n) %>% select(-year) %>% column_to_rownames("CODIGO"))%*%as.matrix(p3_s14) %>% as.data.frame
  p3_s14 %<>% consumer_to_basic("to_basics",year_n) %>% column_to_rownames("code_cpa");p3_s14_nace<-p3_s14
  p3_s14 <- sut.to.iot(paste0(year_ciot_n),"",'TM',"D","city")%*%as.vector(as.vector(as.matrix(p3_s14))) %>% 
  as.data.frame %>% set_names(paste0(year_n))
  
  #### [c] append to the dataset and check CPA-NACE conversion
  s14_vector %<>% left_join(p3_s14 %>% rownames_to_column("code"),by=c("code"))
  print(paste0("In ",paste0(year_n),", the total in NACE is ",
  round(sum(p3_s14_nace)/sum(p3_s14),3)*100,"% of CPA \n"))
 }
  saveRDS(s14_vector,file.path(getwd(),"datasets","city_vector_p3_s14.rds")) 
} else{
  city_vector_p3_s14 <- readRDS(file.path(getwd(),"datasets","city_vector_p3_s14.rds")) %>% as_tibble()
}

#################### 
# [C] Include P3_S14 in City MRIO tables
#########################################################
if(length(names(city_mrio_industry_final)[names(city_mrio_industry_final)=="P3_S14"])==0){
  city_mrio_industry_final %<>% left_join(city_mrio_industry %>% select(code,year,ends_with("P3_S14")) %>% 
  pivot_longer(`AR_P3_S14`:`ZA_P3_S14`,names_to = "counter_code",values_to = "P3_S14") %>% filter(year%in%c(seq(year_str_io,year_end_city))) %>% 
  separate(code,c("cou","code"),sep="_",extra="merge") %>% separate(counter_code,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
  filter(cou==counter_cou) %>% unite(code,"cou","code",sep="_") %>% unite(counter_code,"counter_cou","counter_code",sep="_") %>%
  bind_rows(city_vector_p3_s14 %>% mutate(cou="ES30") %>% unite(code,"cou","code",sep="_") %>% 
  pivot_longer(`2013`:`2019`,names_to = "year",values_to = "P3_S14")) %>% arrange(year,counter_code) %>% 
  select(code,year,P3_S14) %>% mutate_at(c("year"),~as.integer(.)),by=c("code","year")) %>% 
  select(code,`AR_A`:`ZA_T`,`P2`,`P3_S14`,`P3AP5`,`GDP`,`P1`,year) %>%
  mutate_at(c("P3_S14"),~if_else(is.na(.),0,.))
  if(series_ras_expand == TRUE){
    saveRDS(city_mrio_industry_final,file.path(getwd(),"datasets","city_mrio_industry_final_extended_p3_s14.rds")) 
  } else {
    saveRDS(city_mrio_industry_final,file.path(getwd(),"datasets","city_mrio_industry_final_p3_s14.rds")) 
  }
} else {
  if(series_ras_expand == TRUE){
    city_mrio_industry_final <- readRDS(file.path(getwd(),"datasets","city_mrio_industry_final_extended_p3_s14.rds")) 
  } else {
    city_mrio_industry_final <- saveRDS(file.path(getwd(),"datasets","city_mrio_industry_final_p3_s14.rds")) 
  }
}

#################### 
# [D] Computation of emissions from P3_S14
#########################################################
if(length(files_path_figaro[files_path_figaro=="city_emissions_p3_s14.rds"])==0){
  if(sum(objects()=="city_vector_p3_s14")==0){
  city_vector_p3_s14.rds <- readRDS(file.path(getwd(),
  "datasets","city_vector_p3_s14.rds"))}
  data_io_year_city <- data.frame()
  for(year_n in seq(year_str_io,year_end_city)){
    #### [A] Compute the technical coefficients matrix (industry-by-industry)
    temp_matrix <- city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,`AR_A`:`ZA_T`) %>% unite(code,"cou","code",
    sep="_") %>% column_to_rownames("code") %>% as.matrix
    if(length(as.vector(colnames(temp_matrix)==nrow(temp_matrix)))==nrow(temp_matrix)){
    print(paste0(year_n,": The order of rows and columns MATCH")) } else {
    print(paste0(year_n,": The order of rows and columns DO NOT"))}
    
    temp_output <- city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,P1) %>% unite(code,"cou","code",
    sep="_") %>% mutate_at(c("P1"),~1/.) %>% mutate_at(c("P1"),~replace_na(.,0)) %>%
    column_to_rownames("code") %>% unlist %>% as.vector
    
    temp_matrix <- (temp_matrix%*%diag(temp_output)) %>% as.data.frame %>%
    mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.)) %>% as.matrix
    temp_matrix <- solve(diag(nrow(temp_matrix)) - temp_matrix)
    
    #### [B] P3_S14 final demand vector to gross up emisions
    temp_demand <- city_mrio_industry %>% filter(year==year_n) %>% select(code,ends_with("P3_S14")) %>%
    pivot_longer(`AR_P3_S14`:`ZA_P3_S14`,names_to = "counter",values_to = "obs_value") %>%
    separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
    separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(cou==counter_cou) %>%
    pivot_wider(names_from = "counter_code",values_from = "obs_value") %>% select(-counter_cou)
    temp_demand %<>% filter(!cou=="ES") %>% bind_rows(temp_demand %>% filter(cou=="ES") %>%
    select(-cou) %>% rename("ES"="P3_S14") %>% left_join(city_vector_p3_s14 %>% select(code,!!sym(paste0(year_n))) %>% 
    set_names(c("code","ES30")),by=c("code")) %>% mutate_at(c("ES"),~.-ES30) %>% pivot_longer(`ES`:`ES30`,
    names_to = "cou",values_to = "P3_S14") %>% arrange(code) %>% select(cou,code,P3_S14)) %>%
    unite(code,"cou","code",sep="_") %>% arrange(factor(code,levels=c(colnames(temp_matrix))))
    if(length(as.vector(unlist(temp_demand$code))==colnames(temp_matrix))==nrow(temp_matrix)){
    print(paste0(year_n,": The order of country and codes MATCH between demand vector and matrix")) } else {
    print(paste0(year_n,": The order of country and codes DO NOT match between demand vector and matrix"))}
    temp_demand %<>% column_to_rownames("code") %>% unlist %>% as.vector
    
    #### [C] Compute the emissions vector
    emissn_vect <- city_mrio_env_accounts %>% filter(year==year_n) %>% ungroup %>%
    select(code,factor) %>% arrange(factor(code,levels=c(colnames(temp_matrix))))
    if(length(as.vector(unlist(emissn_vect$code))==colnames(temp_matrix))==nrow(temp_matrix)){
    print(paste0(year_n,": The order of country and codes MATCH between emissions vector and matrix")) } else {
    print(paste0(year_n,": The order of country and codes DO NOT match between emissions vector and matrix"))}
    emissn_vect %<>% column_to_rownames("code") %>% t %>% as.vector
    
    #### [D] total emissions
    temp_emissions <- diag(as.vector(emissn_vect))%*%as.matrix(temp_matrix)%*%diag(temp_demand) %>% 
    as.data.frame %>% set_names(c(as.vector(colnames(temp_matrix)))) %>% mutate(code = as.vector(colnames(temp_matrix))) %>% 
    select(code,everything()) %>% ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter",values_to="value") %>% unite(code,"cou","code",sep="_") %>%
    pivot_wider(names_from="code",values_from="value") %>% separate(counter,c("cou","code"),sep="_",extra="merge") %>%
    mutate(kgC02_eq = rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>% mutate(year=paste0(year_n))
    print(temp_emissions %>% group_by(cou) %>% filter(cou=="ES30") %>% mutate(kgC02_eq_city = 
    rowSums(across(`ES30_A`:`ES30_T`),na.rm=TRUE)) %>% summarize(sum(`kgC02_eq`),sum(`kgC02_eq_city`)))
    
    if(round(as.vector(emissn_vect)%*%diag(as.vector(city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(code%in%c(as.vector(unlist(code_iots_city %>% 
    filter(type=="sector") %>%distinct(nace_code))))) %>% select(cou,code,P1) %>% #mutate_at(c("P1"),~if_else(cou=="ES30",.,0)) %>%
    unite(code,"cou","code",sep="_") %>% column_to_rownames("code") %>% unlist %>% as.vector)) %>% t %>% as.data.frame %>% 
    mutate(code = as.vector(colnames(temp_matrix))) %>% select(code,everything()) %>% set_names(c("code",year_n)) %>% 
    ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(cou=="ES30") %>% 
    summarize(sum(!!sym(paste0(year_n)))),10)==round(city_mrio_env_accounts %>% filter(year==year_n) %>% ungroup %>%
    select(code,factor) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(cou=="ES30") %>% 
    left_join(city_mrio_industry_final %>% filter(year==year_n) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,P1),by=c("cou","code")) %>%
    mutate(emissions=factor*P1) %>% summarize(sum(emissions)),10)){
    print(paste0(year_n,": Total industry emissions MATCH the results without Leontief's inverse")) } else {
    print(paste0(year_n,": Total industry emissions DO NOT match the results without Leontief's inverse"))}
   
    data_io_year_city %<>% bind_rows(temp_emissions)
    cat(paste0("\n P3_S14's carbon footprint: ",year_n,", "))
  }
  saveRDS(data_io_year_city,file.path(getwd(),"datasets","city_emissions_p3_s14.rds")) 
} else{
  city_emissions_p3_s14 <- readRDS(file.path(getwd(),"datasets","city_emissions_p3_s14.rds")) %>% as_tibble()
}

#################### 
# [E] Allocation of sector emissions BACK to COICOP
#########################################################
if(length(files_path_figaro[files_path_figaro=="city_emissions_coicop.rds"])==0){
  city_emissions_coicop <- data.frame()
  for(year_n in c(seq(year_str_io,year_end_city))){
    year_ciot_n <- years[which.min(abs(years - year_n))]
    #### [a] retrieve the consumption vector
    temp_vector <- city_emissions_p3_s14 %>% filter(year==year_n&cou==cou_footprint) %>% select(-year) %>% 
    unite(code,"cou","code",sep="_") %>% select(-kgC02_eq) %>% column_to_rownames("code")
    #### [b] move from NACE to CPA
    temp_emissions <- t(t(sut.to.iot(year_ciot_n,"",'TM',"B","city"))%*%as.matrix(temp_vector)) %>%
    set_colnames(c(paste0("CPA_",unique(city_emissions_p3_s14$code))))
    if((round(sum(temp_emissions)/sum(temp_vector),3)*100)==100){
    cat(paste0(year_n,": The CPA and NACE totals MATCH \n" )) } else {
    cat(paste0(year_n,": The CPA and NACE totals DO NOT match \n"))}
  
    #### [c] transform into PRODUCER's prices
    temp_share <- temp_emissions %>% as.data.frame %>% mutate_at(c(paste0("CPA_",
    unique(city_emissions_p3_s14$code))),~./sum(.,na.rm=TRUE)) %>% mutate_all(~if_else(is.nan(.),0,.))
    temp_emissions %<>% as.data.frame %>% rownames_to_column("code") %>%
    separate(code,c("cou","code"),sep="_",extra="merge") %>% select(-cou,-code) %>%
    summarize_all(~sum(.,na.rm=TRUE)) %>% pivot_longer(`CPA_A`:`CPA_T`,names_to = "code",
    values_to = "P3_S14") %>% column_to_rownames("code") %>% ungroup;temp_emissions_cpa <-temp_emissions
    
    #### [d] transform to producers prices to reallocate transport emissions
    temp_emissions %<>% rownames_to_column("code") %>% set_names(c("cpa_code","bp_com")) %>%
    left_join(tax_trade_margins("to_producers",year_n) %>% ungroup %>% filter(year==year_n),by=c("cpa_code")) %>% as_tibble %>%
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
      cat(paste0(year_n,": Emissions total before and after redistributing ttm margins MATCH \n" )) } else {
      cat(paste0(year_n,": Emissions total before and after redistributing ttm margins DO NOT match \n"))}
    
    #### [e] redistribute the new totals across geographical origins and transform into consumption by purpose
    temp_emissions <- as.matrix(temp_share)%*%diag(c(as.matrix(temp_emissions)))%*%as.matrix(bridge_cpa_coicop %>%
    filter(year==year_n) %>% select(-year) %>% column_to_rownames("CODIGO"))
    cat(paste0(year_n,": The COICOP total is ",round(sum(temp_emissions)/sum(temp_vector),3)*100,
    "% of CPA total \n \n"))
   
    #### [f] create dataset
    city_emissions_coicop %<>% bind_rows(temp_emissions %>% t() %>% as.data.frame() %>%
    rownames_to_column("code") %>% mutate(cou="ES30") %>% select(cou,code,everything()) %>% 
    mutate(year=year_n) %>% as_tibble)
  }
  saveRDS(city_emissions_coicop,file.path(getwd(),"datasets","city_emissions_coicop.rds")) 
} else{
  city_emissions_coicop <- readRDS(file.path(getwd(),"datasets","city_emissions_coicop.rds")) %>% as_tibble()
}
    
#################### 
# [F] Household's footprint
#########################################################
if(length(files_path_figaro[files_path_figaro=="budget_survey_hh_city_GHG.rds"])==0){
  #### [a] Create a vector with household level emissions using aggregate emission factors
  budget_survey_hh_city_GHG <- budget_survey_hh_city %>% left_join(city_emissions_coicop %>% 
  group_by(code,year) %>% transmute(kgGHG=rowSums(across(`AR_A`:`ZA_T`))) %>%
  left_join(budget_survey_hh_city %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% 
  group_by(ANOENC,CODIGOd3) %>% summarise_at(c("GASTO"),~sum(.*FACTOR,na.rm=TRUE)/1e06),
  by=c("year"="ANOENC","code"="CODIGOd3")) %>% transmute_at(c("kgGHG"),~./GASTO),
  by=c("CODIGOd3"="code","ANOENC"="year")) %>% mutate_at(c("kgGHG"),~.*GASTO) %>% 
  group_by(ANOENC,NUMERO) %>% mutate(kgGHG_HFCE=sum(kgGHG,na.rm=TRUE)) %>% 
  select(ANOENC:GASTO,kgGHG,kgGHG_HFCE,everything()) %>% ungroup
  
  #### [b] Bottom-up computation of emissions by households using HBS
  budget_survey_hh_city_5d <- readRDS(file.path(getwd(),"datasets","epf_g_0616_d5_grupo04_07.rds")) %>%
  left_join(readxl::read_xlsx(file.path(getwd(),"datasets","hbs_emissions_table.xlsx"),
  col_names = TRUE) %>% mutate_at(c("CODIGOd3","CODIGOd4","CODIGO"),~stringr::str_remove_all(.,fixed("."))) %>%
  pivot_longer(`2007`:`2022`,names_to = "ANOENC",values_to = "INTENSRATIO") %>% distinct(ANOENC,CODIGO,INTENSRATIO,CATEGORIA) %>%
  mutate_at(c("ANOENC"),~as.double(.)),by=c("ANOENC","CODIGO")) %>% filter(!is.na(INTENSRATIO)&!is.na(CANTIDAD)) %>%
  select(ANOENC,NUMERO,FACTOR,CATEGORIA,CODIGO,CODIGOd3,CANTIDAD,INTENSRATIO) %>% 
  mutate(kgGHGdirect=CANTIDAD*INTENSRATIO) %>% ungroup()
  
  #### [c] Integration of direct and indirect emissions by households
  #### [ATTENTION] we exclude "other dwellings" on the assumption that they are not located in the city
  budget_survey_hh_city_5d %<>% filter(!grepl("OTRAS",CATEGORIA)) %>% group_by(ANOENC,NUMERO,CODIGOd3) %>% 
  summarize_at(c("kgGHGdirect"),~sum(.,na.rm=TRUE)) %>% pivot_wider(names_from = "CODIGOd3",
  values_from = "kgGHGdirect") %>% rename("HH_HEAT"="045","HH_TRA"="072")
  budget_survey_hh_city_GHG %<>% left_join(budget_survey_hh_city_5d,by=c("ANOENC","NUMERO")) %>% 
  select(ANOENC:GASTO,kgGHG,HH_HEAT,HH_TRA,kgGHG_HFCE,everything())
  
  #### [d] include HH_OTH
  budget_survey_hh_city_GHG %<>% left_join(budget_survey_hh_city_GHG %>% distinct(ANOENC,NUMERO,FACTOR,GASTOT) %>% 
  group_by(ANOENC) %>% mutate(PROPOR=(GASTOT*FACTOR)/sum(GASTOT*FACTOR,na.rm=TRUE)) %>% left_join(city_acounts_madrid %>% 
  filter(nace_r2=="HH_OTH") %>% rename("HH_OTH"="ktCO2") %>% select(-nace_r2),by=c("ANOENC"="year")) %>% 
  mutate_at(c("HH_OTH"),~(.*PROPOR)*1e06/FACTOR) %>% select(-PROPOR),by=c("ANOENC","NUMERO","FACTOR","GASTOT")) %>%
  mutate(kgGHGTOT = rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% mutate(kgGHGEQV = kgGHGTOT/UC2) %>% 
  select(ANOENC:GASTO,kgGHG,HH_HEAT,HH_TRA,HH_OTH,kgGHG_HFCE,kgGHGTOT,everything()) %>% ungroup
  
  if(city_acounts_madrid %>% filter(grepl("HH",nace_r2)) %>% left_join(budget_survey_hh_city_GHG %>% 
  group_by(ANOENC) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
  summarize_at(c("HH_HEAT","HH_TRA","HH_OTH"),~sum(.*FACTOR,na.rm=TRUE)/1e06) %>%
  pivot_longer(HH_HEAT:HH_OTH,names_to="nace_r2",values_to="ktCO2"),by=c("year"="ANOENC",
  "nace_r2")) %>% group_by(year,nace_r2) %>% transmute(ratio=ktCO2.x/ktCO2.y) %>% ungroup %>%
  summarize_at(c("ratio"),~mean(round(.,3)))==1) {
  cat("\n micro-to-macro and macro-to-micro derivation of direct emissions by households MATCH")} else {
  cat("\n micro-to-macro and macro-to-micro derivation of direct emissions by households DO NOT match")}
  
  #### [d] create groups
  budget_survey_hh_city_GHG %<>% create_groups_epf() %>% create_carbon_groups_epf()
  saveRDS(budget_survey_hh_city_GHG,file.path(getwd(),"datasets","budget_survey_hh_city_GHG.rds"))}else{
  budget_survey_hh_city_GHG <- readRDS(file.path(getwd(),"datasets","budget_survey_hh_city_GHG.rds"))
}
  
  #### [==> Total emissions by households (kilotonnes)
  budget_survey_hh_city_GHG %>% distinct(ANOENC,NUMERO,FACTOR,HH_HEAT,HH_TRA,HH_OTH,kgGHG_HFCE,kgGHGTOT,GASTOT) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`kgGHG_HFCE`),na.rm=TRUE)) %>% group_by(ANOENC) %>%
  summarise_at(c("kgGHGTOT"),~sum(.*FACTOR,na.rm=TRUE)/1e06)
  #### [==> Direct emissions by households (kilotonnes)
  budget_survey_hh_city_GHG %>% distinct(ANOENC,NUMERO,FACTOR,HH_HEAT,HH_TRA,HH_OTH,kgGHGTOT,GASTOT) %>%
  mutate_at(c("kgGHGTOT"),~rowSums(across(`HH_HEAT`:`HH_OTH`),na.rm=TRUE)) %>% group_by(ANOENC) %>%
  summarise_at(c("kgGHGTOT"),~sum(.*FACTOR,na.rm=TRUE)/1e06)
  budget_survey_hh_city_GHG %>% distinct(ANOENC,NUMERO,FACTOR,HH_HEAT,HH_TRA,HH_OTH,kgGHGTOT,GASTOT) %>% group_by(ANOENC) %>%
  summarise_at(c("HH_HEAT","HH_TRA"),~sum(.*FACTOR,na.rm=TRUE)/1e06)
  city_emissions_coicop %>% group_by(cou) %>% filter(cou=="ES30") %>% group_by(year) %>% mutate(kgC02_eq_city = 
  rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>% summarize(`kgC02_eq`=sum(`kgC02_eq_city`,na.rm=TRUE))
  
  #### [a] average income by group
  budget_survey_hh_city_GHG %>% filter(CCAA==13&CAPROV==1&ANOENC%in%c(seq(year_str_io,year_end_city))) %>% distinct(ANOENC,NUMERO,FACTOR,.keep_all = TRUE) %>%
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
  select(COICOP,`Q1`:`Q5`,`15-34`:`+71`,everything(),-year)
    
#################### 
# [G] Plot COICOP results
#########################################################
# #### [4] check that emission totals approximately match in the two classifications
# city_emissions_cpa %>% group_by(year) %>% summarize(P3_S14=sum(rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE),na.rm=TRUE)) %>% 
# left_join(city_emissions_coicop %>% group_by(year) %>% summarize_at(c("P3_S14"),~sum(.,na.rm=TRUE)),by=c("year"))

library(latex2exp)
if (!file.exists(file.path(getwd(),"graphics","city_emissions_coicop"))){
dir.create(file.path(getwd(),"graphics","city_emissions_coicop"))}

#### [a] bar plot for the annual distribution of emissions by 2-digit COICOP category
for(year_n in c(seq(year_str_io,year_end_city))){
  temp_plot_coicop <- city_emissions_coicop %>% filter(year==year_n) %>% pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",
  values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
  left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,2)) %>%group_by(cou,code,ref_area,year) %>%
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(code_coicop_d2,by=c("code"="coicop_g2d")) %>%
  rename("heading"="coicop_g2d_heading_short") %>% rename("P3_S14"="obs_value") %>% ungroup()
  
  temp_plot_coicop %>% ggplot(aes(x = P3_S14, y = reorder(heading,P3_S14),fill=
  factor(ref_area,levels=c(rev(unique(temp_plot_coicop$ref_area)[c(3,4,2,6,1,5)]))))) + 
  geom_bar(stat="identity") + 
    #ggplot(aes(x = P3_S14, y = reorder(heading,P3_S14),fill="blue"))
    scale_fill_brewer(palette="YlOrRd") + labs(fill='') + ylab("COICOP (2-digits)") + 
    labs(caption = paste0("Total consumption-linked emissions of ",
    as.vector(unlist(temp_plot_coicop %>% summarize_at(c("P3_S14"),~round(sum(.,na.rm=TRUE),1)))),
    " kilotonnes in ",year_n,"\n Source: Author's elaboration using FIGARO together with regional and municipal accounts data.")) + 
     #"\n Fuente: elaboraci?n propia con datos del Ayuntamiento de Madrid, la Comunidad de Madrid y el INE.")) + 
  xlab(TeX("$\\CO_2$ emissions (kilotonnes)")) + 
  ggtitle(paste0("Total embedded consumption-linked emissions from ",code_countries %>% filter(cou==cou_footprint) %>%
  select(`NAME`) %>% unlist %>% as.vector," by source area, ",year_n)) +
  theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank(),legend.position = "bottom")
  ggsave(file.path(getwd(),"graphics","city_emissions_coicop",
  paste0("emisiones_coicop_madrid_area_",cou_footprint,"_",year_n,".pdf")),height = 10,width = 15)
}

#### [b] chart with the evolution of all COICOP categories from 2013 to 2018
temp_plot_coicop <- city_emissions_coicop %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",
values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>%
mutate_at(c("code"),~stringr::str_sub(.,1,2)) %>%group_by(cou,code,year) %>%
summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(code_coicop_d2,by=c("code"="coicop_g2d")) %>%
rename("heading"="coicop_g2d_heading_short") %>% rename("P3_S14"="obs_value") %>% ungroup()
temp_plot_coicop %>%
ggplot(aes(y = P3_S14/1e03, x = as.factor(year), group = heading, color = heading)) + 
  geom_line() + scale_fill_brewer(palette="Paired")+labs(color='') + theme(legend.position = "bottom",
  text = element_text(size = 20),axis.ticks.x=element_blank(),axis.ticks.y=element_blank()) +
  ylab(latex2exp::TeX("Emisiones de $\\CO_2$ (kilotoneladas)")) +
  ggtitle(paste0("Emisiones totales en ","ES",", 2010-2020")) +  xlab("") + 
  labs(caption="Fuente: elaboraci?n propia con datos de Eurostat.")+ theme_gray()
ggsave(file.path(getwd(),"graphics","city_emissions_coicop",
paste0("emisiones_por_categoria_coicop.pdf")),height = 5,width = 10)
  
#### [c] bar plot comapring the beginning and end of period emissions by 2-digit COICOP category
city_emissions_coicop %>% #pivot_longer(`2010`:`2021`,names_to = "ANOENC", values_to = "value") %>%
  filter(ANOENC%in%c(2010,2021)) %>% ggplot(aes(x = value/1e03, y = reorder(heading, value),
  fill=ANOENC)) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette="Paired")+
  labs(fill='') +   ylab("Clasificaci?n COICOP") + labs(caption = paste0("Emisiones totales de ",
  city_emissions_coicop %>% summarize_at(c(paste0(2013)),~round(sum(.,na.rm=TRUE)/1e03,1)) %>% unlist %>% 
  as.vector %>% round(2)," y ", city_emissions_coicop %>% summarize_at(c(paste0(2018)),
  ~round(sum(.,na.rm=TRUE)/1e03,1)) %>% unlist %>% as.vector %>% round(2)," kilotoneladas en 2013 y 2018, respectivamente",
  "\n Fuente: elaboración propia con datos del Ayuntamiento de Madrid, la Comunidad de Madrid y el INE.")) + 
  xlab(TeX("Emisiones de $\\CO_2$ (kilotoneladas)")) + 
  ggtitle(paste0("Emisiones ligadas al consumo de los hogares en la ciudad de Madrid por categoria COICOP (2 dígitos)")) +
  theme(text = element_text(size = 20),axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank(),legend.position = "bottom")
  ggsave(file.path(getwd(),"graphics","city_emissions_coicop",
  paste0("emisiones_coicop_madrid.pdf")),height = 10,width = 15)
  
for(year_n in c(seq(year_str_io,year_end_city))){
  city_emissions_coicop %>% select(cou,code,P3_S14,year) %>% filter(year==year_n) %>% 
  left_join(code_coicop_d2,by=c("code"="coicop_g2d")) %>% rename("heading"="coicop_g2d_heading_short") %>%
  ggplot(aes(x = P3_S14, y = reorder(heading,P3_S14),
  fill="blue")) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette="Paired")+
  labs(fill='') + ylab("Clasificaci?n COICOP (2 d?gitos)") + labs(caption = paste0("Emisiones totales de ",
  city_emissions_coicop %>% select(cou,code,P3_S14,year) %>% filter(year==year_n) %>% 
  summarize_at(c("P3_S14"),~round(sum(.,na.rm=TRUE),1)) %>% unlist %>% as.vector %>% round(2)," kilotoneladas en ",
  year_n,"\n Fuente: elaboraci?n propia con datos del Ayuntamiento de Madrid, la Comunidad de Madrid y el INE.")) + 
  xlab(TeX("Emisiones de $\\CO_2$ (kilotoneladas)")) + 
  ggtitle(paste0("Emisiones ligadas al consumo de los hogares en la ciudad de Madrid, ",year_n)) +
  theme(text = element_text(size = 20),axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank(),legend.position = "none")
  ggsave(file.path(getwd(),"graphics","city_emissions_coicop",
  paste0("emisiones_coicop_madrid_",year_n,".pdf")),height = 10,width = 15)
}
  
#### [d] heatmap industry to coicop
for(year_n in c(2013:2018)){
  city_emissions_matrix %>% filter(year==year_n) %>% select(-year) %>% 
  column_to_rownames("code") %>% as.matrix %>% heatmap(., scale="column")
}
  
  tem_enviro_city_year <- readRDS(file.path(getwd(),"datasets","city_accounts_madrid.rds")) %>% as_tibble
  tem_enviro_city_hh <- tem_enviro_city_year %>% filter(grepl("HH",nace_r2)) %>%
  pivot_wider(names_from = "nace_r2",values_from = "ktCO2")
  hh_tra_2019 <- as.vector(round(unlist(tem_enviro_city_hh %>% filter(year==year_n) %>% 
  select(-year) %>% select(`HH_TRA`)),3))
  hh_heat_2019 <- as.vector(round(unlist(tem_enviro_city_hh %>% filter(year==year_n) %>% 
  select(-year) %>% select(`HH_HEAT`)),3))
  hh_other_2019 <- as.vector(round(unlist(tem_enviro_city_hh %>% filter(year==year_n) %>% 
  select(-year) %>% select(`HH_OTH`)),3))
  
  tem_enviro_city_hh %>% left_join(tem_enviro_city_year %>% filter(!grepl("HH",nace_r2)) %>%
  pivot_wider(names_from = "nace_r2",values_from = "ktGHG") %>% group_by(year) %>%
  transmute(`Industries`=rowSums(across(`A`:`T`),na.rm=TRUE)),by=c("year")) %>% 
  #mutate(`Total`=rowSums(across(`HH_HEAT`:`Industries`),na.rm=TRUE)) %>%
  left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Inventario",
  "inventario_madrid_2021.xlsx")) %>% set_names(c("year",paste0("0",2:9),"10","11","TOT")) %>%
  filter(year%in%c(seq(year_str_io,year_end_city))) %>% select(year,TOT),by=c("year")) %>%
  mutate_all(~round(.,1)) %>% set_names(c("Year","Heating","Transport","Other",
  "Industries","Inventory"))
  
#### industry and purpose mapping
library(latex2exp)
plot_nace_coicop_area <- city_emissions_coicop %>% pivot_longer(`AR_A`:`ZA_T`,names_to = "counter_code",values_to = "P3_S14") %>%
separate(counter_code,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
mutate_at(c("counter_cou"),~if_else(grepl("ES",.),.,"RoW")) %>% mutate(counter_code_v2 = counter_code) %>%
mutate_at(c("counter_code"),~stringr::str_sub(.,1,1)) %>% mutate_at(c("counter_code"),~if_else(.=="B","B_E",.)) %>%
mutate_at(c("counter_code"),~if_else(counter_code%in%c("C","H"),"C_H",.)) %>%
mutate_at(c("counter_code"),~if_else(counter_code%in%c("B_E","C_H"),.,"Rest")) %>%
mutate_at(c("code"),~stringr::str_sub(.,1,2)) %>% group_by(cou,code,year,counter_cou,counter_code) %>% 
summarize_at(c("P3_S14"),~sum(.,na.rm=TRUE)) %>% ungroup %>%
left_join(code_coicop_d2 %>% distinct(coicop_g2d,coicop_g2d_heading_short) %>%
rename("heading"="coicop_g2d_heading_short"),by=c("code"="coicop_g2d")) %>%
mutate_at(c("counter_cou"),~case_when(.=="ES"~"Spain",.=="ES30"~"Madrid",.=="RoW"~"RoW")) %>%
mutate(ref_area=paste0(counter_cou," (",counter_code,")"))

library(RColorBrewer)
for(year_n in seq(year_str_io,year_end_city)){
  mycolors <- rev(colorRampPalette(brewer.pal(11,"RdYlBu"))(9))
  plot_nace_coicop_area %>% select(year,P3_S14,heading,ref_area) %>% filter(year==year_n) %>%
  ggplot(aes(x = P3_S14, y = reorder(heading,P3_S14),
  fill=factor(ref_area,levels=c(rev(unique(ref_area)[c(4,5,6,1,2,3,7,8,9)]))))) + 
    geom_bar(stat="identity") + scale_fill_manual(values = mycolors) + 
    labs(fill='') + ylab("COICOP groups (2 digits)") + 
      xlab(TeX("kt$\\CO_2$")) + guides(fill=guide_legend(nrow=3,reverse=TRUE)) +
      theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),legend.position = "bottom",
      panel.border=element_blank(),legend.margin=margin(0,0,0,0))
  ggsave(file.path(getwd(),"paper","graphics",paste0("emisiones_coicop_nace_area_",
  cou_footprint,"_",year_n,".pdf")),height = 5,width = 10)
}
##################################################################################################################################
## END OF SCRIPT
########################################################################################################################  ##########