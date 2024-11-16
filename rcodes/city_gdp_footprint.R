##################################################################################################################################
## Block C - IEM - MADRID (CIUDAD)
##################################################################################################################################
  
#################### 
# [B] Computation of emissions from GDP
#########################################################
if(length(files_path_figaro[files_path_figaro=="city_emissions_gdp.rds"])==0){
  data_io_year_city <- data.frame()
  cat(paste0("\n GDP's carbon footprint: "))
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
    
    #### [B] GDP final demand vector to gross up emisions
    temp_demand <- city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,GDP) %>% #mutate_at(c("GDP"),~if_else(cou=="ES30",.,0)) %>%
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
    
    if(round(as.vector(emissn_vect)%*%diag(as.vector(temp_demand)) %>% t %>% as.data.frame %>% 
    mutate(code = as.vector(colnames(temp_matrix))) %>% select(code,everything()) %>% set_names(c("code",year_n)) %>% 
    ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(cou=="ES30") %>% 
    summarize(sum(!!sym(paste0(year_n)))),0)==round(city_mrio_env_accounts %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    filter(cou=="ES30") %>% group_by(year) %>% filter(year==year_n) %>% left_join(city_mrio_industry_final %>% 
    filter(year==year_n) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code))))) %>% 
    select(cou,code,GDP),by=c("cou","code")) %>% mutate_at(c(paste0("kt",emissions_mod,"F")),~factor*GDP) %>% summarize(sum(!!sym(paste0("kt",emissions_mod,"F")))) %>% 
    select(2),0)){print(paste0(year_n,": The GDP-linked emissions MATCH the results without Leontief's inverse")) } else {
    print(paste0(year_n,": The GDP-linked emissions DO NOT match the results without Leontief's inverse"))}
    
    if(round(as.vector(emissn_vect)%*%diag(as.vector(city_mrio_industry_final %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(code%in%c(as.vector(unlist(code_iots_city %>% 
    filter(type=="sector") %>%distinct(nace_code))))) %>% select(cou,code,P1) %>% #mutate_at(c("P1"),~if_else(cou=="ES30",.,0)) %>%
    unite(code,"cou","code",sep="_") %>% column_to_rownames("code") %>% unlist %>% as.vector)) %>% t %>% as.data.frame %>% 
    mutate(code = as.vector(colnames(temp_matrix))) %>% select(code,everything()) %>% set_names(c("code",year_n)) %>% 
    ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(cou=="ES30") %>% 
    summarize(sum(!!sym(paste0(year_n)))),0)==round(city_mrio_env_accounts %>% filter(year==year_n) %>% ungroup %>%
    select(code,factor) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(cou=="ES30") %>% 
    left_join(city_mrio_industry_final %>% filter(year==year_n) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,P1),by=c("cou","code")) %>%
    mutate(emissions=factor*P1) %>% summarize(sum(emissions)),0)){
    print(paste0(year_n,": Total industry emissions MATCH the results without Leontief's inverse")) } else {
    print(paste0(year_n,": Total industry emissions DO NOT match the results without Leontief's inverse"))}
   
    data_io_year_city %<>% bind_rows(temp_emissions)
    cat(paste0("\n GDP's carbon footprint: ",year_n,", "))
  }
  saveRDS(data_io_year_city,file.path(getwd(),"datasets","city_emissions_gdp.rds")) 
} else{
  city_emissions_gdp <- readRDS(file.path(getwd(),"datasets","city_emissions_gdp.rds")) %>% as_tibble()
}

if(print_plots==TRUE){
  library(latex2exp)
  if (!file.exists(file.path(getwd(),"graphics","city_emissions_gdp"))){
  dir.create(file.path(getwd(),"graphics","city_emissions_gdp"))}
  #### [a] bar plot for the annual distribution of emissions by 2-digit COICOP category
  for(year_n in seq(year_str_io,year_end_city)){
    city_emissions_gdp %>% filter(year==year_n) %>% filter(cou==cou_footprint) %>% select(-kgC02_eq) %>%
    pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),
    sep="_",extra="merge") %>% mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% left_join(code_countries %>% 
    mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
    mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
    distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(code,ref_area,year) %>% 
    summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("GDP"="obs_value") %>% 
    left_join(code_industries %>% distinct(code,name_short),by=c("code")) %>%
    mutate_at(c("code"),~if_else(code=="B","B_E",.)) %>% mutate_at(c("name_short"),
    ~if_else(code=="B_E","Mining & Energy supply",.)) %>% mutate_at(c("name_short"),~paste0(.," (",code,")")) %>% 
      ggplot(aes(x = GDP, y = reorder(name_short,GDP),fill=ref_area)) + geom_bar(stat="identity") + 
        scale_fill_brewer(palette="YlOrRd") + labs(fill='') + ylab("NACE (rev.2)") + 
        labs(caption = paste0("Total GDP-linked emissions of ",as.vector(unlist(round(city_emissions_gdp %>% 
        pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% filter(cou==cou_footprint) %>%
        select(cou,code,obs_value,year) %>% filter(year==year_n) %>% summarize_at(c("obs_value"),~round(sum(.,na.rm=TRUE),1)),2))),
        " kilotonnes in ",year_n,"\n Source: Author's elaboration using FIGARO together with regional and municipal accounts data.")) + 
         #"\n Fuente: elaboraci?n propia con datos del Ayuntamiento de Madrid, la Comunidad de Madrid y el INE.")) + 
      xlab(TeX("$\\CO_2$ emissions (kilotonnes)")) + 
      ggtitle(paste0("Total embedded GDP-linked emissions from ",code_countries %>% filter(cou==cou_footprint) %>%
      select(`NAME`) %>% unlist %>% as.vector," by source area, ",year_n)) +
      theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),legend.position = "bottom")
      ggsave(file.path(getwd(),"graphics","city_emissions_gdp",
      paste0("emisiones_gdp_madrid_area_",cou_footprint,"_",year_n,".pdf")),height = 10,width = 15)
      
    city_emissions_gdp %>% filter(year==year_n) %>% filter(cou==cou_footprint) %>% select(-kgC02_eq) %>%
    pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% separate(counter,c("counter_cou","counter_code"),
    sep="_",extra="merge") %>% mutate(code_2d=stringr::str_sub(code,1,1)) %>% left_join(code_countries %>% 
    mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
    mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
    distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(code,ref_area,year) %>%
    summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("GDP"="obs_value") %>% filter(grepl("C",code)) %>%
    left_join(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code,nace_r2_3d_description) %>%
    mutate_at(c("nace_r2_3d_description"),~stringr::str_remove(.,fixed("Manufacture of"))) %>%
    mutate_at(c("nace_r2_3d_description"),~stringr::str_to_sentence(str_trim(., side = c("left")))) %>%
    separate(nace_r2_3d_description,c("nace_r2_3d_description"),sep=";",extra = "drop") %>% ungroup %>%
    distinct(nace_code,.keep_all = TRUE) %>% filter(grepl("C",nace_code)),by=c("code"="nace_code")) %>% 
    rename("name_short"="nace_r2_3d_description") %>% mutate_at(c("name_short"),~paste0(.," (",code,")")) %>% 
      ggplot(aes(x = GDP, y = reorder(name_short,GDP),fill=ref_area)) + geom_bar(stat="identity") + 
        scale_fill_brewer(palette="YlOrRd") + labs(fill='') + ylab("NACE (rev.2)") + #coord_flip() +
        labs(caption = paste0("Total GDP-linked emissions of ",as.vector(unlist(round(city_emissions_gdp %>% 
        pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% filter(cou==cou_footprint) %>%
        select(cou,code,obs_value,year) %>% filter(year==year_n) %>% summarize_at(c("obs_value"),~round(sum(.,na.rm=TRUE),1)),2))),
        " kilotonnes in ",year_n,"\n Source: Author's elaboration using FIGARO together with regional and municipal accounts data.")) + 
         #"\n Fuente: elaboraci?n propia con datos del Ayuntamiento de Madrid, la Comunidad de Madrid y el INE.")) + 
      xlab(TeX("$\\CO_2$ emissions (kilotonnes)")) + scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
      ggtitle(paste0("Total embedded GDP-linked emissions from ",code_countries %>% filter(cou==cou_footprint) %>%
      select(`NAME`) %>% unlist %>% as.vector," by source area, ",year_n)) +
      theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),legend.position = "bottom")
      ggsave(file.path(getwd(),"graphics","city_emissions_gdp",
      paste0("emisiones_gdp_madrid_area_industry_",cou_footprint,"_",year_n,".pdf")),height = 10,width = 15)
  }
} 

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################