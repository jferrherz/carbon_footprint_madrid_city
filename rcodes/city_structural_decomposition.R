##################################################################################################################################
## Block C - SDA & POLICY SIMULATION
##################################################################################################################################

sda_method <- "trade&technology"
sda_vector <- "P3_S14"
year_sda   <- year_end_city

#################### 
# [A] Structural Decomposition
#########################################################
if(length(files_path_figaro[files_path_figaro=="city_emissions_p3_s14.rds"])==0){
    #### [A] building blocks
    #### [1] Compute the technical coefficients matrix (industry-by-industry)
    temp_matrix_z_str <- city_mrio_industry_final %>% filter(year==year_str) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,`AR_A`:`ZA_T`) %>% unite(code,"cou","code",
    sep="_") %>% column_to_rownames("code") %>% as.matrix
    temp_matrix_z_end <- city_mrio_industry_final %>% filter(year==year_end) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,`AR_A`:`ZA_T`) %>% unite(code,"cou","code",
    sep="_") %>% column_to_rownames("code") %>% as.matrix
    if(length(as.vector(colnames(temp_matrix_z_str)==nrow(temp_matrix_z_str)))==nrow(temp_matrix_z_str)){
    print(paste0(year_sda,": The order of rows and columns MATCH")) } else {
    print(paste0(year_sda,": The order of rows and columns DO NOT"))}
    
    temp_output_str <- city_mrio_industry_final %>% filter(year==year_str) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,P1) %>% unite(code,"cou","code",
    sep="_") %>% mutate_at(c("P1"),~1/.) %>% mutate_at(c("P1"),~replace_na(.,0)) %>%
    column_to_rownames("code") %>% unlist %>% as.vector
    temp_output_end <- city_mrio_industry_final %>% filter(year==year_end) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,P1) %>% unite(code,"cou","code",
    sep="_") %>% mutate_at(c("P1"),~1/.) %>% mutate_at(c("P1"),~replace_na(.,0)) %>%
    column_to_rownames("code") %>% unlist %>% as.vector
    
    temp_matrix_a_str <- (temp_matrix_z_str%*%diag(temp_output_str)) %>% as.data.frame %>%
    mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.)) %>% set_rownames(c(rownames(temp_matrix_z_str))) %>%
    set_colnames(c(colnames(temp_matrix_z_end))) %>% as.matrix
    temp_matrix_a_end <- (temp_matrix_z_end%*%diag(temp_output_end)) %>% as.data.frame %>%
    mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.)) %>% set_rownames(c(rownames(temp_matrix_z_str))) %>%
    set_colnames(c(colnames(temp_matrix_z_str))) %>% as.matrix
    temp_matrix_str <- solve(diag(nrow(temp_matrix_a_str)) - temp_matrix_a_str)
    temp_matrix_end <- solve(diag(nrow(temp_matrix_a_end)) - temp_matrix_a_end)
    
    #### [2] P3_S14 final demand vector to gross up emisions
    temp_demand_str <- city_mrio_industry_final %>% filter(year==year_str) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,!!sym(paste0(sda_vector))) %>%
    unite(code,"cou","code",sep="_") %>% arrange(factor(code,levels=c(colnames(temp_matrix_str))))
    temp_demand_str %<>% column_to_rownames("code") %>% unlist %>% as.vector
    temp_demand_end <- city_mrio_industry_final %>% filter(year==year_end) %>% 
    separate(code,c("cou","code"),sep="_",extra="merge") %>%
    filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(nace_code))))) %>% select(cou,code,!!sym(paste0(sda_vector))) %>%
    unite(code,"cou","code",sep="_") %>% arrange(factor(code,levels=c(colnames(temp_matrix_end))))
    temp_demand_end %<>% column_to_rownames("code") %>% unlist %>% as.vector
    
    #### [3] Compute the emissions vector
    emissn_vect_str <- city_mrio_env_accounts %>% filter(year==year_str) %>% ungroup %>%
    select(code,factor) %>% arrange(factor(code,levels=c(colnames(temp_matrix_str))))
    if(length(as.vector(unlist(emissn_vect_str$code))==colnames(temp_matrix_str))==nrow(temp_matrix_str)){
    print(paste0(year_sda,": The order of country and codes MATCH between emissions vector and matrix")) } else {
    print(paste0(year_sda,": The order of country and codes DO NOT match between emissions vector and matrix"))}
    emissn_vect_str %<>% column_to_rownames("code") %>% t %>% as.vector
    emissn_vect_end<- city_mrio_env_accounts %>% filter(year==year_end) %>% ungroup %>%
    select(code,factor) %>% arrange(factor(code,levels=c(colnames(temp_matrix_end))))
    if(length(as.vector(unlist(emissn_vect_end$code))==colnames(temp_matrix_end))==nrow(temp_matrix_end)){
    print(paste0(year_sda,": The order of country and codes MATCH between emissions vector and matrix")) } else {
    print(paste0(year_sda,": The order of country and codes DO NOT match between emissions vector and matrix"))}
    emissn_vect_end%<>% column_to_rownames("code") %>% t %>% as.vector
    
    #### [4] total emissions
    temp_emissions_str <- diag(as.vector(emissn_vect_str))%*%as.matrix(temp_matrix_str)%*%diag(temp_demand_str) %>% 
    as.data.frame %>% set_names(c(as.vector(colnames(temp_matrix_str)))) %>% mutate(code = as.vector(colnames(temp_matrix_str))) %>% 
    select(code,everything()) %>% ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter",values_to="value") %>% unite(code,"cou","code",sep="_") %>%
    pivot_wider(names_from="code",values_from="value") %>% separate(counter,c("cou","code"),sep="_",extra="merge") %>%
    mutate(kgC02_eq = rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>% mutate(year=paste0(year_sda)) %>% select(cou,code,kgC02_eq)
    temp_emissions_end <- diag(as.vector(emissn_vect_end))%*%as.matrix(temp_matrix_end)%*%diag(temp_demand_end) %>% 
    as.data.frame %>% set_names(c(as.vector(colnames(temp_matrix_end)))) %>% mutate(code = as.vector(colnames(temp_matrix_end))) %>% 
    select(code,everything()) %>% ungroup() %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter",values_to="value") %>% unite(code,"cou","code",sep="_") %>%
    pivot_wider(names_from="code",values_from="value") %>% separate(counter,c("cou","code"),sep="_",extra="merge") %>%
    mutate(kgC02_eq = rowSums(across(`AR_A`:`ZA_T`),na.rm=TRUE)) %>% mutate(year=paste0(year_sda)) %>% select(cou,code,kgC02_eq)
    
    #### [C] Structural decomposition - Next level decomposition of L and F
    #### Xu and Dietzenbacher
    temp_matrix_h_str <- temp_matrix_a_str %>% as.data.frame %>% rownames_to_column("code") %>% as_tibble %>%
    separate(code,c("cou","code"),sep="_",extra="merge") %>% select(cou,code) %>% left_join(temp_matrix_a_str %>% 
    as.data.frame %>% rownames_to_column("code") %>% as_tibble %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    group_by(code) %>% summarize_at(c(colnames(temp_matrix_a_str)),~sum(.,na.rm=TRUE)),by=c("code")) %>%
    unite(code,"cou","code",sep="_") %>% column_to_rownames("code") %>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    temp_matrix_c_str <- temp_matrix_a_str %>% as.data.frame %>% rownames_to_column("code") %>% as_tibble %>%
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter_code",values_to="obs_value") %>% left_join(temp_matrix_h_str %>% 
    as.data.frame %>% rownames_to_column("code") %>% as_tibble %>% pivot_longer(`AR_A`:`ZA_T`,names_to="counter_code",
    values_to="obs_value"),by=c("code","counter_code")) %>% rename("A"="obs_value.x","H"="obs_value.y") %>%
    mutate_at(c("A"),~./`H`) %>% select(-`H`) %>% pivot_wider(names_from = "counter_code",values_from = "A") %>%
    column_to_rownames("code") %>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    
    temp_matrix_h_end <- temp_matrix_a_end %>% as.data.frame %>% rownames_to_column("code") %>% as_tibble %>%
    separate(code,c("cou","code"),sep="_",extra="merge") %>% select(cou,code) %>% left_join(temp_matrix_a_end %>% 
    as.data.frame %>% rownames_to_column("code") %>% as_tibble %>% separate(code,c("cou","code"),sep="_",extra="merge") %>% 
    group_by(code) %>% summarize_at(c(colnames(temp_matrix_a_str)),~sum(.,na.rm=TRUE)),by=c("code")) %>%
    unite(code,"cou","code",sep="_") %>% column_to_rownames("code") %>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    temp_matrix_c_end <- temp_matrix_a_end %>% as.data.frame %>% rownames_to_column("code") %>% as_tibble %>%
    pivot_longer(`AR_A`:`ZA_T`,names_to="counter_code",values_to="obs_value") %>% left_join(temp_matrix_h_end %>% 
    as.data.frame %>% rownames_to_column("code") %>% as_tibble %>% pivot_longer(`AR_A`:`ZA_T`,names_to="counter_code",
    values_to="obs_value"),by=c("code","counter_code")) %>% rename("A"="obs_value.x","H"="obs_value.y") %>%
    mutate_at(c("A"),~./`H`) %>% select(-`H`) %>% pivot_wider(names_from = "counter_code",
    values_from = "A") %>% column_to_rownames("code") %>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    
    if(hadamard.prod(as.matrix(temp_matrix_c_str),as.matrix(temp_matrix_h_str))[1,1]==as.matrix(temp_matrix_a_str)[1,1]){
    print(paste0("The technical coefficients matrix MATCH the Hadamard product of their components"))} else {
    print(paste0("The technical coefficients matrix DO NOT match the Hadamard product of their components"))}
    if((temp_matrix_str[1,1])==(solve(diag(nrow(temp_matrix_h_str)) - hadamard.prod(as.matrix(temp_matrix_c_str),as.matrix(temp_matrix_h_str)))[1,1])){
    print(paste0("The Leontief inverse MATCH the Hadamard product of their components"))} else {
    print(paste0("The Leontief inverse DO NOT match the Hadamard product of their components"))}
    
    #### [1] growth vectors to check that the decomposition matches
    delta_eps <- temp_emissions_str %>% rename("kgC02_eq_str"="kgC02_eq") %>% 
    left_join(temp_emissions_end %>% rename("kgC02_eq_end"="kgC02_eq"),by=c("cou","code")) %>% 
    mutate(delta_emiss = kgC02_eq_end-kgC02_eq_str) %>% unite(code,cou,code) %>% select(code,delta_emiss) %>%
    column_to_rownames("code") %>% unlist %>% as.vector
    delta_e   <- as.vector(emissn_vect_end) - as.vector(emissn_vect_str)
    delta_L   <- as.matrix(temp_matrix_end) - as.matrix(temp_matrix_str)
    delta_C   <- as.matrix(temp_matrix_c_end) - as.matrix(temp_matrix_c_str)
    delta_H   <- as.matrix(temp_matrix_h_end) - as.matrix(temp_matrix_h_str)
    delta_A   <- hadamard.prod(as.matrix(temp_matrix_c_end),as.matrix(temp_matrix_h_end))-hadamard.prod(as.matrix(temp_matrix_c_str),as.matrix(temp_matrix_h_str))
    delta_f   <- as.vector(temp_demand_end) - as.vector(temp_demand_str)
    delta_eLf <- as.vector(emissn_vect_end)%*%as.matrix(temp_matrix_end)%*%diag(temp_demand_end)-
    as.vector(emissn_vect_str)%*%as.matrix(temp_matrix_str)%*%diag(temp_demand_str)
    
    #### [2] Simple SD formula
    #### [a] Emissions intensity effect
    change_input <- ((1/2)*(delta_e%*%((as.matrix(temp_matrix_str)%*%diag(temp_demand_str)) + (as.matrix(temp_matrix_end)%*%diag(temp_demand_end)))))
    #### [b] Technology effect
    # if(round(delta_L[1],5)==round((hadamard.prod(delta_C,delta_H))[1],5)){
    # print(paste0("The change to the Leontief inverse MATCHES the change of the subcomponents"))} else {
    # print(paste0("The change to the Leontief inverse DO NOT match the change of the subcomponents"))}
    #### Technology effect original
    change_tech_tot <- ((1/2)*((as.vector(emissn_vect_str)%*%delta_L%*%diag(temp_demand_end)) + (as.vector(emissn_vect_end)%*%delta_L%*%diag(temp_demand_str))))
    #### Trade effect [matrix C]
    change_trade <- ((1/4)*(as.vector(emissn_vect_str)%*%(as.matrix(temp_matrix_str)%*%(hadamard.prod(delta_C,(as.matrix(temp_matrix_h_end) + as.matrix(temp_matrix_h_str))))%*%as.matrix(temp_matrix_end))%*%diag(temp_demand_end)))+
    ((1/4)*(as.vector(emissn_vect_end)%*%(as.matrix(temp_matrix_str)%*%(hadamard.prod(delta_C,(as.matrix(temp_matrix_h_end) + as.matrix(temp_matrix_h_str))))%*%as.matrix(temp_matrix_end))%*%diag(temp_demand_str)))
    #### Technology effect [matrix H, A* in the literature]
    change_tech <- ((1/4)*(as.vector(emissn_vect_str)%*%(as.matrix(temp_matrix_str)%*%(hadamard.prod((as.matrix(temp_matrix_c_end) + as.matrix(temp_matrix_c_str)),delta_H))%*%as.matrix(temp_matrix_end))%*%diag(temp_demand_end)))+
    ((1/4)*(as.vector(emissn_vect_end)%*%(as.matrix(temp_matrix_str)%*%(hadamard.prod((as.matrix(temp_matrix_c_end) + as.matrix(temp_matrix_c_str)),delta_H))%*%as.matrix(temp_matrix_end))%*%diag(temp_demand_str)))
    if(round(change_tech_tot[1],5)==round((change_trade+change_tech)[1],5)){
    print(paste0("The trade and technology effects MATCH the correct aggregation"))} else {
    print(paste0("The trade and technology effects DO NOT match the correct aggregation"))}
    #### [c] Demand effect
    change_demd  <- ((1/2)*((as.vector(emissn_vect_str)%*%as.matrix(temp_matrix_str))+
    (as.vector(emissn_vect_end)%*%as.matrix(temp_matrix_end)))%*%diag(delta_f))
    change_emiss <- change_input+change_trade+change_tech+change_demd
    if(round((change_input+change_trade+change_tech+change_demd)[1],0)==round(sum(delta_eps[1],na.rm=TRUE,0))){
    print(paste0("The structural decomposition elemtns MATCH the difference in emissions"))} else {
    print(paste0("The structural decomposition elemtns DO NOT match the difference in emissions"))}
    
    if(sda_method == "technology"){
      str_decomposition <- temp_emissions_str %>% rename("kgC02_eq_str"="kgC02_eq") %>% left_join(temp_emissions_end %>% 
      rename("kgC02_eq_end"="kgC02_eq"),by=c("cou","code")) %>% mutate(delta_emiss = kgC02_eq_end-kgC02_eq_str) %>% 
      unite(code,cou,code) %>% select(code,delta_emiss)  %>% left_join(change_input %>% t %>% as.data.frame %>% 
      set_rownames(c(as.vector(unlist(temp_emissions_str %>% select(cou,code) %>% unite(code,"cou","code",sep="_"))))) %>%
      set_names(c("change_input")) %>% rownames_to_column("code"),by=c("code")) %>%
      left_join(change_tech_tot %>% t %>% as.data.frame %>% set_rownames(c(as.vector(unlist(temp_emissions_str %>% 
      select(cou,code) %>% unite(code,"cou","code",sep="_"))))) %>% set_names(c("change_tech")) %>% rownames_to_column("code"),by=c("code")) %>%
      left_join(change_demd %>% t %>% as.data.frame %>% set_rownames(c(as.vector(unlist(temp_emissions_str %>% 
      select(cou,code) %>% unite(code,"cou","code",sep="_"))))) %>% set_names(c("change_demd")) %>% rownames_to_column("code"),by=c("code")) %>%
      mutate(check = round(delta_emiss - (change_input+change_tech+change_demd),6)) %>% separate(code,c("cou","code"),sep="_",extra="merge")
    }
    
    if(sda_method == "trade&technology"){
      str_decomposition <- temp_emissions_str %>% rename("kgC02_eq_str"="kgC02_eq") %>% left_join(temp_emissions_end %>% 
      rename("kgC02_eq_end"="kgC02_eq"),by=c("cou","code")) %>% mutate(delta_emiss = kgC02_eq_end-kgC02_eq_str) %>% 
      unite(code,cou,code) %>% select(code,delta_emiss)  %>% left_join(change_input %>% t %>% as.data.frame %>% 
      set_rownames(c(as.vector(unlist(temp_emissions_str %>% select(cou,code) %>% unite(code,"cou","code",sep="_"))))) %>%
      set_names(c("change_input")) %>% rownames_to_column("code"),by=c("code")) %>%
      left_join(change_trade %>% t %>% as.data.frame %>% set_rownames(c(as.vector(unlist(temp_emissions_str %>% 
      select(cou,code) %>% unite(code,"cou","code",sep="_"))))) %>% set_names(c("change_trade")) %>% rownames_to_column("code"),by=c("code")) %>%
      left_join(change_tech %>% t %>% as.data.frame %>% set_rownames(c(as.vector(unlist(temp_emissions_str %>% 
      select(cou,code) %>% unite(code,"cou","code",sep="_"))))) %>% set_names(c("change_tech")) %>% rownames_to_column("code"),by=c("code")) %>%
      left_join(change_demd %>% t %>% as.data.frame %>% set_rownames(c(as.vector(unlist(temp_emissions_str %>% 
      select(cou,code) %>% unite(code,"cou","code",sep="_"))))) %>% set_names(c("change_demd")) %>% rownames_to_column("code"),by=c("code")) %>%
      mutate(check = round(delta_emiss - (change_input+change_trade+change_tech+change_demd),6)) %>% separate(code,c("cou","code"),sep="_",extra="merge")
    }
    
  saveRDS(str_decomposition,file.path(getwd(),"datasets",paste0("str_decomposition_",sda_method,"_",sda_vector,".rds")))
} else{
  str_decomposition <- readRDS(file.path(getwd(),"datasets",paste0("str_decomposition_",sda_method,"_",sda_vector,".rds"))) %>% as_tibble()
}

  str_decomposition %>% filter(cou%in%c("ES30")) %>% select(-check,-cou) %>% left_join(str_decomposition %>% filter(cou%in%c("ES")) %>%
  select(-check,-cou),by=c("code")) %>% set_names(c("code",paste0("ES30_",c("emissions","input","trade","tech","demand")),
  paste0("ES_",c("emissions","input","trade","tech","demand")))) %>% mutate_if(is.double,~round(.,0)) %>%
  kbl(booktabs = T,"latex") %>% kable_styling(latex_options = c("hold_position","scale_down"),font_size = 8)
  
  names_list <- function(string_n){
  string_n[string_n=="Rest of the world"] <- "the rest of the World"
  string_n[string_n=="Rest of Spain"] <- "The rest of Spain"
  string_n_list <- paste0(paste0(string_n[1:(length(string_n)-1)],
  collapse=", "),", and ",string_n[length(string_n)]);return(string_n_list)}

  library(wesanderson);library(latex2exp);library(ggthemes)
  str_decomposition_plot <- str_decomposition %>% filter(cou%in%c("ES30")) %>% pivot_longer(`delta_emiss`:`change_demd`,names_to = "factor",values_to = "obs_value") %>%
  bind_rows(str_decomposition %>% filter(cou%in%c("ES")) %>% pivot_longer(`delta_emiss`:`change_demd`,names_to = "factor",values_to = "obs_value")) %>% 
    bind_rows(str_decomposition %>% filter(cou%in%c("ES30")) %>% pivot_longer(`delta_emiss`:`change_demd`,names_to = "factor",values_to = "obs_value") %>%
    bind_rows(str_decomposition %>% filter(cou%in%c("ES")) %>% pivot_longer(`delta_emiss`:`change_demd`,names_to = "factor",values_to = "obs_value")) %>%
    group_by(cou,factor) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% mutate(code="Total") %>% select(cou,code,factor,obs_value)) %>%
    select(-check) %>% mutate_at(c("factor"),~case_when(.=="delta_emiss"~"Total",.=="change_input"~"Intensity",.=="change_trade"~"Trade",
    .=="change_tech"~"Technology",.=="change_demd"~"Consumption")) %>% filter(cou=="ES30") %>% filter(!code=="T") %>%
      left_join(readxl::read_xlsx(file.path(getwd(),"datasets","classification_iots_mad.xlsx"),sheet = "City") %>%
      filter(digits_full>2) %>% distinct(nace_code,Definition_short_en),by=c("code"="nace_code")) %>% 
      mutate_at(c("Definition_short_en"),~if_else(code=="Total","Total",.)) 
    saveRDS(str_decomposition_plot,file.path(getwd(),"datasets","str_decomposition_plot.rds"))
  
    str_decomposition_plot %>%
    ggplot(aes(x=`obs_value`/1e03,y=factor(factor,levels=rev(c("Total","Intensity","Trade","Technology","Consumption"))),group=factor,fill=factor)) + 
    geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette="Dark2") + facet_wrap(~factor(Definition_short_en,
    levels=c(as.vector(unlist(str_decomposition_plot %>% distinct(code,Definition_short_en) %>% arrange(code) %>% select(Definition_short_en))))),
    ncol = 7,scales = "free_x") + labs(fill='') + ylab(TeX("Components of total Mt$\\CO_2$ growth")) + xlab(TeX("Mt$\\CO_2$")) + 
    guides(fill=guide_legend(nrow=1,reverse=TRUE)) + theme(text = element_text(size = 12),axis.ticks.x=element_blank(),axis.text = element_text(size = 10),
    axis.ticks.y=element_blank(),legend.position = "none",panel.border=element_blank(),legend.margin=margin(0,0,0,0))
    ggsave(file.path(getwd(),"paper","graphics",paste0("sda_decomposition.pdf")),height = 6,width = 12)
    ggsave(file.path(getwd(),"graphics",paste0("sda_decomposition.pdf")),height = 6,width = 12)
    
    str_decomposition_prct <- str_decomposition_plot %>% filter(!code=="Total") %>% left_join(temp_emissions_str,by=c("cou","code")) %>%
    mutate_at(c("obs_value"),~round(.*100/kgC02_eq,1)) %>% select(-kgC02_eq) %>% pivot_wider(names_from = "factor",
    values_from = "obs_value") %>% bind_rows(str_decomposition_plot %>% filter(code=="Total") %>% 
    left_join(temp_emissions_str %>% group_by(cou) %>% summarize_at(c("kgC02_eq"),~sum(.,na.rm=TRUE)),by=c("cou")) %>% 
    mutate_at(c("obs_value"),~round(.*100/kgC02_eq,1)) %>% select(-kgC02_eq) %>% pivot_wider(names_from = "factor",
    values_from = "obs_value"))
    saveRDS(str_decomposition_prct,file.path(getwd(),"datasets","str_decomposition_prct.rds"))
    
    str_decomposition_cou <- str_decomposition %>% pivot_longer(`delta_emiss`:`change_demd`,names_to = "factor",values_to = "obs_value") %>%
    group_by(cou,factor) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% select(cou,factor,obs_value) %>%
    bind_rows(str_decomposition %>% pivot_longer(`delta_emiss`:`change_demd`,names_to = "factor",values_to = "obs_value") %>%
    group_by(factor) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% mutate(cou="WORLD") %>% select(cou,factor,obs_value)) %>%
    mutate_at(c("factor"),~case_when(.=="delta_emiss"~"Total",.=="change_input"~"Intensity",.=="change_trade"~"Trade",
    .=="change_tech"~"Technology",.=="change_demd"~"Consumption")) %>% left_join(code_countries %>% select(CODE,NAME),
    by=c("cou"="CODE")) %>% mutate_at(c("NAME"),~if_else(cou=="WORLD","World total",.)) %>% 
    mutate_at(c("NAME"),~if_else(cou=="ES","Rest of Spain",.)) %>% 
    mutate_at(c("NAME"),~if_else(cou=="DE","Germany",.))
    
    saveRDS(str_decomposition_cou,file.path(getwd(),"datasets","str_decomposition_cou.rds"))
  
    str_decomposition_cou %>% filter(!NAME%in%c("Latvia","Lithuania","Romania","Hungary","Croatia","Malta",
    "Bulgaria","Estonia","Czech Republic","Denmark","Ireland","Slovakia","Slovenia")) %>%
    ggplot(aes(x=`obs_value`/1e03,y=factor(factor,levels=rev(c("Total","Intensity","Trade","Technology","Consumption"))),group=factor,fill=factor)) + 
    geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette="Dark2") + facet_wrap(~factor(NAME,levels=c(
    unique(sort(str_decomposition_cou$NAME)))),ncol = 7,scales = "free_x") + 
    labs(fill='') + ylab(TeX("Components of total Mt$\\CO_2$ growth")) + xlab(TeX("Mt$\\CO_2$")) + 
    guides(fill=guide_legend(nrow=1,reverse=TRUE)) + theme(text = element_text(size = 12),axis.ticks.x=element_blank(),axis.text = element_text(size = 10),
    axis.ticks.y=element_blank(),legend.position = "none",panel.border=element_blank(),legend.margin=margin(0,0,0,0))
    ggsave(file.path(getwd(),"paper","graphics",paste0("sda_decomposition_cou.pdf")),height = 6,width = 12)
    ggsave(file.path(getwd(),"graphics",paste0("sda_decomposition_cou.pdf")),height = 6,width = 12)
##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################