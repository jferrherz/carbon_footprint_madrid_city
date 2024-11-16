##################################################################################################################################
## [B] BASIC INPUTS FOR ESTIMATIONS - AUGMENTED MRIO TABLES 
##################################################################################################################################

#################### 
# [A] Aggregating FIGARO MRIO
#########################################################
#### ==> reduce the sectors from FIGARO to match city distribution
if(length(files_path_figaro[files_path_figaro=="city_mrio_industry.rds"])==0){
  #### [1] load FIGARO dataset and create a list of codes
  figaro_wio_ind <- readRDS(file.path(path_figaro_data,paste0("figaro_ind-by-ind_mrio_",
  as.numeric(str_sub(year_end_io,3,4))+2,"ed.rds")))
  cou_list  <- as.vector(unlist(figaro_wio_ind %>% separate(code,c("cou","code"),"_",extra="merge") %>% 
  filter(!cou%in%c("W2")) %>% distinct(cou)))
  code_list <- as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code)))
  code_set <- list();fd_set <- list()
  for(cou_n in cou_list){code_set[[cou_n]] <- paste0(cou_n,"_",code_list)}
  for(cou_n in cou_list){fd_set[[cou_n]] <- paste0(cou_n,"_",c("P3_S13","P3_S14","P3_S15","P51G","P5M"))}
  value_set <- c("W2_D21X31","W2_OP_RES","W2_OP_NRES","W2_D1","W2_D29X39","W2_B2A3G")
  data_io_mrio <- data.frame()
  for(year_n in seq(year_str_io,year_end_io)){
    #### [a] aggregate rows
    temp_data_mrio  <- figaro_wio_ind %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),"_",extra="merge") %>% left_join(code_iots_city %>% 
    distinct(nace_r2_64,nace_code),by=c("code"="nace_r2_64")) %>% group_by(cou,nace_code) %>% 
    select(-code,-year) %>% summarize_all(~sum(.,na.rm=TRUE)) %>% 
    unite(code,"cou","nace_code",sep="_") 
    #### [b] aggregate columns
    temp_data_mrio %<>% pivot_longer(`AR_A01`:`ZA_P5M`,names_to = "nace_r2_64",
    values_to = "value") %>% separate(nace_r2_64,c("cou","nace_r2_64"),"_",extra="merge") %>%
    left_join(code_iots_city %>% distinct(nace_r2_64,nace_code),by=c("nace_r2_64")) %>% 
    group_by(code,cou,nace_code) %>% summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% 
    unite(nace_r2_64,"cou","nace_code",sep="_") %>% pivot_wider(names_from = "nace_r2_64",
    values_from = "value")
    #### [c] reorder columns and rows in the original way
    temp_data_mrio %<>% arrange(factor(code, levels = c(as.vector(unlist(code_set)),
    as.vector(unlist(value_set))))) %>% select(code,all_of(as.vector(unlist(code_set))),
    all_of(as.vector(unlist(fd_set)))) %>% mutate(year = paste0(year_n))
    data_io_mrio %<>% bind_rows(temp_data_mrio)
    cat(paste0("\n Output: FIGARO ",year_n," world table aggregated up to Madrid city NACE digits \n"))
  }
  #### [2] save the aggregated dataset
  saveRDS(data_io_mrio,file.path(getwd(),"datasets","city_mrio_industry.rds"))
} else {
  city_mrio_industry <- readRDS(file.path(getwd(),"datasets","city_mrio_industry.rds")) %>% as_tibble
}

if(length(files_path_figaro[files_path_figaro=="city_mrio_product.rds"])==0){
  #### [1] load FIGARO dataset and create a list of codes
  figaro_wio_prd <- readRDS(file.path(path_figaro_data,paste0("figaro_prod-by-prod_mrio_",
  as.numeric(str_sub(year_end_io,3,4))+2,"ed.rds")))
  cou_list  <- as.vector(unlist(figaro_wio_prd %>% separate(code,c("cou","code"),"_",extra="merge") %>% 
  filter(!cou%in%c("W2")) %>% distinct(cou)))
  code_list <- as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% distinct(cpa_code)))
  code_set <- list();fd_set <- list()
  for(cou_n in cou_list){code_set[[cou_n]] <- paste0(cou_n,"_",code_list)}
  for(cou_n in cou_list){fd_set[[cou_n]] <- paste0(cou_n,"_",c("P3_S13","P3_S14","P3_S15","P51G","P5M"))}
  value_set <- c("W2_D21X31","W2_OP_RES","W2_OP_NRES","W2_D1","W2_D29X39","W2_B2A3G")
  data_io_mrio <- data.frame()
  for(year_n in seq(year_str_io,year_end_io)){
    #### [a] aggregate rows
    temp_data_mrio  <- figaro_wio_prd %>% filter(year==year_n) %>% 
    separate(code,c("cou","code"),"_",extra="merge") %>% left_join(code_iots_city %>% 
    distinct(cpa_r2_64,cpa_code),by=c("code"="cpa_r2_64")) %>% group_by(cou,cpa_code) %>% 
    select(-code,-year) %>% summarize_all(~sum(.,na.rm=TRUE)) %>% 
    unite(code,"cou","cpa_code",sep="_") 
    #### [b] aggregate columns
    temp_data_mrio %<>% pivot_longer(`AR_CPA_A01`:`ZA_P5M`,names_to = "cpa_r2_64",
    values_to = "value") %>% separate(cpa_r2_64,c("cou","cpa_r2_64"),"_",extra="merge") %>%
    left_join(code_iots_city %>% distinct(cpa_r2_64,cpa_code),by=c("cpa_r2_64")) %>% 
    group_by(code,cou,cpa_code) %>% summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% 
    unite(cpa_r2_64,"cou","cpa_code",sep="_") %>% pivot_wider(names_from = "cpa_r2_64",
    values_from = "value")
    #### [c] reorder columns and rows in the original way
    temp_data_mrio %<>% arrange(factor(code, levels = c(as.vector(unlist(code_set)),
    as.vector(unlist(value_set))))) %>% select(code,all_of(as.vector(unlist(code_set))),
    all_of(as.vector(unlist(fd_set)))) %>% mutate(year = paste0(year_n))
    data_io_mrio %<>% bind_rows(temp_data_mrio)
    cat(paste0("\n Output: FIGARO ",year_n," world table aggregated up to Madrid city CPA digits \n"))
  }
  #### [2] save the aggregated dataset
  saveRDS(data_io_mrio,file.path(getwd(),"datasets","city_mrio_product.rds"))
} else {
  city_mrio_product <- readRDS(file.path(getwd(),"datasets","city_mrio_product.rds")) %>% as_tibble
}

#################### 
# [B] Include Madrid City in the MRIO
#########################################################
#### [METHOD] Manual adjustment
#########################################################
if(series_ras_expand == FALSE){
if(length(files_path_figaro[files_path_figaro=="city_mrio_industry_final.rds"])==0){
data_io_mrio <- data.frame()
for(year_n in seq(year_str,year_end)){
  #### [A] extract the country matrix at city aggregation level
  #### [counter_area es la demanda de cada país sobre RoW, invertido orden de demanda]
  temp_city_mrio   <- city_mrio_industry %>% filter(year == year_n) %>% select(-year) %>% 
  filter(!grepl("W2",code)) %>% column_to_rownames("code") %>% select(`AR_A`:`ZA_T`) %>% 
  rownames_to_column("code") %>% separate(code,c("ref_area","code"),extra = "merge") %>% 
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter_area",values_to = "obs_value") %>%
  separate(counter_area,c("counter_area","counter_code"),extra = "merge")
  temp_a_mrio <- temp_city_mrio %>% left_join(city_mrio_industry %>% filter(year == year_n) %>% 
  select(code,`AR_A`:`ZA_T`) %>% column_to_rownames("code") %>% summarize_all(~sum(.,na.rm=TRUE)) %>% 
  pivot_longer(`AR_A`:`ZA_T`,names_to = "code",values_to = "obs_value_x") %>%separate(code,c("counter_area",
  "counter_code"),extra = "merge"),by=c("counter_area","counter_code")) %>% 
  mutate_at(c("obs_value"),~./obs_value_x) %>% select(-obs_value_x)
  code_list <- temp_city_mrio %>% distinct(ref_area,code) %>% bind_rows(temp_city_mrio %>% distinct(ref_area,code) %>% 
  filter(ref_area=="ES") %>% mutate_at(c("ref_area"),~paste0("ES30"))) %>% arrange(ref_area,code) %>% 
  unite(code,"ref_area","code")
  
  #### [B] derive the two-region matrices from the national matrix
  #### ==> computation of the nation, city, and rest of the nation IOT
    #### [=> national matrix]
    Z_n <- temp_city_mrio %>% filter(ref_area == "ES"&counter_area  == "ES") %>% unite(counter_area,"counter_area","counter_code") %>%
    unite(code,"ref_area","code") %>% pivot_wider(names_from = "counter_area",values_from = "obs_value") %>% column_to_rownames("code")
    x_n  <- city_mrio_industry %>% filter(year == year_n) %>% select(code,`ES_A`:`ES_T`) %>% column_to_rownames("code") %>% 
    summarize_all(~sum(.,na.rm=TRUE)) %>% pivot_longer(`ES_A`:`ES_T`,names_to = "code",values_to = "P1") %>% mutate_at(c("P1"),~1/.) %>% 
    mutate_at(c("P1"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% select(P1) %>% unlist %>% as.vector
    A_nn <- as.matrix(Z_n)%*%diag(x_n) %>% set_colnames(c(colnames(Z_n)))
    
    #### [=> regional matrix]
    Z_rr <- city_iots_industry %>% filter(year==year_n&code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% select(nace_code))))) %>% 
    select(code,all_of(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% select(nace_code))))) %>% pivot_longer(`A`:`T`,
    names_to = "counter_code",values_to = "obs_value") %>% mutate(ref_area = "ES30", counter_area = "ES30") %>% 
    select(ref_area,code,counter_area,counter_code,obs_value) %>% unite(code,"ref_area","code") %>% unite(counter_area,"counter_area","counter_code") %>% 
    as_tibble %>% mutate_at(c("obs_value"),~./1e03) %>% pivot_wider(names_from = "counter_area",values_from = "obs_value") %>% column_to_rownames("code")
    Z_rr_m <- city_iots_industry %>% filter(year==year_n&code%in%c(paste0(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% 
    select(nace_code))),"_IM"))) %>% select(code,all_of(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% select(nace_code))))) %>% 
    pivot_longer(`A`:`T`,names_to = "counter_code",values_to = "obs_value") %>% mutate(ref_area = "ES30", counter_area = "ES30") %>% 
    select(ref_area,code,counter_area,counter_code,obs_value) %>% unite(code,"ref_area","code") %>% unite(counter_area,"counter_area","counter_code") %>% 
    as_tibble %>% mutate_at(c("obs_value"),~./1e03) %>% mutate_at(c("code"),~stringr::str_remove(.,fixed("_IM"))) %>%
    pivot_wider(names_from = "counter_area",values_from = "obs_value") %>% column_to_rownames("code")
    x_r  <- city_iots_industry %>% filter(year==year_n&code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% 
    select(nace_code))))) %>% mutate_at(c("P1"),~1/(./1e03)) %>% mutate_at(c("P1"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% 
    select(P1) %>% unlist %>% as.vector
    A_rr   <- as.matrix(Z_rr)%*%diag(x_n) %>% set_colnames(c(colnames(Z_rr))) %>% set_rownames(c(rownames(Z_rr)))
    A_rr_m <- as.matrix(Z_rr_m)%*%diag(x_r) %>% set_colnames(c(colnames(Z_rr))) %>% set_rownames(c(rownames(Z_rr)))
    
    #### [=> rest of the nation (RoN) matrix]
    Z_ron <- Z_n - Z_rr
    x_ron <- city_mrio_industry %>% filter(year == year_n) %>% select(code,`ES_A`:`ES_T`) %>% column_to_rownames("code") %>% 
    summarize_all(~sum(.,na.rm=TRUE)) %>% pivot_longer(`ES_A`:`ES_T`,names_to = "code",values_to = "P1") %>%
    separate(code,c("cou","code"),extra = "merge") %>% rename("nation"="P1") %>% left_join(city_iots_industry %>% filter(year==year_n&
    code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% select(nace_code))))) %>% select(code,P1) %>%
    rename("city"="P1"),by=c("code")) %>% mutate_at(c("city"),~./1e03) %>% transmute(P1 = nation - city)
    A_ron <- as.matrix(Z_ron)%*%diag(as.vector(unlist(x_n))) %>% set_colnames(c(colnames(Z_ron))) %>% set_rownames(c(rownames(Z_ron)))
    
    #### [=> rest of the world (RoW) matrices for the regional flows]
    x_row_r  <- city_iots_industry %>% filter(year==year_n&code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% 
    select(nace_code)))))  %>% mutate_at(c("P1"),~./1e03) %>% select(P1) %>% unlist %>% as.vector
    Z_row_x <- temp_city_mrio  %>% filter(ref_area == "ES"&!counter_area == "ES") %>% mutate_at(c("ref_area","counter_area"),
    ~if_else(.=="ES","ES30",.)) %>% filter(ref_area == "ES30") %>% unite(code,"ref_area","code",sep="_") %>% 
    group_by(code,counter_code) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% mutate_at(c("counter_code"),
    ~paste0("ES30_",counter_code)) %>% pivot_wider(names_from = "counter_code",values_from = "obs_value") %>%
    column_to_rownames("code")
    A_row_x <- as.matrix(Z_row_x)%*%diag(x_n) %>% set_colnames(c(colnames(Z_row_x)))
    Z_row_m <- temp_city_mrio  %>% filter(!(ref_area == "ES"&counter_area == "ES")) %>% mutate_at(c("ref_area","counter_area"),
    ~if_else(.=="ES","ES30",.)) %>% filter(counter_area == "ES30") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>% 
    ungroup %>% group_by(code,counter_code) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% mutate_at(c("code"),
    ~paste0("ES30_",code)) %>% pivot_wider(names_from = "counter_code",values_from = "obs_value")  %>%
    column_to_rownames("code")
    A_row_m <- as.matrix(Z_row_m)%*%diag(x_n) %>% set_colnames(c(colnames(Z_row_m)))
    
  #### [C] Derive the matrix of imports (Z_nr) as the share of RoW demand over RoN total inputs,
  #### which should keep the proportions of the relationship between available supply of
  #### goods at the domestic level and the rest of the world
  #### [a] calculate the import distribution between national and foreign trade
  matrix_import_distr <- Z_n %>% as.data.frame() %>% rownames_to_column("code") %>% pivot_longer(`ES_A`:`ES_T`,names_to = "counter_code",values_to = "RoN") %>% 
  separate(code,c("ref_area","code"),extra = "merge") %>% separate(counter_code,c("counter_area","counter_code"),extra = "merge") %>%
  left_join(temp_city_mrio  %>% filter(!(ref_area == "ES"&counter_area == "ES")) %>% filter(counter_area == "ES") %>% 
  unite(counter_code,"counter_area","counter_code",sep="_") %>% ungroup %>% group_by(code,counter_code) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% mutate_at(c("code"),~paste0("ES_",code)) %>% rename("RoW" = "obs_value") %>%
  separate(code,c("ref_area","code"),extra = "merge") %>% separate(counter_code,c("counter_area","counter_code"),extra = "merge"),
  by=c("ref_area","counter_area","code","counter_code")) %>% ungroup %>% group_by(counter_area,counter_code) %>% 
  mutate(Total = RoN+RoW) %>% mutate_at(c("RoW","RoN"),~./sum(Total,na.rm=TRUE))%>% mutate_at(c("Total"),~./sum(Total,na.rm=TRUE)) %>%
  mutate(check = RoN + RoW)
  
  #### [b] calculate initial RoN and RoW matrices
  Z_ron_m <- matrix_import_distr %>% select(ref_area:counter_code,RoN) %>% mutate_at(c("counter_area"),~paste0(.,"30")) %>%
  unite(code,"ref_area","code",sep="_") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
  left_join(Z_rr_m %>% colSums %>% as.data.frame() %>% rename("P2"=".") %>% rownames_to_column("code"),by=c("counter_code"="code")) %>%
  mutate_at(c("RoN"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% mutate_at(c("RoN"),~.*P2) %>% select(code:RoN) %>%
  pivot_wider(names_from = "counter_code",values_from = "RoN") %>% column_to_rownames("code")
  Z_row_m <- matrix_import_distr %>% select(ref_area:counter_code,RoW) %>% mutate_at(c("counter_area"),~paste0(.,"30")) %>%
  unite(code,"ref_area","code",sep="_") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
  left_join(Z_rr_m %>% colSums %>% as.data.frame() %>% rename("P2"=".") %>% rownames_to_column("code"),by=c("counter_code"="code")) %>%
  mutate_at(c("RoW"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% mutate_at(c("RoW"),~.*P2) %>% select(code:RoW) %>%
  pivot_wider(names_from = "counter_code",values_from = "RoW") %>% column_to_rownames("code")
  
  #### [c] Calculate D21X31 vector and substruct it from the total imports scaled to to P2
  import_vec <- nio.mad.matrix(year_n,"",'M',"D","city") %>% rownames_to_column("code") %>% as_tibble %>% filter(code%in%c(as.vector(unlist(code_iots_city %>%
  filter(type=="sector") %>% distinct(nace_code))),paste0(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code))),"_IM"))) %>%
  summarize_at(c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code)))),~sum(.,na.rm=TRUE)) %>% 
  pivot_longer(`A`:`T`,names_to = "code",values_to = "P7") %>% left_join(nio.mad.matrix(year_n,"",'M',"D","city") %>% rownames_to_column("code") %>% 
  as_tibble %>% filter(code%in%c("D21X31")) %>% select(all_of(as.vector(unlist(code_iots_city %>%filter(type=="sector") %>% distinct(nace_code))))) %>% 
  pivot_longer(`A`:`T`,names_to = "code",values_to = "D21X31"),by=c("code")) %>% mutate(D21X31s = D21X31/P7) %>% mutate_at(c("code"),~paste0("ES30_",.)) %>%
  left_join(colSums(Z_rr_m)  %>% as.data.frame %>% rownames_to_column("code") %>%set_names(c("code","P7M")),by=c("code")) %>%
  left_join(colSums(Z_ron_m) %>% as.data.frame %>% rownames_to_column("code") %>%set_names(c("code","P7M_ron")),by=c("code")) %>%
  left_join(colSums(Z_row_m) %>% as.data.frame %>% rownames_to_column("code") %>%set_names(c("code","P7M_row")),by=c("code")) %>%
  mutate_at(c("D21X31"),~D21X31s * (P7M_ron+P7M_row)) %>% mutate(P70 = rowSums(across(`P7M_ron`:`P7M_row`),na.rm=TRUE)) %>% 
  mutate_at(c("P7"),~(P7M_ron+P7M_row)-D21X31) %>% mutate_at(c("P7M","P7M_ron","P7M_row"),~P7 * (. / P70)) %>%
  mutate(P7B = rowSums(across(`P7M`:`P7M_row`),na.rm=TRUE)) %>% select(code,`P7M`:`P7M_row`,D21X31) %>% 
  column_to_rownames("code") %>% mutate_all(~if_else(is.na(.),0,.))
  
  #### [d] rescale the three import matrices
  Z_rr_m  <- as.matrix(Z_rr_m %>% mutate_all(~./sum(.,na.rm=TRUE)) %>% mutate_all(~if_else(is.na(.),0,.)))%*%diag(as.vector(import_vec$P7M)) %>%
  set_colnames(c(colnames(Z_row_m)))
  Z_ron_m <- as.matrix(Z_ron_m %>% as.data.frame %>%mutate_all(~./sum(.,na.rm=TRUE)) %>% 
  mutate_all(~if_else(is.na(.),0,.)))%*%diag(as.vector(import_vec$P7M_ron)) %>% set_colnames(c(colnames(Z_row_m)))
  Z_row_m <- as.matrix(temp_city_mrio  %>% filter(!(ref_area == "ES"&counter_area == "ES")) %>% mutate_at(c("ref_area","counter_area"),
  ~if_else(.=="ES","ES30",.)) %>% filter(counter_area == "ES30") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
  unite(code,"ref_area","code",sep="_") %>% ungroup %>% group_by(counter_code) %>% mutate_at(c("obs_value"),~./sum(.,na.rm=TRUE)) %>%
  mutate_at(c("obs_value"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% pivot_wider(names_from = "counter_code",values_from = "obs_value") %>%
  column_to_rownames("code"))%*%diag(as.vector(import_vec$P7M_row)) %>% set_colnames(c(colnames(Z_row_m)))
  if(mean(round((colSums(Z_ron_m)+colSums(Z_row_m))/colSums(Z_rr_m),10),na.rm=TRUE)==1){ 
  cat("the RoN and RoW import matrices match the imported domestic (local) totals") } else {
  cat("the RoN and RoW import matrices DO NOT the imported domestic (local) totals")}
  
    #### NECESITO COMPROBAR EN X Y M SI es DEBERíA ESTAR PRESENTE, COMO EN z_ROW_m ????
    #### [ Matrix Znr ]
    #### [==> 28x(28x42?) matrix with total imported inputs to the local economy
    Z_nr    <- as.data.frame(Z_ron_m) %>% bind_rows(as.data.frame(Z_row_m))
    #### [==> I need to substract this from the RoN matrix
    Z_nr_ron <- temp_city_mrio  %>% filter(!ref_area=="ES") %>% filter(counter_area == "ES") %>% rename("ES"="obs_value") %>%
    unite(counter_code,"counter_area","counter_code",sep="_") %>% separate(counter_code,c("counter_area","counter_code"),extra = "merge") %>%
    left_join(Z_nr %>% rownames_to_column("code") %>% separate(code,c("ref_area","code"),extra = "merge") %>% filter(!ref_area == "ES") %>%
    pivot_longer(`ES30_A`:`ES30_T`,names_to = "counter_code",values_to = "ES30") %>% separate(counter_code,c("counter_area","counter_code"),
    extra = "merge") %>% select(-counter_area),by=c("ref_area","code","counter_code")) %>% mutate_at(c("ES"),~.-ES30) %>% select(ref_area:ES) %>% 
    unite(code,"ref_area","code",sep="_") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
    pivot_wider(names_from = "counter_code",values_from = "ES") %>% column_to_rownames("code")
    
    # temp_city_mrio  %>% filter(!ref_area=="ES") %>% filter(counter_area == "ES") %>% rename("ES"="obs_value") %>%
    # unite(counter_code,"counter_area","counter_code",sep="_") %>% separate(counter_code,c("counter_area","counter_code"),extra = "merge") %>% summarize(sum(ES))
  
  #### [D] Derive the matrix of exports as the share of RoW demand over RoN total outputs,
  #### which should keep the proportions of the relationship between available supply of
  #### goods at the domestic level and the rest of the world
  #### [==> this is a challenge because (a) we have less information on distribution and
  #### (b) we do not have a total value, which we can proxy with the trade balance of the region
  #### [METHODOLOGY] using the regional trade balance, I correct the total for imports to find
  #### the total for exports, which I then multiply by the regional distribution of exports (not level)
  xm_cobertura <- readxl::read_xlsx(file.path(getwd(),"datasets","comercio_exterior_cmadrid.xlsx"))
  fd_exports <- nio.city.matrix('ES',year_n,"",'P6',"industry") %>% rownames_to_column("code") %>% 
  left_join(nio.mad.matrix(year_n,"",'P6',"D","city") %>% rownames_to_column("code"),by=c("code")) %>%
  set_names(c("code","nation","region")) %>% mutate_at(c("nation","region"),~./sum(.,na.rm=TRUE)) %>%
  left_join(Z_nr %>% colSums() %>% as.data.frame %>% rownames_to_column("code") %>% rename("P7"=".") %>%
  separate(code,c("ref_area","code"),extra = "merge"),by=c("code")) %>% left_join(import_vec %>%
  rownames_to_column("code") %>% separate(code,c("ref_area","code"),extra = "merge") %>%
  select(ref_area,code,D21X31),by=c("ref_area","code")) %>% mutate_at(c("P7"),~.+D21X31) %>%
  mutate(P6 = region * P7)
  fd_exports %<>% left_join(xm_cobertura %>% filter(year == year_n) %>% select(tasa_cobertura) %>% mutate(ref_area = "ES30"),
  by=c("ref_area")) %>% mutate_at(c("P6"),~sum(P7)/tasa_cobertura) %>%mutate_at(c("P6"),~. * region) %>%
  select(ref_area,code,P7,P6)
  
  matrix_export_distr <- Z_n %>% as.data.frame() %>% rownames_to_column("code") %>% pivot_longer(`ES_A`:`ES_T`,names_to = "counter_code",
  values_to = "RoN") %>% separate(code,c("ref_area","code"),extra = "merge") %>% separate(counter_code,c("counter_area","counter_code"),extra = "merge") %>%
  mutate_at(c("ref_area"),~paste0("ES30")) %>% left_join(temp_city_mrio  %>% filter(!(ref_area == "ES"&counter_area == "ES")) %>% 
  filter(ref_area == "ES") %>% ungroup %>% group_by(ref_area,code,counter_code) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% 
  mutate_at(c("ref_area"),~paste0("ES30")) %>% rename("RoW" = "obs_value"),by=c("ref_area","code","counter_code")) %>% ungroup %>% 
  group_by(ref_area,code) %>% mutate(Total = RoN+RoW) %>% mutate_at(c("RoW","RoN"),~./sum(Total,na.rm=TRUE)) %>% 
  mutate_at(c("Total"),~./sum(Total,na.rm=TRUE)) %>% mutate(check = RoN + RoW)
  
  Z_ron_x <- matrix_export_distr %>% select(ref_area:counter_code,RoN) %>% left_join(fd_exports,by=c("ref_area","code")) %>%
  unite(code,"ref_area","code",sep="_") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
  mutate_at(c("RoN"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% mutate_at(c("RoN"),~.*P6) %>% select(code:RoN) %>%
  pivot_wider(names_from = "counter_code",values_from = "RoN") %>% column_to_rownames("code")
  
  Z_row_x <- matrix_export_distr %>% select(ref_area:counter_code,RoW) %>% left_join(fd_exports,by=c("ref_area","code")) %>% 
  unite(code,"ref_area","code",sep="_") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
  mutate_at(c("RoW"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% mutate_at(c("RoW"),~.*P6) %>% select(code:RoW) %>%
  pivot_wider(names_from = "counter_code",values_from = "RoW") %>% column_to_rownames("code");Z_row_x_check <- Z_row_x
  
  Z_row_x <- diag(rowSums(Z_row_x))%*%as.matrix(temp_city_mrio  %>% filter(!(ref_area == "ES"&counter_area == "ES")) %>% filter(ref_area == "ES") %>% ungroup %>% 
  mutate_at(c("ref_area"),~paste0("ES30")) %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
  unite(code,"ref_area","code",sep="_") %>% ungroup %>% group_by(code) %>% mutate_at(c("obs_value"),~./sum(.,na.rm=TRUE)) %>%
  mutate_at(c("obs_value"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% pivot_wider(names_from = "counter_code",values_from = "obs_value") %>%
  column_to_rownames("code"))
  if(mean(round((rowSums(Z_ron_x)+rowSums(Z_row_x_check))/fd_exports$P6,10),na.rm=TRUE)==1){ 
  cat("the RoN and RoW export matrices match the exported domestic (local) totals") } else {
  cat("the RoN and RoW export matrices DO NOT the exported domestic (local) totals")}
      
    #### [ Matrix Zrn ]
    #### [==> (28x42?)x28 matrix with total exported inputs to the local economy
    Z_rn    <- as.data.frame(Z_ron_x) %>% bind_cols(as.data.frame(Z_row_x))
    #### [==> I need to substract this from the RoN matrix
    Z_rn_ron <- temp_city_mrio  %>% filter(!(ref_area == "ES"&counter_area == "ES")) %>% filter(ref_area == "ES") %>% 
    rename("ES"="obs_value") %>% left_join(Z_rn %>% rownames_to_column("code") %>% separate(code,c("ref_area","code"),extra = "merge") %>% 
    pivot_longer(`ES_A`:`ZA_T`,names_to = "counter_code",values_to = "ES30") %>% separate(counter_code,c("counter_area","counter_code"),
    extra = "merge") %>% filter(!counter_area == "ES") %>% mutate_at(c("ref_area"),~paste0("ES")),by=c("ref_area","code","counter_area","counter_code")) %>% 
    mutate_at(c("ES"),~.-ES30) %>% select(ref_area:ES) %>% unite(code,"ref_area","code",sep="_") %>% unite(counter_code,"counter_area","counter_code",sep="_") %>%
    pivot_wider(names_from = "counter_code",values_from = "ES") %>% column_to_rownames("code")
    
    #### [Plot of the differences in the sectoral distribution of exports between the national and regional tables]
    nio.city.matrix('ES',year_n,"",'P6',"industry") %>% rownames_to_column("code") %>% left_join(
    nio.mad.matrix(year_n,"",'P6',"D","city") %>% rownames_to_column("code"),by=c("code")) %>%
    set_names(c("code","nation","region")) %>% mutate_at(c("nation","region"),~./sum(.,na.rm=TRUE)) %>%
    pivot_longer(`nation`:`region`,names_to = "geo",values_to = "obs_value") %>%
    ggplot(aes(x=obs_value, y=code, fill=geo)) + geom_bar(stat="identity", position=position_dodge())
  
  #### recomputation of rest of the nation by substracting the regional flows
  #### sum(Z_n)-sum(Z_rr)-sum(Z_rn[1:28,1:28])-sum(Z_nr[1:28,1:28])
  #### sum(Z_n-Z_rr-t(Z_rn[1:28,1:28])-Z_nr[1:28,1:28])
  #### Z_iot_ron <- Z_n-Z_rr-t(Z_rn[1:28,1:28])-Z_nr[1:28,1:28]
  Z_ron <- Z_n-Z_rr-t(Z_rn %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`ES_A`:`ZA_T`,names_to = "counter_code",
  values_to = "obs_value") %>% separate(counter_code,c("counter_area","counter_code"),extra = "merge") %>% filter(counter_area=="ES") %>% 
  unite(counter_code,"counter_area","counter_code",sep="_") %>% pivot_wider(names_from = "counter_code",values_from = "obs_value")  %>% 
  column_to_rownames("code"))-(Z_nr %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`ES30_A`:`ES30_T`,names_to = "counter_code",
  values_to = "obs_value") %>% separate(code,c("ref_area","code"),extra = "merge") %>% filter(ref_area=="ES") %>% 
  unite(code,"ref_area","code",sep="_") %>% pivot_wider(names_from = "counter_code",values_from = "obs_value")  %>% 
  column_to_rownames("code"))
  
  #### [E] bind together the four matrices with the world matrix and add final demand and value added vectors
  #### [==> check that sum(Z_mrio)/temp_city_mrio %>% summarize(sum(obs_value)) = 1]
  Z_mrio <- temp_city_mrio  %>% filter(!ref_area == "ES"&!counter_area == "ES") %>% 
  unite(counter_code,"counter_area","counter_code",sep="_") %>% unite(code,"ref_area","code",sep="_") %>%
  bind_rows(Z_ron %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`ES_A`:`ES_T`,names_to = "counter_code",values_to = "obs_value")) %>%
  bind_rows(Z_rr %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`ES30_A`:`ES30_T`,names_to = "counter_code",values_to = "obs_value")) %>%
  bind_rows(Z_nr %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`ES30_A`:`ES30_T`,names_to = "counter_code",values_to = "obs_value") %>%
  separate(code,c("ref_area","code"),extra = "merge") %>% unite(code,"ref_area","code",sep="_")) %>% #filter(!ref_area=="ES") %>% 
  bind_rows(Z_rn %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`ES_A`:`ZA_T`,names_to = "counter_code",values_to = "obs_value") %>%
  separate(counter_code,c("counter_area","counter_code"),extra = "merge") %>% unite(counter_code,"counter_area","counter_code",sep="_")) %>% # filter(!counter_area=="ES") %>% 
  bind_rows(Z_rn_ron %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter_code",values_to = "obs_value")) %>%
  bind_rows(Z_nr_ron %>% as.data.frame %>% rownames_to_column("code") %>%  pivot_longer(`ES_A`:`ES_T`,names_to = "counter_code",values_to = "obs_value")) %>%
  separate(counter_code,c("counter_area","counter_code"),extra = "merge") %>% arrange(counter_area,counter_code) %>%
  unite(counter_code,"counter_area","counter_code",sep="_") %>% pivot_wider(names_from = "counter_code",values_from = "obs_value") %>%
  separate(code,c("ref_area","code"),extra = "merge") %>% arrange(ref_area,code) %>% unite(code,"ref_area","code",sep="_") %>%
  as.data.frame %>% column_to_rownames("code")
  if(round(sum(Z_mrio)/temp_city_mrio %>% summarize(sum(obs_value)),10)==1){ 
  cat("the new and original MRIOs match") } else {cat("the new and original MRIOs DO NOT match")}
    #### adds the vectors of value added and total output, gdp, and final demand (separate the vector of consumption as well
    #### to include the municipal level later on): P3_S14, P3AP6 (P1 - P2), P1, GDP
    #### [a] ==> P1 & P2
    Z_mrio %<>% rownames_to_column("code") %>% left_join(Z_mrio %>% rownames_to_column("code") %>% mutate(P2 = rowSums(across(`AR_A`:`ZA_T`),
    na.rm=TRUE)) %>% select(code,P2) %>% left_join(city_mrio_industry %>% filter(year == year_n) %>% select(-year) %>% 
    mutate(P1 = rowSums(across(`AR_A`:`ZA_P5M`),na.rm=TRUE)) %>% select(code,P1) %>% separate(code,c("ref_area","code"),extra = "merge") %>% 
    filter(!ref_area=="ES") %>% unite(code,"ref_area","code",sep="_") %>% bind_rows(city_mrio_industry %>% filter(year == year_n) %>% select(-year) %>% 
    mutate(P1 = rowSums(across(`AR_A`:`ZA_P5M`),na.rm=TRUE)) %>% select(code,P1) %>% separate(code,c("ref_area","code"),extra = "merge") %>% 
    filter(ref_area=="ES") %>% rename("ES"="P1") %>% left_join(accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>% 
    select(-code) %>% filter(year==year_n) %>% rename("code"="var","counter_code"="nace_code","obs_value"="value") %>% mutate(counter_area = "ES30") %>% 
    select(code,counter_area,counter_code,obs_value) %>% pivot_wider(names_from = "counter_area",values_from = "obs_value") %>% 
    filter(code%in%c("P1")) %>% pivot_wider(names_from = "code",values_from = "ES30") %>% rename("ES30"="P1") %>% mutate_at(c("ES30"),~./1e03),
    by=c("code"="counter_code")) %>% mutate_at(c("ES"),~. - ES30) %>% select(-ref_area) %>% pivot_longer(`ES`:`ES30`,names_to = "ref_area",
    values_to = "P1") %>% arrange(ref_area) %>% unite(code,"ref_area","code",sep="_")) %>% arrange(code),by=c("code")),by=c("code"))
    #### [b] ==> P3AP6
    Z_mrio %<>% mutate(P3AP5 = P1 - P2)
    #### [c] ==> GDP
    gdp_cou <- data.frame();for(cou_n in as.vector(unlist(city_mrio_industry %>% separate(code,c("ref_area","code"),extra = "merge") %>%
    filter(!grepl("W2",ref_area)) %>% distinct(ref_area)))){gdp_cou %<>% bind_rows(nio.city.matrix(cou_n,year_n,"",'GDP',"industry") %>%
    rownames_to_column("code") %>% mutate(ref_area = cou_n))}
      #### [ATTENTION: rescaling city vector]
      Z_mrio %<>% left_join(gdp_cou %>% select(ref_area,code,GDP) %>% unite(code,"ref_area","code",sep="_") %>%
      bind_rows(accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>% filter(var=="GDP") %>%
      filter(year==year_n) %>% select(nace_code,value) %>% mutate(ref_area = "ES30") %>% unite(code,"ref_area","nace_code",sep="_") %>% 
      rename("GDP"="value") %>% select(code,GDP) %>% mutate_at(c("GDP"),~./1e3)),by=c("code"))
    #### [d] ==> GVA
    Z_mrio %<>% bind_rows(city_mrio_industry %>% filter(year == year_n) %>% select(-year) %>% filter(grepl("W2",code)) %>% 
    select(code,`ES_A`:`ES_T`) %>% separate(code,c("ref_area","code"),extra = "merge") %>% select(-ref_area) %>%
    pivot_longer(`ES_A`:`ES_T`,names_to = "counter_area",values_to = "obs_value") %>% separate(counter_area,
    c("counter_area","counter_code"),extra = "merge") %>% pivot_wider(names_from = "counter_area",values_from = "obs_value") %>%
      #### bind municipal accounts [note that vectors are rescaled to match the order of magnitud in FIGARO]
      left_join(accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>% select(-code) %>% 
      filter(year==year_n) %>% rename("code"="var","counter_code"="nace_code","obs_value"="value") %>% mutate(counter_area = "ES30") %>% 
      select(code,counter_area,counter_code,obs_value) %>% pivot_wider(names_from = "counter_area",values_from = "obs_value") %>%
      mutate_at(c("code"),~stringr::str_replace(.,"D29XD39","D29X39")) %>% bind_rows(import_vec %>% rownames_to_column("counter_code") %>%
      mutate(code = "D21X31") %>% rename("ES30"="D21X31") %>% select(counter_code,ES30,code) %>% mutate_at(c("counter_code"),
      ~stringr::str_remove(.,fixed("ES30_")))),by=c("code","counter_code")) %>% mutate_at(c("ES30"),~if_else(is.na(.),0,.)) %>% 
      mutate_at(c("ES30"),~if_else(code=="D21X31",.,./1e03)) %>% mutate(ES0 = ES) %>% group_by(code) %>% mutate_at(c("ES"),~ES-ES30) %>% ungroup() %>%
    pivot_longer(`ES`:`ES30`,names_to = "counter_area",values_to = "obs_value") %>% arrange(counter_area) %>%
    select(code,counter_area,counter_code,obs_value) %>% bind_rows(city_mrio_industry %>% filter(year == year_n) %>% 
    select(-year) %>% filter(grepl("W2",code)) %>%  select(code,`AR_A`:`ZA_T`) %>% separate(code,c("ref_area","code"),extra = "merge") %>% 
    select(-ref_area) %>% pivot_longer(`AR_A`:`ZA_T`,names_to = "counter_area",values_to = "obs_value") %>% separate(counter_area,
    c("counter_area","counter_code"),extra = "merge") %>% filter(!counter_area=="ES")) %>% arrange(counter_area) %>%
    unite(counter_code,"counter_area","counter_code",sep="_") %>% pivot_wider(names_from = "counter_code",values_from = "obs_value") %>%
    select(code,`AR_A`:`ZA_T`)) %>% select(code,`AR_A`:`ZA_T`,P2,P3AP5,GDP,P1) %>% column_to_rownames("code")
    Z_mrio %<>% bind_rows(Z_mrio %>% select(`AR_A`:`ZA_T`) %>% summarize_all(~sum(.,na.rm=TRUE)) %>% mutate(code = "P1") %>%
    column_to_rownames("code")) %>% select(all_of(as.vector(unlist(temp_city_mrio %>% distinct(ref_area,code) %>% 
    bind_rows(temp_city_mrio %>% distinct(ref_area,code) %>% filter(ref_area=="ES") %>% mutate_at(c("ref_area"),~paste0("ES30"))) %>% 
    arrange(ref_area,code) %>% unite(code,"ref_area","code")))),P2,P3AP5,GDP,P1)
    Z_mrio %<>% mutate_all(~if_else(is.na(.),0,.))
    
    ##### check gross value added vectors
    if(round(sum(city_mrio_industry %>% filter(year == year_n) %>% select(-year) %>% filter(grepl("W2",code)) %>% select(`AR_A`:`ZA_T`) %>%
    colSums)/sum(Z_mrio %>% rownames_to_column("code") %>% filter(code%in%c("D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G")) %>%
    select(`AR_A`:`ZA_T`)),15)==1){cat("the new and original MRIOs GVA vectos match")}else{
    cat("the new and original MRIOs GVA vectors DO NOT match")}
    #### check total output vectors
    if(round(sum(city_mrio_industry %>% filter(year == year_n) %>% select(-year) %>% select(`AR_A`:`ZA_T`) %>%
    colSums)/sum(Z_mrio %>% rownames_to_column("code") %>% filter(code=="P1") %>% select(`AR_A`:`ZA_T`)),15)==1){
    cat("the new and original MRIOs output vectors match")}else{cat("the new and original MRIOs output vectors DO NOT match")}
    
  #### [H] include in the dataset
  data_io_mrio %<>% bind_rows(Z_mrio %>% mutate(year=year_n) %>% rownames_to_column("code"))
  cat(year_n)
  }
  #### save the aggregated dataset
  saveRDS(city_mrio_industry_final,file.path(getwd(),"datasets","city_mrio_industry_final.rds"))
} else {
  city_mrio_industry_final <- readRDS(file.path(getwd(),"datasets","city_mrio_industry_final.rds"))
  city_mrio_industry_final %<>% as_tibble
}
}
##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################