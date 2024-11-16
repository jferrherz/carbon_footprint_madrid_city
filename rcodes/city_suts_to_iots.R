##################################################################################################################################
## [B] BASIC INPUTS FOR ESTIMATIONS
##################################################################################################################################

#################### 
# [A] Loading municipal accounts
#########################################################

#### [1] Macro targets at city level
#########################################################
accounts_city <- code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento",
"Contabilidad municipal","D1121121_rev19_2021.xlsx"),sheet = "2",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(seq(2000,year_end_city)))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("P1")) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "3",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("P2"))) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "4",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("GDP"))) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "5",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("RGDP"))) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "6",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("D1"))) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "6S",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("D11"))) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "6C",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("D12"))) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "7",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("D29XD39"))) %>% bind_rows(
code_iots_city %>% filter(type=="sector") %>% distinct(code,code_3d,.keep_all = TRUE) %>%
left_join(readxl::read_xlsx(file.path(database_path,"Ayuntamiento","Contabilidad municipal",
"D1121121_rev19_2021.xlsx"),sheet = "8",col_names = FALSE, skip = 7) %>%
set_names(c("code","code_3d","Definition",as.character(2000:year_end_city))) %>% select(-1:-2),
by=c("Definition")) %>% mutate_at(c("Definition"),~gsub("- ","",.)) %>% 
mutate(var = paste0("B2A3G"))) %>% select(code,nace_code,
!!sym(paste0(year_str_io)):!!sym(paste0(year_end_city)),var)

#### [2] Macro targets for consumption
#########################################################
expend_demo_city <- tibble()
for(sheet_n in c(paste0(sapply(c(1:4), \(x) paste(x, c(1:8), sep = ".")),"."))){
temp_city <- readxl::read_xls(file.path(database_path,"Ayuntamiento",
"Encuesta de Presupuestos Familiares (EPF)","EPF_2021 (A3000320)_VALORES.xls"),
sheet = paste0(sheet_n),col_names = TRUE, skip = 4) %>% rename("Grupo"="...1") %>%
filter(!is.na(Grupo)&!grepl("FUENTE",Grupo)&!grepl("Nota",Grupo)) %>% 
mutate(sheet = paste0(sheet_n)) %>% mutate_at(c(2:4),~as.numeric(.))
expend_demo_city %<>% bind_rows(temp_city)}

expend_coicop_city <- tibble()
for(sheet_n in c(paste0("5.",c(2:9),"."))){
temp_city <- readxl::read_xls(file.path(database_path,"Ayuntamiento",
"Encuesta de Presupuestos Familiares (EPF)","EPF_2021 (A3000320)_VALORES.xls"),
sheet = paste0(sheet_n),col_names = TRUE, skip = 4) %>% rename("Grupo"="...1") %>%
filter(!is.na(Grupo)&!grepl("FUENTE",Grupo)&!grepl("Nota",Grupo)) %>% 
mutate(sheet = paste0(sheet_n)) %>% mutate_at(c(2:14),~as.numeric(.))
expend_coicop_city %<>% bind_rows(temp_city)}

expend_coicop_macro <- readxl::read_xls(file.path(database_path,"Ayuntamiento",
"Encuesta de Presupuestos Familiares (EPF)","EPF_2021 (A3000320)_VALORES.xls"),
sheet = "5.1.",col_names = TRUE, skip = 4) %>% rename("Grupo"="...1") %>%
filter(!is.na(Grupo)&!grepl("FUENTE",Grupo)&!grepl("Nota",Grupo)) %>% 
mutate(sheet = paste0(sheet_n)) %>% mutate_at(c(2:5),~as.numeric(.))

ratio_region_to_city <- readxl::read_xls(file.path(database_path,"Ayuntamiento",
"Encuesta de Presupuestos Familiares (EPF)","EPF_2021 (A3000320)_VALORES.xls"),
sheet = "1.1.",col_names = TRUE, skip = 4) %>% rename("Grupo"="...1") %>%
filter(!is.na(Grupo)&!grepl("FUENTE",Grupo)&!grepl("Nota",Grupo)) %>% 
filter(Grupo=="Total") %>% mutate(city_to_region = `Ciudad de Madrid`/
`Comunidad de Madrid`) %>% mutate(ANOENC=2021) %>% select(ANOENC,city_to_region)
for(year_n in c(2010:2020)){ratio_region_to_city %<>% bind_rows(ratio_region_to_city %>% 
filter(ANOENC==2021) %>% mutate(ANOENC = year_n)) %>% arrange(ANOENC)}

#################### 
# [B] City IO framework
#########################################################

#### SUPPLY Table
#######################
if(length(files_path_figaro[files_path_figaro=="city_sup_raw.rds"])==0){
  data_io_supply <- data.frame()
  for(year_n in years){
    temp_data_io_supply  <- readRDS(file.path(getwd(),"datasets","cam_sup_raw.rds")) %>% 
    filter(year==year_n) %>% left_join(code_iots_city %>% distinct(cpa_r2_consolidated,cpa_code),
    by=c("code"="cpa_r2_consolidated")) %>% group_by(cpa_code) %>% select(-code,-year) %>%
    summarize_all(~sum(.,na.rm=TRUE)) %>%  pivot_longer(`A`:`TS_BP`,names_to = "nace_r2_consolidated",
    values_to = "value") %>% left_join(code_iots_city %>% distinct(nace_r2_consolidated,nace_code),
    by=c("nace_r2_consolidated")) %>% group_by(cpa_code,nace_code) %>% summarize_at(c("value"),
    ~sum(.,na.rm=TRUE)) %>% pivot_wider(names_from = "nace_code",values_from = "value") %>% 
    arrange(factor(cpa_code,levels=c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>%
    distinct(cpa_code))),"CIFOBADJ","OP_RES","TS_BP"))) %>% mutate(year = paste0(year_n)) %>% 
    rename("code"="cpa_code")
    data_io_supply %<>% bind_rows(temp_data_io_supply)
  }
  saveRDS(data_io_supply,file.path(getwd(),"datasets","city_sup_raw.rds"))
} else {
  city_sup_raw <- readRDS(file.path(getwd(),"datasets","city_sup_raw.rds"))
}

#### USE table
#######################
if(length(files_path_figaro[files_path_figaro=="city_use_raw.rds"])==0){
 data_io_use <- data.frame()
 for(year_n in years){
    temp_data_io_use  <- readRDS(file.path(getwd(),"datasets","cam_use_raw.rds")) %>% 
    filter(year==year_n) %>% left_join(code_iots_city %>% distinct(cpa_r2_consolidated,cpa_code) %>%
    bind_rows(code_iots_city %>% distinct(cpa_r2_consolidated,cpa_code) %>% mutate_at(c("cpa_code",
    "cpa_r2_consolidated"),~paste0(.,"_IM"))),by=c("code"="cpa_r2_consolidated")) %>%
    group_by(cpa_code) %>% select(-code,-year) %>% summarize_all(~sum(.,na.rm=TRUE)) %>% 
    pivot_longer(`A`:`TU`,names_to = "nace_r2_consolidated",values_to = "value") %>% 
    left_join(code_iots_city %>% distinct(nace_r2_consolidated,nace_code),
    by=c("nace_r2_consolidated")) %>% group_by(cpa_code,nace_code) %>% summarize_at(c("value"),
    ~sum(.,na.rm=TRUE)) %>% pivot_wider(names_from = "nace_code",values_from = "value") %>% 
    arrange(factor(cpa_code,levels=as.vector(c(code_iots_city %>% filter(type=="sector") %>% 
    distinct(cpa_code) %>% unlist,paste0(code_iots_city %>% filter(type=="sector") %>% 
    distinct(cpa_code) %>% unlist,"_IM"),code_iots_city %>% filter(!type=="sector") %>% 
    distinct(cpa_code) %>% unlist)))) %>% mutate(year = paste0(year_n)) %>% 
    rename("code"="cpa_code")
    
    # temp_data_io_use %>% filter(code%in%c(as.vector(unlist(code_iots_city %>% filter(type=="sector") %>% 
    # distinct(cpa_code))))) %>% ungroup %>% summarize(sum(P2)/1e06)
    # 
    # readxl::read_xlsx(file.path(database_path,"Database_ISM_IO","crmb13.xlsx"),skip=9,
    # sheet="Tabla 1",col_names=TRUE) %>% rename("code"="...1") %>% filter(!is.na(`2002`))
    
    data_io_use %<>% bind_rows(temp_data_io_use)
 }
  saveRDS(data_io_use,file.path(getwd(),"datasets","city_use_raw.rds"))
} else {
  city_use_raw <- readRDS(file.path(getwd(),"datasets","city_use_raw.rds"))
}

#### CITY SUPPLY
#########################
if(length(files_path_figaro[files_path_figaro=="city_nio_supp.rds"])==0){
  data_io_year <- data.frame()
  for(year_n in unique(city_sup_raw$year)){
      temp_nio <- supply.use(year_n,"supply","city") %>% as.data.frame()
      data_io_year %<>% bind_rows(temp_nio %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
  }
  saveRDS(data_io_year,file.path(getwd(),"datasets","city_nio_supp.rds"))
} else{
  city_sup <- readRDS(file.path(getwd(),"datasets","city_nio_supp.rds"))
}
#### CITY USE
#########################
if(length(files_path_figaro[files_path_figaro=="city_nio_use.rds"])==0){
  data_io_year <- data.frame()
  for(year_n in unique(city_use_raw$year)){
      temp_nio <- supply.use(year_n,"use","city") %>% as.data.frame()
      data_io_year %<>% bind_rows(temp_nio %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
  }
  saveRDS(data_io_year,file.path(getwd(),"datasets","city_nio_use.rds"))
} else{
  city_use <- readRDS(file.path(getwd(),"datasets","city_nio_use.rds"))
}

#### CITY IOTs (domestic)
#########################
if(length(files_path_figaro[files_path_figaro=="city_niot_ind.rds"])==0){
  data_io_year_ind <- data.frame()
  data_io_year_prd <- data.frame()
  for(year_n in unique(city_sup_raw$year)){
      temp_nio_ind <- nio.mad.matrix(year_n,"","M","D","city") %>% as.data.frame()
      temp_nio_prd <- nio.mad.matrix(year_n,"","M","B","city") %>% as.data.frame()
      data_io_year_ind %<>% bind_rows(temp_nio_ind %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
      data_io_year_prd %<>% bind_rows(temp_nio_prd %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
  }
  saveRDS(data_io_year_ind,file.path(getwd(),"datasets","city_niot_ind.rds")) 
  saveRDS(data_io_year_prd,file.path(getwd(),"datasets","city_niot_prd.rds")) 
} else{
  city_niot_ind <- readRDS(file.path(getwd(),"datasets","city_niot_ind.rds")) %>% as.data.frame()
  city_niot_prd <- readRDS(file.path(getwd(),"datasets","city_niot_prd.rds")) %>% as.data.frame()
}

#### CITY IOT
#########################
if(length(files_path_figaro[files_path_figaro=="city_iots_ind.rds"])==0){
  data_io_year <- data.frame()
  for(year_n in years){
    #### [a] load municipal data
    p1  <- accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>%
    pivot_wider(names_from = "var",values_from = "value") %>% filter(year == year_n) %>% select(nace_code,P1)
    p2  <- accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>%
    pivot_wider(names_from = "var",values_from = "value") %>% filter(year == year_n) %>% select(nace_code,P2)
    b2a3g  <- accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>%
    pivot_wider(names_from = "var",values_from = "value") %>% filter(year == year_n) %>% select(nace_code,B2A3G)
    d29xd39  <- accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>%
    pivot_wider(names_from = "var",values_from = "value") %>% filter(year == year_n) %>% select(nace_code,D29XD39)
    d1 <- accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>%
    pivot_wider(names_from = "var",values_from = "value") %>% filter(year == year_n) %>% select(nace_code,D1)
    gdp <- accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>%
    pivot_wider(names_from = "var",values_from = "value") %>% filter(year == year_n) %>% select(nace_code,GDP)
    p2b <- accounts_city %>% pivot_longer(`2000`:`2020`,names_to = "year",values_to = "value") %>% filter(year == year_n) %>%
    pivot_wider(names_from = "var",values_from = "value") %>% mutate(B1G = rowSums(across(c("D1","D29XD39","B2A3G")),
    na.rm=TRUE)) %>% mutate(P2B = P1-GDP) %>% mutate(P2C = P1-B1G) %>% select(nace_code,P2B,P2C)
      
    #### [b] correct the regional table with municipal inputs
    z_matrix   <- nio.mad.matrix(year_n,"",'Z',"D","city")
    im_matrix  <- nio.mad.matrix(year_n,"",'P7M',"D","city")
    t_matrix   <- nio.mad.matrix(year_n,"",'VMT',"D","city")
    zx_matrix  <- nio.mad.matrix(year_n,"",'ZT',"D","city") %>% colSums %>% as.data.frame() %>%
    rename("P2"=".") %>% mutate_at(c("P2"),~1/.) %>% mutate_at(c("P2"),~if_else(is.nan(.)|is.infinite(.),0,.))
    z_matrix   <- as.matrix(z_matrix)%*%diag(as.vector(unlist(zx_matrix))) %>% set_colnames(c(rownames(z_matrix)))
    im_matrix  <- as.matrix(im_matrix)%*%diag(as.vector(unlist(zx_matrix))) %>% set_colnames(c(rownames(z_matrix)))
    x_matrix   <- p2 %>% column_to_rownames("nace_code")
    z_matrix   <- as.matrix(z_matrix)%*%diag(as.vector(unlist(x_matrix))) %>% set_colnames(c(rownames(z_matrix)))
    im_matrix  <- as.matrix(im_matrix)%*%diag(as.vector(unlist(x_matrix))) %>% set_colnames(c(rownames(z_matrix)))
    t_matrix   <- as.matrix(t_matrix)%*%diag(as.vector(unlist(x_matrix))) %>% set_colnames(c(rownames(z_matrix)))
    im_matrixs <- colSums(im_matrix)/(colSums(z_matrix+im_matrix)) %>% as.data.frame %>% set_names("P2")
    im_matrixs %<>% mutate_at(c("P2"),~if_else(is.nan(.)|is.infinite(.),0,.))
    
    ##### [c] to adjust the regional matrix down to city proportions we use the RAS algorithm
    ##### [==> the "v" is the result of correcting the distribution the new matrix colsums by the distribution of P2
    u <- p2 %>% left_join(colSums(z_matrix) %>% as.data.frame %>% rename("P2B"=".") %>% rownames_to_column("code"),
    by=c("nace_code"="code")) %>% mutate(P2_new = P2B,P2BT = sum(P2B),P2T = sum(P2)) %>% mutate_at(c("P2","P2B"),~./sum(.,na.rm=TRUE)) %>%
    mutate(ratio = (P2/P2B)) %>% mutate_at(c("P2_new"),~.*ratio) %>% mutate(sum(P2_new,na.rm=TRUE)) %>% select(P2_new) %>%
    mutate_all(~if_else(is.na(.),0,.)) %>% unlist %>% as.vector
    ##### [==> the "u" assumes the proportions between the row sums and total output in the regional table
    ##### it is important to adjust the distribution and, then, to correct the levels to match the "u"
    v <- rowSums(nio.mad.matrix(year_n,"",'Z',"D","city")) %>% as.data.frame %>% rename("P2"=".") %>% rownames_to_column("code") %>%
    left_join(nio.mad.matrix(year_n,"",'P1',"D","city")%>% as.data.frame %>% rownames_to_column("code"),by=c("code")) %>%
    mutate(v_ratio = (P2/P1)) %>% left_join(p1,by=c("code"="nace_code")) %>% mutate(v = v_ratio * P1.y) %>%
    bind_cols("u" = u) %>% mutate_at(c("v"),~v*(sum(u)/sum(v))) %>% mutate(sum(u),sum(v)) %>% select(v) %>%
    mutate_all(~if_else(is.na(.),0,.)) %>% unlist %>% as.vector
    ##### [==> apply GRAS algorithm ==> note the swap of u and v (otherwise im_matrix_r wrong)
    z_matrix_ras <- gras_update(u = v, v = u, X0 = as.matrix(z_matrix),epsilon=0.00001) %>% 
    as.data.frame %>% mutate_all(~if_else(is.na(.),0,.)) %>% set_colnames(c(rownames(z_matrix)))
    #### [==> retrieve the import matrix
    im_matrix_r<- colSums(z_matrix_ras) %>% as.data.frame() %>% rename("P2Z" = ".") %>% rownames_to_column("code") %>% 
    left_join(colSums(as.matrix(z_matrix_ras)%*%diag(as.vector(unlist(im_matrixs)))) %>% as.data.frame() %>% 
    rename("P2M" = ".") %>% mutate(code = rownames(z_matrix)),by=c("code")) %>% left_join(p2,by=c("code"="nace_code")) %>% #mutate(sum(P2Z)+sum(P2M),sum(P2))
    mutate(P2R = P2Z + P2M,ratio = P2/P2R,ratio_m = ((P2-P2Z)/P2M),(ratio_m * P2M)+P2Z-P2) %>% select(ratio_m) %>%
    mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    im_matrix  <- as.matrix(z_matrix_ras)%*%diag(as.vector(unlist(im_matrixs)))%*%diag(as.vector(unlist(im_matrix_r))) %>% 
    as.data.frame %>% set_rownames(c(paste0(rownames(z_matrix),"_IM"))) %>% set_colnames(c(rownames(z_matrix)))
    im_matrix %<>% mutate_all(~if_else(is.nan(.)|is.infinite(.),0,.))
    t_matrix  %<>% as.data.frame %>% rownames_to_column("code")
    z_matrix  <- z_matrix_ras %>% as.data.frame %>% mutate_all(~if_else(.<1,0,.)) %>% rownames_to_column("code")
    im_matrix <- im_matrix %>% mutate_all(~if_else(.<1,0,.)) %>% rownames_to_column("code")
    #### check:
    if(sum(round(((colSums(z_matrix[,-1])+colSums(im_matrix[,-1]))/p2$P2)[-28],0))<27) { break }
    
    #### [c] put together inputs and the new table
    m_matrix <- z_matrix %>% bind_rows(im_matrix) %>% bind_rows(d29xd39 %>% pivot_wider(names_from = "nace_code",values_from = "D29XD39") %>%
    mutate(code = "D29XD39") %>% select(code,everything())) %>%bind_rows(b2a3g %>% pivot_wider(names_from = "nace_code",values_from = "B2A3G") %>%
    mutate(code = "B2A3G") %>% select(code,everything())) %>% bind_rows(d1 %>% pivot_wider(names_from = "nace_code",values_from = "D1") %>%
    mutate(code = "D1") %>% select(code,everything()))
    m_matrix %<>% bind_rows(m_matrix %>% column_to_rownames("code") %>% colSums %>% rbind %>% as.data.frame %>%
    set_rownames(c("P1")) %>% rownames_to_column("code") %>% select(code,everything()))
    #### [==> note that the total rowSums does not match P1 since GDP is not final demand (obvious, but)
    #### [==> check that the share of  inputs on output is the same as in the regional table [validated 18/01/2023]
    m_matrix %<>% left_join(p1,by=c("code"="nace_code")) %>% mutate(P2 = rowSums(across(`A`:`T`),na.rm=TRUE)) %>%
    mutate(P3AP6 = P1 - P2) %>% left_join(gdp,by=c("code"="nace_code")) %>% select(code,`A`:`T`,P2,P3AP6,GDP,P1) %>% 
    mutate_if(is.numeric,~replace_na(.,0))
    
    #### [d] save to dataset
    data_io_year %<>% bind_rows(m_matrix %>% mutate(year = year_n))
    print(year_n)
  }
  saveRDS(data_io_year,file.path(getwd(),"datasets","city_iots_ind.rds"))
} else{
  city_iots_industry <- readRDS(file.path(getwd(),"datasets","city_iots_ind.rds"))
}

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################