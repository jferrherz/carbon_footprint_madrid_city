##################################################################################################################################
## Block B - Processing the regional IO framework for MAD
##################################################################################################################################

# #################### 
# # Importing Regional SUTs from Madrid Region
# #########################################################
# #### source: https://www.madrid.org/iestadis/fijas/estructu/economicas/contabilidad/estructumio.htm
# #### cross-classification of regional SUTs and National Accounts
# code_iots_mad <- readxl::read_xlsx(file.path(getwd(),"datasets","classification_iots_mad.xlsx"),sheet = "Tabla")
# code_iots_city <- readxl::read_xlsx(file.path(getwd(),"datasets","classification_iots_mad.xlsx"),sheet = "City") %>%
# filter(digits_full>2)
# code_fiot64 <- readxl::read_xlsx(file.path(path_figaro,"Description-FIGARO-tables.xlsx"), 
# skip = 10, col_names = TRUE, sheet = "Codes")
# cout_fiot64 <- readxl::read_xlsx(file.path(path_figaro,"Description-FIGARO-tables.xlsx"), 
# skip = 10, col_names = TRUE, sheet = "Countries") %>%
# unite(`non FIGARO countries`,`...4`,c("non FIGARO countries","...4")) %>% 
# unite(`FIGARO countries`,`...2`,c("FIGARO countries","...2")) %>%
# gather(key,value) %>% mutate(figaro = if_else((key=="non FIGARO countries"|key=="...4"),"no","yes")) %>% 
# separate(value,c("country","cou"), sep = "_") %>% dplyr::select(-key) %>% filter(!cou=="NA") %>%
# mutate(cou = if_else(cou=="EL","GR",cou)) %>% mutate(cou = if_else(cou=="UK","GB",cou))

#################### 
# REGIONAL - Madrid
#########################################################

#### SUPPLY Table
#######################
if(length(files_path_figaro[files_path_figaro=="cam_sup_raw.rds"])==0){
  data_io_supply <- data.frame()
  for(year_n in c(years)){
    temp_data_io_supply  <- readxl::read_xlsx(file.path(path_riot,"Base_2013",paste0("miob13",str_sub(year_n,3,4),".xlsx")), 
    sheet = "Origen", skip = 7) %>% rename_if(grepl("basicos",colnames(.)),~"Oferta total a precios básicos") %>%
    rename_if(grepl("importaciones",colnames(.)),~"Total Importaciones") %>% rename("code"="...1") %>%
    filter(!is.na(`Oferta total a precios básicos`)&!grepl("Fuente:",code)) %>% 
    mutate_at(c("code"),~if_else(is.na(.),"Ajuste CIF/FOB",.))
    
    temp_data_io_supply %<>% #### changing the sector names for row industries (supply)
    mutate_at(c("code"),~stringr::str_replace_all(.,setNames(c(as.vector(code_iots_mad %>% 
    filter(IOTs_MAD%in%c(temp_data_io_supply$code)) %>% arrange(factor(IOTs_MAD,levels=c(temp_data_io_supply$code))) %>% 
    select(cpa_r2) %>% unlist)),c(temp_data_io_supply$code)))) %>% mutate_at(c("code"),~
    if_else(.=="28. Productos de madera y corcho (exc. muebles), cestería y espartería","CPA_C16",.))
    
    #### [ATTENTION] consolidation of 3-digits down to unique sectors
    temp_data_io_supply %<>% pivot_longer(!!sym(names(temp_data_io_supply)[2]):!!sym(names(temp_data_io_supply)[length(names(temp_data_io_supply))]),
    names_to = "demand",values_to = "value") %>% mutate_at(c("demand"),~stringr::str_replace_all(.,setNames(c("code",
    as.vector(code_iots_mad %>% filter(IOTs_MAD_short%in%c(names(temp_data_io_supply))) %>% arrange(factor(IOTs_MAD_short,
    levels=c(names(temp_data_io_supply)))) %>% select(nace_r2_d3r) %>% unlist)),c(names(temp_data_io_supply))))) %>% group_by(code,demand) %>% 
    summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% pivot_wider(names_from = "demand",values_from = "value") %>%
    arrange(factor(code,levels=c(code_iots_mad %>% filter(type=="sector") %>% distinct(cpa_r2) %>% unlist %>% as.vector,"CIFOBADJ",
    "OP_RES","TS_BP"))) %>% mutate(year = paste0(year_n)) %>% select(code,all_of(code_iots_mad %>% filter(type=="sector") %>% 
    distinct(nace_r2_d3r) %>% arrange(nace_r2_d3r) %>% unlist %>% as.vector),everything()) %>%
    mutate(year = paste0(year_n)) %>% ungroup()
    data_io_supply %<>% bind_rows(temp_data_io_supply)
  }
  saveRDS(data_io_supply,file.path(getwd(),"datasets","cam_sup_raw.rds"))} else {
  region_sup_raw <- readRDS(file.path(getwd(),"datasets","cam_sup_raw.rds"))
}

#### USE table
#######################
if(length(files_path_figaro[files_path_figaro=="cam_use_raw.rds"])==0){
 data_io_use <- data.frame()
 for(year_n in c(years)){
    temp_data_io_use  <- readxl::read_xlsx(file.path(path_riot,"Base_2013",paste0("miob13",str_sub(year_n,3,4),".xlsx")), 
    sheet = "Destino", skip = 7) %>% rename("code"="...1") %>% filter(!is.na(`code`)&!`code`=="Total") %>% 
    mutate_at(c("code"),~if_else(.%in%c("Madrid"),paste0(lag(.),"_",.),.)) %>%
    mutate_at(c("code"),~if_else(.%in%c("Importado"),paste0(lag(.,2),"_",.),.)) %>%
    separate(code,c("code","residence"),"_",extra = "merge") %>% 
    filter(!(is.na(`Total empleos`)&is.na(`01. Agricultura y ganadería`))) %>%
    rename_if(grepl("19. Actividades",colnames(.)),~"43. Actividades postales y de correos") %>%
    rename_if(grepl("20. Servicios",colnames(.)),~"48. Servicios de información") %>%
    mutate_at(c("code"),~if_else(grepl("19. Servicios",.),"43. Servicios postales y de correos",.)) %>%
    mutate_at(c("code"),~if_else(grepl("20. Servicios",.),"48. Servicios de Información",.)) %>%
    filter(!grepl("trabajo",code)) %>% filter(!grepl("Asalariados",code)) %>%
    filter(!code%in%c("Total interior a precios básicos")&!grepl("Fuente:",code))
    
    temp_data_io_use %<>% #### changing the sector names for row industries (supply)
    mutate_at(c("code"),~stringr::str_replace_all(.,setNames(c(as.vector(code_iots_mad %>% 
    filter(IOTs_MAD%in%c(temp_data_io_use$code)) %>% arrange(factor(IOTs_MAD,levels=c(unique(temp_data_io_use$code)))) %>% 
    select(cpa_r2) %>% unlist)),c(unique(temp_data_io_use$code))))) %>% mutate_at(c("code"),~
    if_else(.=="28. Productos de madera y corcho (exc. muebles), cestería y espartería","CPA_C16",.)) %>%
    mutate_at(c("residence"),~if_else(is.na(residence),"Totals",.)) %>% filter(!residence=="Total") %>%
    mutate_at(c("code"),~if_else(residence=="Importado",paste0(.,"_IM"),.)) %>%
    arrange(factor(residence,levels=c("Madrid","Importado")),factor(code,
    levels=c(code_iots_mad %>% filter(IOTs_MAD%in%c(unique(temp_data_io_use$code))) %>% 
    distinct(nace_r2_d3r) %>% unlist %>% as.vector)))
    
    #### [ATTENTION] consolidation of 3-digits down to unique sectors
    temp_data_io_use %<>% pivot_longer(!!sym(names(temp_data_io_use)[3]):!!sym(names(temp_data_io_use)[length(names(temp_data_io_use))]),
    names_to = "demand",values_to = "value") %>% mutate_at(c("demand"),~stringr::str_replace_all(.,setNames(c("code","residence",
    as.vector(code_iots_mad %>% filter(IOTs_MAD_short%in%c(names(temp_data_io_use))) %>% arrange(factor(IOTs_MAD_short,
    levels=c(names(temp_data_io_use)))) %>% select(nace_r2_d3r) %>% unlist)),c(names(temp_data_io_use))))) %>% group_by(code,demand) %>% 
    summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% pivot_wider(names_from = "demand",values_from = "value") %>% rename("P51G"="P5 fijo") %>% 
    arrange(factor(code,levels=as.vector(c(code_iots_mad %>% filter(type=="sector") %>% distinct(cpa_r2) %>% 
    unlist,paste0(code_iots_mad %>% filter(type=="sector") %>% distinct(cpa_r2) %>% 
    unlist,"_IM"),code_iots_mad %>% filter(!type=="sector") %>% distinct(cpa_r2) %>% 
    unlist)))) %>% select(code,all_of(code_iots_mad %>% filter(type=="sector") %>% 
    distinct(nace_r2_d3r) %>% arrange(nace_r2_d3r) %>% unlist %>% as.vector),everything()) %>%
    mutate(year = paste0(year_n)) %>% ungroup()
    
  data_io_use %<>% bind_rows(temp_data_io_use)
  #print(temp_data_io_use %>% select(P3_S14) %>% filter(code=="CPA_C19"))
 }
  # temp_data_io_use %>% select(1:2,67) %>% filter(code=="25. Coque y productos de refino de petróleo")
  # temp_data_io_use %>% select(1:2,67) %>% filter(code=="CPA_C19")
  # temp_data_io_use %>% select(P3_S14) %>% filter(code=="CPA_C19")
  saveRDS(data_io_use,file.path(getwd(),"datasets","cam_use_raw.rds"))} else {
  region_use_raw <- readRDS(file.path(getwd(),"datasets","cam_use_raw.rds"))
}

#### Regional SUPPLY
#########################
if(length(files_path_figaro[files_path_figaro=="cam_suts_sup.rds"])==0){
  data_io_year <- data.frame()
  for(year_n in years){
      temp_nio <- supply.use(year_n,"supply","region") %>% as.data.frame()
      data_io_year %<>% bind_rows(temp_nio %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
  }
  saveRDS(data_io_year,file.path(getwd(),"datasets","cam_suts_sup.rds"))
  openxlsx::write.xlsx(data_io_year,file.path(getwd(),"datasets","cam_suts_sup.xlsx"))
} else{
  region_sup <- readRDS(file.path(getwd(),"datasets","cam_suts_sup.rds"))
}
#### Regional USE
#########################
if(length(files_path_figaro[files_path_figaro=="cam_suts_use.rds"])==0){
  data_io_year <- data.frame()
  for(year_n in years){
      temp_nio <- supply.use(year_n,"use","region") %>% as.data.frame()
      data_io_year %<>% bind_rows(temp_nio %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
  }
  saveRDS(data_io_year,file.path(getwd(),"datasets","cam_suts_use.rds"))
  openxlsx::write.xlsx(data_io_year,file.path(getwd(),"datasets","ccam_suts_use.xlsx"))
} else{
  region_use <- readRDS(file.path(getwd(),"datasets","cam_suts_use.rds"))
}

#### Regional IOTs (domestic)
#########################
if(length(files_path_figaro[files_path_figaro=="cam_riot_ind.rds"])==0){
  data_io_year_ind <- data.frame()
  data_io_year_prd <- data.frame()
  for(year_n in years){
      temp_nio_ind <- nio.mad.matrix(year_n,"","M","D","region") %>% as.data.frame()
      temp_nio_prd <- nio.mad.matrix(year_n,"","M","B","region") %>% as.data.frame()
      data_io_year_ind %<>% bind_rows(temp_nio_ind %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
      data_io_year_prd %<>% bind_rows(temp_nio_prd %>% mutate(year = as.character(year_n)) %>%
      rownames_to_column("code"))
  }
  saveRDS(data_io_year_ind,file.path(getwd(),"datasets","cam_riot_ind.rds")) 
  saveRDS(data_io_year_prd,file.path(getwd(),"datasets","cam_riot_prd.rds")) 
} else{
  madrid_niot_ind <- readRDS(file.path(getwd(),"datasets","cam_riot_ind.rds")) %>% as.data.frame()
  madrid_niot_prd <- readRDS(file.path(getwd(),"datasets","cam_riot_prd.rds")) %>% as.data.frame()
}

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################