##################################################################################################################################
## [B] BASIC INPUTS FOR ESTIMATIONS - FIGARO IOTs
##################################################################################################################################

  #########
  #### [B] FIGARO
  ##############################################################
  # ===== This part of the code loads the tables as they are provided by the FIGARO repository.
  # it loads (1) the industry by industry, (2) product by product, (3) supply and (4) use tables
  
  #### INDUSTRY by INDUSTRY
  #########################
   if(length(files_path_figaro[files_path_figaro==paste0("figaro_ind-by-ind_mrio_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_ind <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      data_io_ind_temp <- readr::read_csv(file.path(path_figaro_ind_ind,
      paste0("matrix_eu-ic-io_ind-by-ind_",stringr::str_sub(year_end_io+2,3,4),
      "ed_",year_i,".csv"))) %>% mutate(year = paste0(year_i))
      data_io_ind %<>% bind_rows(data_io_ind_temp)
      cat(paste0("\n year: ",year_i))
    }
    
    data_io_ind %<>% rename("code"="rowLabels") %>% as_tibble()
    data_io_ind %<>% mutate_at(c("code"),~stringr::str_replace_all(.,
    setNames(c(code_indus64 %>% filter(type=="sector") %>% select(nace_64) %>% 
    unlist %>% as.vector()),c(data_io_ind %>% as.data.frame() %>% separate(code,
    c("cou","code"),extra = "merge") %>% distinct(code) %>% filter(!code%in%c(code_fiot64 %>% 
    filter(type%in%c("value","vimport")&!induse=="B1G") %>% distinct(induse) %>% unlist %>% 
    as.vector)) %>% unlist %>% as.vector()))))
    codes_vec <- list();for(n in c(as.vector(unlist(data_io_ind %>% separate(code,c("cou","code"),
    extra = "drop") %>% distinct(cou))))){codes_vec[[n]] <- paste0(n,'_',c("P3_S13","P3_S14",
    "P3_S15","P51G","P5M"))};codes_vec %<>% unlist %>% as.vector
    data_io_ind %<>% rename(!!setNames(c(colnames(data_io_ind)[!colnames(data_io_ind)%in%
    c("code",codes_vec,"year")]),c(as.vector(unlist(data_io_ind %>% distinct(code) %>% 
    filter(!code%in%c(paste0("W2_",code_fiot64 %>% filter(type%in%c("value","vimport")&
    !induse=="B1G") %>% distinct(induse) %>% unlist %>% as.vector))))))))
    saveRDS(data_io_ind,file.path(path_figaro_data,paste0("figaro_ind-by-ind_mrio_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  } else {figaro_industry_wio <- readRDS(file.path(path_figaro_data,
  paste0("figaro_ind-by-ind_mrio_",stringr::str_sub(year_end_io+2,3,4),
  "ed.rds")))}
  
  #### PRODUCT by PRODUCT
  #######################
  if(length(files_path_figaro[files_path_figaro==paste0("figaro_prod-by-prod_mrio_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_prod <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      data_io_prod_temp <- readr::read_csv(file.path(path_figaro_prod_prod,
      paste0("matrix_eu-ic-io_prod-by-prod_",stringr::str_sub(year_end_io+2,3,4),
      "ed_",year_i,".csv"))) %>% mutate(year = paste0(year_i))
      data_io_prod %<>% bind_rows(data_io_prod_temp)
      cat(paste0("\n year: ",year_i))
    }
    data_io_prod %<>% rename("code"="rowLabels") %>% as_tibble()
    data_io_prod %<>% mutate_at(c("code"),~stringr::str_replace_all(.,
    setNames(c(code_produ64 %>% filter(type=="sector") %>% select(nace_64) %>% 
    unlist %>% as.vector()),c(data_io_prod %>% as.data.frame() %>% separate(code,
    c("cou","code"),extra = "merge") %>% distinct(code) %>% filter(!code%in%c(code_fiot64 %>% 
    filter(type%in%c("value","vimport")&!induse=="B1G") %>% distinct(induse) %>% unlist %>% 
    as.vector)) %>% unlist %>% as.vector()))))
    codes_vec <- list();for(n in c(as.vector(unlist(data_io_prod %>% separate(code,c("cou","code"),
    extra = "drop") %>% distinct(cou))))){codes_vec[[n]] <- paste0(n,'_',c("P3_S13","P3_S14",
    "P3_S15","P51G","P5M"))};codes_vec %<>% unlist %>% as.vector
    data_io_prod %<>% rename(!!setNames(c(colnames(data_io_prod)[!colnames(data_io_prod)%in%
    c("code",codes_vec,"year")]),c(as.vector(unlist(data_io_prod %>% distinct(code) %>% 
    filter(!code%in%c(paste0("W2_",code_fiot64 %>% filter(type%in%c("value","vimport")&
    !induse=="B1G") %>% distinct(induse) %>% unlist %>% as.vector))))))))
    saveRDS(data_io_prod,file.path(path_figaro_data,paste0("figaro_prod-by-prod_mrio_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  } else {figaro_product_wio <- readRDS(file.path(path_figaro_data,
  paste0("figaro_prod-by-prod_mrio_",stringr::str_sub(year_end_io+2,3,4),
  "ed.rds")))}
  
  #### SUPPLY Table
  #######################
  if(length(files_path_figaro[files_path_figaro==paste0("figaro_sup_mrio_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_supply <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      temp_data_io_supply  <- readr::read_csv(file.path(path_figaro_supply,
      paste0("matrix_eu-ic-supply_",stringr::str_sub(year_end_io+2,3,4),
      "ed_",year_i,".csv"))) %>% mutate(year = paste0(year_i))
      data_io_supply %<>% bind_rows(temp_data_io_supply)
      cat(paste0("\n year: ",year_i))
    }
    data_io_supply %<>% rename("code"="rowLabels") %>% as_tibble()
    data_io_supply %<>% mutate_at(c("code"),~stringr::str_replace_all(.,
    setNames(c(code_produ64 %>% filter(type=="sector") %>% select(nace_64) %>% 
    unlist %>% as.vector()),c(data_io_supply %>% as.data.frame() %>% separate(code,
    c("cou","code"),extra = "merge") %>% distinct(code) %>% filter(!code%in%c(code_fiot64 %>% 
    filter(type%in%c("value","vimport")&!induse=="B1G") %>% distinct(induse) %>% unlist %>% 
    as.vector)) %>% unlist %>% as.vector()))))
    codes_vec <- list();for(n in c(as.vector(unlist(data_io_supply %>% separate(code,c("cou","code"),
    extra = "drop") %>% distinct(cou))))){codes_vec[[n]] <- paste0(n,'_',c(code_indus64 %>% filter(type=="sector") %>%
    distinct(nace_64) %>% unlist %>% as.vector))};codes_vec %<>% unlist %>% as.vector
    data_io_supply %<>% rename(!!setNames(c(as.vector(unlist(names(data_io_supply)))),c("code",codes_vec,"year")))
    saveRDS(data_io_supply,file.path(path_figaro_data,paste0("figaro_sup_mrio_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  } else {figaro_sup_wio <- readRDS(file.path(path_figaro_data,
  paste0("figaro_sup_mrio_",stringr::str_sub(year_end_io+2,3,4),
  "ed.rds")))}
  
  #### USE table
  #######################
  if(length(files_path_figaro[files_path_figaro==paste0("figaro_use_mrio_",
  stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_use <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      temp_data_io_use  <- readr::read_csv(file.path(path_figaro_use,
      paste0("matrix_eu-ic-use_",stringr::str_sub(year_end_io+2,3,4),
      "ed_",year_i,".csv"))) %>% mutate(year = paste0(year_i))
      data_io_use %<>% bind_rows(temp_data_io_use)
      cat(paste0("\n year: ",year_i))
    }
    data_io_use %<>% rename("code"="rowLabels") %>% as_tibble()
    data_io_use %<>% mutate_at(c("code"),~stringr::str_replace_all(.,
    setNames(c(code_produ64 %>% filter(type=="sector") %>% select(nace_64) %>% 
    unlist %>% as.vector()),c(data_io_use %>% as.data.frame() %>% separate(code,
    c("cou","code"),extra = "merge") %>% distinct(code) %>% filter(!code%in%c(code_fiot64 %>% 
    filter(type%in%c("value","vimport")&!induse=="B1G") %>% distinct(induse) %>% unlist %>% 
    as.vector)) %>% unlist %>% as.vector()))))
    codes_vec <- list();for(n in c(as.vector(unlist(names(data_io_use)[-c(1,length(names(data_io_use)))] %>%
    t %>% t %>% as_tibble %>% separate(V1,c("cou","code"),extra = "drop") %>% distinct(cou))))){
    codes_vec[[n]] <- paste0(n,'_',c(code_indus64 %>% filter(type=="sector") %>%
    distinct(nace_64) %>% unlist %>% as.vector))};codes_vec %<>% unlist %>% as.vector
    codes_vec_dem <- list();for(n in c(as.vector(unlist(names(data_io_use)[-c(1,length(names(data_io_use)))] %>%
    t %>% t %>% as_tibble %>% separate(V1,c("cou","code"),extra = "drop") %>% distinct(cou))))){
    codes_vec_dem[[n]] <- paste0(n,'_',c("P3_S13","P3_S14","P3_S15","P51G","P5M"))};codes_vec_dem %<>% unlist %>% as.vector
    data_io_use %<>% rename(!!setNames(c(as.vector(unlist(names(data_io_use)))),c("code",codes_vec,codes_vec_dem,"year")))
    saveRDS(data_io_use,file.path(path_figaro_data,paste0("figaro_use_mrio_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  } else {figaro_use_wio <- readRDS(file.path(path_figaro_data,
  paste0("figaro_use_mrio_",stringr::str_sub(year_end_io+2,3,4),
  "ed.rds")))}
  
  #########################
  #### [C] Creation of national tables (NIOs) out of the loaded tables
  ###############################################################################
    # ==> This part of the code creates NIOs out of the tables provided by the FIGARO repository.
    #     it creates a single dataset for the (1) the industry by industry, (2) product by product tables
    # ==> The construction of the (3) supply and (4) use tables NIOs are still pending
    
  #### INDUSTRY by INDUSTRY
  #########################
  if(length(files_path_figaro[files_path_figaro==paste0("figaro_nio_ind_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_year <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      cat(paste0("\n year ",year_i,": "))
      data_io_cou <- data.frame()
      for(cou_j in countries){
        temp_nio <- wio.to.nio(cou_j,year_i,"industry") %>% as.data.frame() %>% mutate(cou = as.character(cou_j))
        data_io_cou %<>% bind_rows(temp_nio)
        cat(paste0(cou_j,", "))
      }
      data_io_year %<>% bind_rows(data_io_cou %>% mutate(year = as.character(year_i)))
    }
    saveRDS(data_io_year,file.path(path_figaro_data,paste0("figaro_nio_ind_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds"))) 
  } else{
    figaro_ind <- readRDS(file.path(path_figaro_data,paste0("figaro_nio_ind_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  }
  
  #### PRODUCT by PRODUCT
  #########################
  if(length(files_path_figaro[files_path_figaro==paste0("figaro_nio_prod_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_year <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      cat(paste0("\n year ",year_i,": "))
      data_io_cou <- data.frame()
      for(cou_j in countries){
        temp_nio <- wio.to.nio(cou_j,year_i,"product") %>% mutate(cou = as.character(cou_j))
        data_io_cou %<>% bind_rows(temp_nio)
        cat(paste0(cou_j,", "))
      }
      data_io_year %<>% bind_rows(data_io_cou %>% mutate(year = as.character(year_i)))
    }
    saveRDS(data_io_year,file.path(path_figaro_data,paste0("figaro_nio_prod_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  } else{
    figaro_prod <- readRDS(file.path(path_figaro_data,paste0("figaro_nio_prod_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  }
  
  #### National SUPPLY
  #########################
  if(length(files_path_figaro[files_path_figaro==paste0("figaro_nio_sup_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_year <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      cat(paste0("\n year ",year_i,": "))
      data_io_cou <- data.frame()
      for(cou_j in countries){
        temp_nio <- supply.use(cou_j,year_i,"supply") %>% mutate(cou = as.character(cou_j)) %>% as.data.frame()
        data_io_cou %<>% bind_rows(temp_nio)
        cat(paste0(cou_j,", "))
      }
      data_io_year %<>% bind_rows(data_io_cou %>% mutate(year = as.character(year_i)))
    }
    saveRDS(data_io_year,file.path(path_figaro_data,paste0("figaro_nio_sup_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  } else{
    figaro_supply <- readRDS(file.path(path_figaro_data,paste0("figaro_nio_sup_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  }
  
  #### National USE
  #########################
  if(length(files_path_figaro[files_path_figaro==paste0("figaro_nio_use_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")])==0){
    data_io_year <- data.frame()
    for(year_i in seq(year_str_io,year_end_io,1)){
      cat(paste0("\n year ",year_i,": "))
      data_io_cou <- list()
      for(cou_j in countries){
        temp_nio <- supply.use(cou_j,year_i,"use") %>% mutate(cou = as.character(cou_j))
        data_io_cou %<>% bind_rows(temp_nio)
        cat(paste0(cou_j,", "))
      }
      data_io_year %<>% bind_rows(data_io_cou %>% mutate(year = as.character(year_i)))
    }
    saveRDS(data_io_year,file.path(path_figaro_data,paste0("figaro_nio_use_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  } else{
    figaro_use <- readRDS(file.path(path_figaro_data,paste0("figaro_nio_use_",
    stringr::str_sub(year_end_io+2,3,4),"ed.rds")))
  }
  
  #########################
  #### [D] Creation of national tables out of EUKLEMS sector disaggregation
  ###############################################################################
    # ==> Creation of the NIOs at two levels of desagregation compible with (a) EUKLEMS and (b) HBS.
    # ==> This is available for the (1) the industry by industry and (2) product by product tables
    
  #### INDUSTRY by INDUSTRY
  #########################
  # if(length(files_path_figaro[files_path_figaro=="figaro_io_ind_a42.rds"])==0){
  #   figaro <- readRDS(file.path(path_figaro_data,"figaro_nio_ind_ln_1020.rds"))
  #   figaro %<>% mutate(code = recode(code,!!!setNames(as.vector(c(code_indus64 %>% filter(type=="sector") %>% 
  #     select(nace_42) %>% unlist,paste0("M_",c(code_indus64 %>% filter(type=="sector") %>% select(nace_42) %>% unlist)),
  #     "D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")),c(figaro$code %>% unique))))
  #   figaro %<>% ungroup %>% group_by(code,cou,year) %>% summarize_at(c(code_indus64 %>% filter(figaro_nio==1) %>% select(nace_64) %>% 
  #     unlist),~sum(.,na.rm=TRUE)) %>% arrange(cou,year,code) %>% set_names(c("code","cou","year",c(code_indus64 %>% 
  #     filter(figaro_nio==1) %>% select(nace_64) %>% unlist))) %>% ungroup() %>% gather(key,value,-code,-cou,-year) 
  #   figaro %<>% mutate(key = recode(key,!!!setNames(as.vector(c(code_indus64 %>% filter(figaro_nio==1) %>% 
  #     select(nace_42) %>% unlist)),c(figaro %>% distinct(key) %>% unlist)))) %>% ungroup 
  #   figaro %<>% group_by(code,key,cou,year) %>% summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% ungroup
  #   figaro %<>% spread(key,value) %>% select(code,as.vector(c(code_indus64 %>% filter(figaro_nio==1) %>% 
  #    distinct(nace_42) %>% unlist)),cou,year) %>% arrange(cou,year,factor(code, levels = c(code_indus64 %>% 
  #    filter(type=="sector") %>% distinct(nace_42) %>% unlist,paste0("M_",code_indus64 %>% 
  #    filter(type=="sector") %>% distinct(nace_42) %>% unlist),"D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")))
  #   saveRDS(figaro,file.path(path_figaro_data,"figaro_io_ind_a42.rds"))
  # } else{
  #   figaro_ind_a42 <- readRDS(file.path(path_figaro_data,"figaro_io_ind_a42.rds"))
  # }
  #   
  # if(length(files_path_figaro[files_path_figaro=="figaro_io_ind_a10.rds"])==0){
  #   figaro <- readRDS(file.path(path_figaro_data,"figaro_nio_ind_ln_1020.rds"))
  #   figaro %<>% mutate(code = recode(code,!!!setNames(as.vector(c(code_indus64 %>% filter(type=="sector") %>% 
  #     select(nace_10) %>% unlist,paste0("M_",c(code_indus64 %>% filter(type=="sector") %>% select(nace_10) %>% unlist)),
  #     "D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")),c(figaro$code %>% unique))))
  #   figaro %<>% ungroup %>% group_by(code,cou,year) %>% summarize_at(c(code_indus64 %>% filter(figaro_nio==1) %>% select(nace_64) %>% 
  #     unlist),~sum(.,na.rm=TRUE)) %>% arrange(cou,year,code) %>% set_names(c("code","cou","year",c(code_indus64 %>% 
  #     filter(figaro_nio==1) %>% select(nace_64) %>% unlist))) %>% ungroup() %>% gather(key,value,-code,-cou,-year) 
  #   figaro %<>% mutate(key = recode(key,!!!setNames(as.vector(c(code_indus64 %>% filter(figaro_nio==1) %>% 
  #     select(nace_10) %>% unlist)),c(figaro %>% distinct(key) %>% unlist)))) %>% ungroup 
  #   figaro %<>% group_by(code,key,cou,year) %>% summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% ungroup
  #   figaro %<>% spread(key,value) %>% select(code,as.vector(c(code_indus64 %>% filter(figaro_nio==1) %>% 
  #    distinct(nace_10) %>% unlist)),cou,year) %>% arrange(cou,year,factor(code, levels = c(code_indus64 %>% 
  #    filter(type=="sector") %>% distinct(nace_10) %>% unlist,paste0("M_",code_indus64 %>% 
  #    filter(type=="sector") %>% distinct(nace_10) %>% unlist),"D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")))
  #   saveRDS(figaro,file.path(path_figaro_data,"figaro_io_ind_a10.rds"))
  # } else{
  #   figaro_ind_a10 <- readRDS(file.path(path_figaro_data,"figaro_io_ind_a10.rds"))
  # }
  # 
  # #### PRODUCT by PRODUCT
  # #######################
  # if(length(files_path_figaro[files_path_figaro=="figaro_io_prod_a42.rds"])==0){
  #   figaro <- readRDS(paste0(path_figaro_data,"/figaro_nio_prod_ln_1020.rds"))
  #   figaro %<>% mutate(code = recode(code,!!!setNames(as.vector(c(code_produ64 %>% filter(type=="sector") %>% 
  #     select(nace_42) %>% unlist,paste0("M_",c(code_produ64 %>% filter(type=="sector") %>% select(nace_42) %>% unlist)),
  #     "D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")),c(figaro$code %>% unique))))
  #   figaro %<>% ungroup %>% group_by(code,cou,year) %>% summarize_at(c(code_produ64 %>% filter(figaro_nio==1) %>% select(nace_64) %>% 
  #     unlist),~sum(.,na.rm=TRUE)) %>% arrange(cou,year,code) %>% set_names(c("code","cou","year",c(code_produ64 %>% 
  #     filter(figaro_nio==1) %>% select(nace_64) %>% unlist))) %>% ungroup() %>% gather(key,value,-code,-cou,-year) 
  #   figaro %<>% mutate(key = recode(key,!!!setNames(as.vector(c(code_produ64 %>% filter(figaro_nio==1) %>% 
  #     select(nace_42) %>% unlist)),c(figaro %>% distinct(key) %>% unlist)))) %>% ungroup 
  #   figaro %<>% group_by(code,key,cou,year) %>% summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% ungroup
  #   figaro %<>% spread(key,value) %>% select(code,as.vector(c(code_produ64 %>% filter(figaro_nio==1) %>% 
  #    distinct(nace_42) %>% unlist)),cou,year) %>% arrange(cou,year,factor(code, levels = c(code_produ64 %>% 
  #    filter(type=="sector") %>% distinct(nace_42) %>% unlist,paste0("M_",code_produ64 %>% 
  #    filter(type=="sector") %>% distinct(nace_42) %>% unlist),"D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")))
  #   saveRDS(figaro,paste0(path_figaro_data,"/figaro_io_prod_a42.rds"))
  # } else{
  #   figaro_prod_a42 <- readRDS(paste0(path_figaro_data,"/figaro_io_prod_a42.rds"))
  # }
  #   
  # if(length(files_path_figaro[files_path_figaro=="figaro_io_prod_a10.rds"])==0){
  #   figaro <- readRDS(paste0(path_figaro_data,"/figaro_nio_prod_ln_1020.rds"))
  #   figaro %<>% mutate(code = recode(code,!!!setNames(as.vector(c(code_produ64 %>% filter(type=="sector") %>% 
  #     select(nace_10) %>% unlist,paste0("M_",c(code_produ64 %>% filter(type=="sector") %>% select(nace_10) %>% unlist)),
  #     "D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")),c(figaro$code %>% unique))))
  #   figaro %<>% ungroup %>% group_by(code,cou,year) %>% summarize_at(c(code_produ64 %>% filter(figaro_nio==1) %>% select(nace_64) %>% 
  #     unlist),~sum(.,na.rm=TRUE)) %>% arrange(cou,year,code) %>% set_names(c("code","cou","year",c(code_produ64 %>% 
  #     filter(figaro_nio==1) %>% select(nace_64) %>% unlist))) %>% ungroup() %>% gather(key,value,-code,-cou,-year) 
  #   figaro %<>% mutate(key = recode(key,!!!setNames(as.vector(c(code_produ64 %>% filter(figaro_nio==1) %>% 
  #     select(nace_10) %>% unlist)),c(figaro %>% distinct(key) %>% unlist)))) %>% ungroup 
  #   figaro %<>% group_by(code,key,cou,year) %>% summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% ungroup
  #   figaro %<>% spread(key,value) %>% select(code,as.vector(c(code_produ64 %>% filter(figaro_nio==1) %>% 
  #    distinct(nace_10) %>% unlist)),cou,year) %>% arrange(cou,year,factor(code, levels = c(code_produ64 %>% 
  #    filter(type=="sector") %>% distinct(nace_10) %>% unlist,paste0("M_",code_produ64 %>% 
  #    filter(type=="sector") %>% distinct(nace_10) %>% unlist),"D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")))
  #   saveRDS(figaro,paste0(path_figaro_data,"/figaro_io_prod_a10.rds"))
  # } else{
  #   figaro_prod_a10 <- readRDS(paste0(path_figaro_data,"/figaro_io_prod_a10.rds"))
  # }
  # 
  # #### SUPPLY & USE
  # #######################
  # if(length(files_path_figaro[files_path_figaro=="figaro_io_supply_a10.rds"])==0){
  #   figaro <- readRDS(paste0(path_figaro_data,"/figaro_nio_supp1020.rds"))
  #   figaro %<>% mutate(code = recode(code,!!!setNames(as.vector(c(code_indus64 %>% 
  #     filter(type=="sector") %>% select(nace_10) %>% unlist)),c(figaro$code %>% unique))))
  #   figaro %<>% ungroup %>% group_by(code,cou,year) %>% 
  #     summarize_at(c(code_produ64 %>% filter(figaro_nio==1&type=="sector") %>% select(nace_64) %>% unlist),~sum(.,na.rm=TRUE)) %>% 
  #     arrange(cou,year,code) %>% set_names(c("code","cou","year",c(code_produ64 %>% filter(figaro_nio==1&type=="sector") %>% 
  #     select(nace_64) %>% unlist))) %>% ungroup()
  #   saveRDS(figaro,paste0(path_figaro_data,"/figaro_io_supply_a10.rds"))
  # } else{
  #   figaro_supply_a10 <- readRDS(paste0(path_figaro_data,"/figaro_io_supply_a10.rds"))
  # }
  #   
  # if(length(files_path_figaro[files_path_figaro=="figaro_io_use_a10.rds"])==0){
  #   figaro <- readRDS(paste0(path_figaro_data,"/figaro_nio_use1020.rds"))
  #   figaro %<>% gather(key,value,-code,-cou,-year)
  #   figaro %<>% mutate(key = recode(key,!!!setNames(as.vector(c(code_indus64 %>% 
  #     filter(type=="sector") %>% select(nace_10) %>% unlist)),c(code_indus64 %>% 
  #     filter(type=="sector") %>% select(nace_64) %>% unlist))))
  #   figaro %<>% group_by(code,key,cou,year) %>% summarize_at(c("value"),~sum(.,na.rm=TRUE)) %>% ungroup
  #   figaro %<>% spread(key,value) %>% select(code,as.vector(c(code_indus64 %>% filter(figaro_nio==1) %>% 
  #    distinct(nace_10) %>% unlist)),cou,year) %>% arrange(cou,year,factor(code, levels = c(code_produ64 %>% 
  #    filter(type=="sector") %>% distinct(nace_64) %>% unlist,paste0("M_",code_produ64 %>% 
  #    filter(type=="sector") %>% distinct(nace_64) %>% unlist),"D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")))
  #   saveRDS(figaro,paste0(path_figaro_data,"/figaro_io_use_a10.rds"))
  # } else{
  #   figaro_use_a10 <- readRDS(paste0(path_figaro_data,"/figaro_io_use_a10.rds"))
  # }

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################