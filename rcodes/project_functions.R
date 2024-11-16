##################################################################################################################################
## Functions
##################################################################################################################################

  epf_centile_groups <- function(x, WeightVar, Quantiles = seq(0,1, by = 0.01)){
    c(-Inf,Hmisc::wtd.quantile(x, weights = WeightVar, probs = Quantiles[2:(length(Quantiles)-1)], na.rm = TRUE) %>%
    as.vector(),+Inf)}
  epf_decile_groups <- function(x, WeightVar, Quantiles = seq(0,1, by = 0.1)){
    c(-Inf,Hmisc::wtd.quantile(x, weights = WeightVar, probs = Quantiles[2:(length(Quantiles)-1)], na.rm = TRUE) %>%
    as.vector(),+Inf)}
  epf_quintile_groups <- function(x, WeightVar, Quantiles = seq(0,1, by = 0.2)){
    c(-Inf,Hmisc::wtd.quantile(x, weights = WeightVar, probs = Quantiles[2:(length(Quantiles)-1)], na.rm = TRUE) %>%
    as.vector(),+Inf)}

  create_groups_epf <- function(dataset) {
      dataset %<>% select(-starts_with("DECIL"),-starts_with("QUNTL"),-starts_with("CENTL"))
      dataset %<>% left_join(dataset %>%  distinct(ANOENC,NUMERO,FACTOR,IMPEXACEQV) %>% group_by(ANOENC) %>%
      mutate(CENTLI = cut(IMPEXACEQV, breaks = epf_centile_groups(IMPEXACEQV,FACTOR),include.lowest=TRUE,labels=FALSE)) %>%
      mutate(DECILI = cut(IMPEXACEQV, breaks = epf_decile_groups(IMPEXACEQV,FACTOR),include.lowest=TRUE,labels=FALSE)) %>%
      mutate(QUNTLI = cut(IMPEXACEQV, breaks = epf_quintile_groups(IMPEXACEQV,FACTOR),include.lowest=TRUE,labels=FALSE)),
      by = c("ANOENC","NUMERO","FACTOR","IMPEXACEQV"))
      dataset %<>% left_join(dataset %>%  distinct(ANOENC,NUMERO,FACTOR,GASTOEQV) %>% group_by(ANOENC) %>%
      mutate(CENTLG = cut(GASTOEQV, breaks = epf_centile_groups(GASTOEQV,FACTOR),include.lowest=TRUE,labels=FALSE)) %>%
      mutate(DECILG = cut(GASTOEQV, breaks = epf_decile_groups(GASTOEQV,FACTOR),include.lowest=TRUE,labels=FALSE)) %>%
      mutate(QUNTLG = cut(GASTOEQV, breaks = epf_quintile_groups(GASTOEQV,FACTOR),include.lowest=TRUE,labels=FALSE)),
      by = c("ANOENC","NUMERO","FACTOR","GASTOEQV"))
  }
  
  create_carbon_groups_epf <- function(dataset) {
      dataset %<>% select(-starts_with("DECILC"),-starts_with("QUNTLC"),-starts_with("CENTLC"))
      dataset %<>% left_join(dataset %>% distinct(ANOENC,NUMERO,FACTOR,kgGHGEQV) %>% group_by(ANOENC) %>%
      mutate(CENTLC = cut(kgGHGEQV, breaks = epf_centile_groups(kgGHGEQV,FACTOR),include.lowest=TRUE,labels=FALSE)) %>%
      mutate(DECILC = cut(kgGHGEQV, breaks = epf_decile_groups(kgGHGEQV,FACTOR),include.lowest=TRUE,labels=FALSE)) %>%
      mutate(QUNTLC = cut(kgGHGEQV, breaks = epf_quintile_groups(kgGHGEQV,FACTOR),include.lowest=TRUE,labels=FALSE)),
      by = c("ANOENC","NUMERO","FACTOR","kgGHGEQV"))
  }

  clean_match_codes <- function(dataset){
    if(year_n==2020) {
      if(as.vector(unlist(dataset %>% summarize(max(as.numeric(num)))))>64){
        dataset %<>% left_join(readxl::read_xlsx(file.path(database_path,"National SUTs",
        "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla8",skip=7,col_names=TRUE) %>% rename("num"="...2") %>%
        select(num,Productos,`CPA 2008`) %>% separate(`CPA 2008`,c("cpa_r08","cpa_r08_2"),sep="-",extra="merge") %>%
        pivot_longer(`cpa_r08`:`cpa_r08_2`,names_to="length",values_to ="cpa_r08") %>% filter(!is.na(cpa_r08)) %>%
        mutate_at(c("Productos"),~str_replace(.,"escado","Pescado")) %>% mutate_at(c("cpa_r08"),~stringr::str_sub(.,1,2)) %>% 
        left_join(code_nace_r2 %>% select(-code),by=c("cpa_r08"="nace_3d"),relationship = "many-to-many") %>% 
        distinct(num,Productos,cpa_r08,nace_r2,letter),by=c("num","Productos")) %>% left_join(code_iots_city %>% 
        filter(type=="sector") %>% select(cpa_code,nace_r2_64),by=c("nace_r2"="nace_r2_64")) %>% 
        distinct(num,cpa_code,.keep_all=TRUE)
      }
      if(as.vector(unlist(dataset %>% summarize(max(as.numeric(num)))))==64){
        dataset %<>% left_join(readxl::read_xlsx(file.path(database_path,"National SUTs",
        "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla9",skip=7,col_names=TRUE) %>% rename("num"="...2") %>%
        select(num,Productos,`CPA 2008`) %>% separate(`CPA 2008`,c("cpa_r08","cpa_r08_2"),sep="-",extra="merge") %>%
        pivot_longer(`cpa_r08`:`cpa_r08_2`,names_to="length",values_to ="cpa_r08") %>% filter(!is.na(cpa_r08)) %>%
        mutate_at(c("cpa_r08"),~stringr::str_sub(.,1,2)) %>% left_join(code_nace_r2 %>% select(-code),by=c("cpa_r08"="nace_3d"),
        relationship = "many-to-many") %>% distinct(num,cpa_r08,nace_r2,letter),by=c("num")) %>% 
        left_join(code_iots_city %>% filter(type=="sector") %>% select(cpa_code,nace_r2_64),
        by=c("nace_r2"="nace_r2_64")) %>% distinct(num,cpa_code,.keep_all=TRUE)
      }
    } else if (year_n>2020) {
      if(as.vector(unlist(dataset %>% summarize(max(as.numeric(num)))))>64){
        dataset %<>% left_join(readxl::read_xlsx(file.path(database_path,"National SUTs",
        "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla6",skip=7,col_names=TRUE) %>% rename("num"="...2") %>%
        select(num,Productos,`CPA 2008`) %>% separate(`CPA 2008`,c("cpa_r08","cpa_r08_2"),sep="-",extra="merge") %>%
        pivot_longer(`cpa_r08`:`cpa_r08_2`,names_to="length",values_to ="cpa_r08") %>% filter(!is.na(cpa_r08)) %>%
        mutate_at(c("Productos"),~str_replace(.,"escado","Pescado")) %>% mutate_at(c("cpa_r08"),~stringr::str_sub(.,1,2)) %>% 
        left_join(code_nace_r2 %>% select(-code),by=c("cpa_r08"="nace_3d"),relationship = "many-to-many") %>% 
        distinct(num,Productos,cpa_r08,nace_r2,letter),by=c("num","Productos")) %>% left_join(code_iots_city %>% 
        filter(type=="sector") %>% select(cpa_code,nace_r2_64),by=c("nace_r2"="nace_r2_64")) %>% 
        distinct(num,cpa_code,.keep_all=TRUE)
      }
      if(as.vector(unlist(dataset %>% summarize(max(as.numeric(num)))))==64){
        dataset %<>% left_join(readxl::read_xlsx(file.path(database_path,"National SUTs",
        "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla7",skip=7,col_names=TRUE) %>% rename("num"="...2") %>%
        select(num,Productos,`CPA 2008`) %>% separate(`CPA 2008`,c("cpa_r08","cpa_r08_2"),sep="-",extra="merge") %>%
        pivot_longer(`cpa_r08`:`cpa_r08_2`,names_to="length",values_to ="cpa_r08") %>% filter(!is.na(cpa_r08)) %>%
        mutate_at(c("cpa_r08"),~stringr::str_sub(.,1,2)) %>% left_join(code_nace_r2 %>% select(-code),by=c("cpa_r08"="nace_3d"),
        relationship = "many-to-many") %>% distinct(num,cpa_r08,nace_r2,letter),by=c("num")) %>% 
        left_join(code_iots_city %>% filter(type=="sector") %>% select(cpa_code,nace_r2_64),
        by=c("nace_r2"="nace_r2_64")) %>% distinct(num,cpa_code,.keep_all=TRUE)
      }
    } else if(year_n>2015&year_n<2020) {
      if(as.vector(unlist(dataset %>% summarize(max(as.numeric(num)))))>64){
        dataset %<>% left_join(readxl::read_xlsx(file.path(database_path,"National SUTs",
        "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla6",skip=7,col_names=TRUE) %>% rename("num"="...2") %>%
        select(num,Productos,`CPA 2008`) %>% separate(`CPA 2008`,c("cpa_r08","cpa_r08_2"),sep="-",extra="merge") %>%
        pivot_longer(`cpa_r08`:`cpa_r08_2`,names_to="length",values_to ="cpa_r08") %>% filter(!is.na(cpa_r08)) %>%
        mutate_at(c("Productos"),~str_replace(.,"escado","Pescado")) %>% mutate_at(c("cpa_r08"),~stringr::str_sub(.,1,2)) %>% 
        left_join(code_nace_r2 %>% select(-code),by=c("cpa_r08"="nace_3d"),relationship = "many-to-many") %>% 
        distinct(num,Productos,cpa_r08,nace_r2,letter),by=c("num","Productos")) %>% left_join(code_iots_city %>% 
        filter(type=="sector") %>% select(cpa_code,nace_r2_64),by=c("nace_r2"="nace_r2_64")) %>% 
        distinct(num,cpa_code,.keep_all=TRUE)
      }
      if(as.vector(unlist(dataset %>% summarize(max(as.numeric(num)))))==64){
        dataset %<>% left_join(readxl::read_xlsx(file.path(database_path,"National SUTs",
        "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla7",skip=7,col_names=TRUE) %>% rename("num"="...2") %>%
        select(num,Productos,`CPA 2008`) %>% separate(`CPA 2008`,c("cpa_r08","cpa_r08_2"),sep="-",extra="merge") %>%
        pivot_longer(`cpa_r08`:`cpa_r08_2`,names_to="length",values_to ="cpa_r08") %>% filter(!is.na(cpa_r08)) %>%
        mutate_at(c("cpa_r08"),~stringr::str_sub(.,1,2)) %>% left_join(code_nace_r2 %>% select(-code),by=c("cpa_r08"="nace_3d"),
        relationship = "many-to-many") %>% distinct(num,cpa_r08,nace_r2,letter),by=c("num")) %>% 
        left_join(code_iots_city %>% filter(type=="sector") %>% select(cpa_code,nace_r2_64),
        by=c("nace_r2"="nace_r2_64")) %>% distinct(num,cpa_code,.keep_all=TRUE)
      }
    } else if(year_n<2016) {
      #### [NOTE] requires manual editing of cne_tot_15 in Table 8 to become Table 4
      dataset %<>% left_join(readxl::read_xlsx(file.path(database_path,"National SUTs",
      "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla4",skip=5,col_names=TRUE) %>% 
      rename("num"="...2") %>% select(num,Productos,`CPA 2008`) %>% mutate_at(c("CPA 2008"),~gsub("05-09","05-09",.)) %>%
      mutate_at(c("CPA 2008"),~if_else(num=="44a","68",.)) %>% separate(`CPA 2008`,c("cpa_r08","cpa_r08_2"),sep="-",extra="merge") %>%
      pivot_longer(`cpa_r08`:`cpa_r08_2`,names_to="length",values_to ="cpa_r08") %>% filter(!is.na(cpa_r08)) %>% 
      mutate_at(c("cpa_r08"),~stringr::str_sub(.,1,2)) %>% distinct(num,Productos,cpa_r08,cpa_r08) %>%
      left_join(code_nace_r2 %>% select(-code),by=c("cpa_r08"="nace_3d"), relationship = "many-to-many") %>% 
      distinct(num,cpa_r08,nace_r2,letter),by=c("num")) %>% filter(!num=="44a") %>% 
      left_join(code_iots_city %>% filter(type=="sector") %>% select(cpa_code,nace_r2_64),
      by=c("nace_r2"="nace_r2_64")) %>% distinct(num,cpa_code,.keep_all=TRUE)
    }
    return(dataset)
  }
    
  tax_trade_margins <- function(x,year_n){
    purchasers_margins <- data.frame()
    #### [a] first we include the NEW VECTORS that we want to correct [this step is open]
    if(year_n>2015){temp_p3_s14 <- readxl::read_xlsx(file.path(database_path,"National SUTs",
    "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla2",skip=7,col_names=TRUE) %>% 
    rename("code"="...1") %>% filter(!is.na(code)&!grepl("La valoración",code)) %>% 
    separate(code,c("num","Productos"),sep=". ",extra="merge") %>% filter(num%in%c(1:110)) %>% select(num,Productos,
    starts_with("Gasto en consumo final de los hogares")) %>% 
    rename("P3_S14_pp"="Gasto en consumo final de los hogares") %>% clean_match_codes %>% 
    group_by(cpa_code) %>% summarize_at(c("P3_S14_pp"),~sum(.,na.rm=TRUE)) %>% left_join(
    readxl::read_xlsx(file.path(database_path,"National SUTs","ES",paste0("cne_tod_",
    stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla3",skip=7,col_names=TRUE) %>% 
    rename("code"="...1") %>% filter(!is.na(code)&!grepl("La valoración",code)) %>% 
    separate(code,c("num","Productos"),sep=". ",extra="merge") %>% filter(num%in%c(1:100)) %>% 
    select(num,Productos,starts_with("Gasto en consumo final de los hogares")) %>%
    rename("P3_S14_bp"="Gasto en consumo final de los hogares") %>% clean_match_codes %>% 
    group_by(cpa_code) %>% summarize_at(c("P3_S14_bp"),~sum(.,na.rm=TRUE)),by=c("cpa_code"))}
      
    if(year_n<2016){temp_p3_s14 <- readxl::read_xlsx(file.path(database_path,"National SUTs",
    "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla2",skip=5,col_names=TRUE) %>% 
    rename("code"="...1","num"="...2") %>% filter(!is.na(code)&!grepl("La valoración",code)&!grepl("Ajuste",code)
    &!grepl("Total",code)&!grepl("Producción",code)&!grepl("Compras",code)&!grepl("Otra producción",code)) %>% 
    rename("Productos"="code") %>% select(num,Productos,starts_with("Gasto en consumo final de los hogares")) %>% 
    set_names(c("num","Productos","P3_S14_pp")) %>% mutate_at(c("num"),~as.character(.)) %>% filter(!is.na(num)) %>%
    mutate_at(c("Productos"),~str_replace(.,"escado","Pescado")) %>% mutate_at(c("num"),~gsub("44 bis","44a",.)) %>%
    clean_match_codes %>% group_by(cpa_code) %>% summarize_at(c("P3_S14_pp"),~sum(.,na.rm=TRUE)) %>% left_join(
    readxl::read_xlsx(file.path(database_path,"National SUTs","ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),
    ".xlsx")),sheet="Tabla3",skip=5,col_names=TRUE) %>% rename("code"="...1","num"="...2") %>% 
    filter(!is.na(code)&!grepl("La valoración",code)&!grepl("Ajuste",code)&!grepl("Total",code)&
    !grepl("Producción",code)&!grepl("Compras",code)&!grepl("Otra producción",code)) %>% 
    rename("Productos"="code") %>% select(num,Productos,starts_with("Gasto en consumo final de los hogares")) %>% 
    set_names(c("num","Productos","P3_S14_bp")) %>% mutate_at(c("num"),~as.character(.)) %>% filter(!is.na(num)) %>%
    mutate_at(c("Productos"),~str_replace(.,"escado","Pescado")) %>% mutate_at(c("num"),~gsub("44 bis","44a",.)) %>%
    clean_match_codes %>% group_by(cpa_code) %>% summarize_at(c("P3_S14_bp"),~sum(.,na.rm=TRUE)),by=c("cpa_code"))}
      
    #### [b] afterwards, we use the supply at purchasers and basics to derive the implicit
    #### trade&transport and tax margins, which are based on the GLOBAL EXCESS or difference between
    #### the two prices instead of each industry differences to avoid problems with the negatives
    if(year_n<2016){temp_pp_margins <- readxl::read_xlsx(file.path(database_path,"National SUTs",
    "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla1",skip=5,col_names=TRUE) %>%
    rename("code"="...1","num"="...2") %>% filter(!is.na(code)&!grepl("La valoración",code)&!grepl("Ajuste",code)
    &!grepl("Total",code)&!grepl("Producción",code)&!grepl("Compras",code)&!grepl("Otra producción",code)) %>% 
    rename("Productos"="code") %>% select(num,Productos,starts_with("Márgenes"),starts_with("Impuestos"),
    starts_with("Total oferta")) %>% set_names(c("num","Productos","trade_margins","transport_margins",
    "taxes_production","supply_basics","supply_purchasers")) %>% mutate_at(c("num"),~as.character(.)) %>%
    mutate_at(c("Productos"),~str_replace(.,"escado","Pescado")) %>% mutate_at(c("num"),~gsub("44 bis","44a",.)) %>%
    clean_match_codes}
      
    if(year_n>2015){temp_pp_margins <- readxl::read_xlsx(file.path(database_path,"National SUTs",
    "ES",paste0("cne_tod_",stringr::str_sub(year_n,3,4),".xlsx")),sheet="Tabla1",skip=7,col_names=TRUE) %>% 
    rename("code"="...1") %>% filter(!is.na(code)&!grepl("La valoración",code)&!grepl("Ajuste",code)
    &!grepl("TOTAL",code)&!grepl("Producción",code)&!grepl("Compras",code)&!grepl("Otra producción",code)) %>% 
    separate(code,c("num","Productos"),sep=". ",extra="merge") %>% mutate_at(c("Productos"),~str_remove_all(.,fixed(".P"))) %>%
    mutate_at(c("num"),~as.numeric(.)) %>% mutate_at(c("num"),~if_else(is.na(.),as.numeric(lag(.))+1,as.numeric(.))) %>%
    select(num,Productos,starts_with("Márgenes"),starts_with("Impuestos"),starts_with("Total oferta")) %>% 
    set_names(c("num","Productos","trade_margins","transport_margins","taxes_production","supply_basics",
    "supply_purchasers")) %>% mutate_at(c("num"),~as.character(.)) %>%
    mutate_at(c("Productos"),~str_replace(.,"escado","Pescado")) %>% clean_match_codes}
    
    temp_pp_margins %<>% group_by(cpa_code) %>% summarize_at(c("trade_margins",
    "transport_margins","taxes_production","supply_basics","supply_purchasers"),~sum(.,na.rm=TRUE)) %>%
    #### [==> we recompute totals from broken-down vectors
    mutate(`ttm_com`= (trade_margins+transport_margins),`tls_com`= taxes_production) %>%
    select(-starts_with("trade"),-starts_with("taxes"),-starts_with("transport")) %>% 
    rename("pp_com"="supply_purchasers","bp_com"="supply_basics") %>% 
    #### [==> we calculate the EXCESS as "pp_com" ad then calculate implicit rates
    #### as a SHARE of total MARGINS over EXCESS
    mutate(pp_sum = abs(pp_com-bp_com)) %>% 
    mutate(`%tls` = tls_com/pp_sum,`%ttm` = ttm_com/pp_sum) %>% 
    #### [==> we perform some validations [14/03/2024]
    mutate(pp_com_check = round(pp_com - (bp_com + (`%tls`*pp_sum)+(`%ttm`*pp_sum)),5)) %>% 
    mutate(`ttm_sum` = if_else(ttm_com>0,ttm_com,0)) %>% mutate(`ttm_sum` = sum(`ttm_sum`,na.rm=TRUE)) %>% 
    mutate(`%ttm_exc` = ttm_com/`ttm_sum`) %>% mutate_at(c("%ttm","%tls"),~if_else(is.nan(.)|is.infinite(.),1,.)) %>% 
    select(-bp_com,-pp_com,-ends_with("sum"),-ends_with("check"))
    
    #### [c] next, we join the new vectors with the implicit rates and derive the NEW GLOBAL EXCESS
    #### to derive the industry TOTALS for TAXES AND Trade&Transport MARGINS, which we then validate
    #### they add up to previous purchasing prices ==> the direction here is from PP to BP
    temp_pp_margins %<>% left_join(temp_p3_s14,by=c("cpa_code")) %>% rename("pp_com"="P3_S14_pp","bp_com"="P3_S14_bp") %>%
    #### [==> compute ABSOLUTE value of EXCESS to multiply by trade share
    #### [ATTENTION] we start with trade and derive taxes because trade needs to balance out to 0
    mutate(pp_excess = abs(pp_com-bp_com)) %>% mutate_at(c("ttm_com"),~`%ttm`*pp_excess) %>% 
    #### [==> after deriving the initial values of trade margins we sum through positive values and derive
    #### the ratio of the excess sum of trade to the total sum of trade, so that we can rescale positive values
    #### [==> rescale positive only because negatives match up to the total discrepancy between sectors
    #### making the hypothetical negative adjustment violate the constraint set by the level of basic prices
    mutate(`ttm_sum` = if_else(ttm_com>0,ttm_com,0)) %>% mutate(`%ttm_sum` = sum(`ttm_com`,na.rm=TRUE)/sum(`ttm_sum`,na.rm=TRUE)) %>%
    mutate_at(c("ttm_sum"),~.*(1-`%ttm_sum`)) %>% mutate_at(c("ttm_com"),~if_else(ttm_com<0,.,`ttm_sum`)) %>%
    #### [==> taxes come as the residual of the excess minus trade, but signs are inverted
    mutate_at(c("tls_com"),~pp_excess-abs(ttm_com)) %>% 
    #### [==> we need to change signs and we can the retrieve basic prices
    mutate_at(c("tls_com"),~if_else(`%tls`>0&`tls_com`<0,-1*.,.)) %>% 
    mutate_at(c("tls_com"),~if_else(`%tls`<0&`tls_com`>0,-1*.,.)) %>% 
    mutate(pp_com_check = round(pp_com - (bp_com +tls_com+ttm_com),5))
    
    if(x=="to_basics"){
      temp_pp_margins %<>%
      #### [d] now we can calculate the IMPLICIT RATES for the NEW VECTORS
      #### [==> for TAXES we need to use excess total instead of net "pp_com" to avoid the
      #### problem with sectors that have 0 amounts for purchaser prices.
      #### [NOTE] since we talk about consumption and not output, there are sectors with no
      #### HH demand and, hence, NO TAXES ON PRODUCTS
      mutate(`%tls` = if_else(pp_com==0,tls_com/(pp_excess-tls_com),tls_com/(pp_com-tls_com))) %>%
      #### [ATTENTION] it is crucial to perform the sum outside the if_else statemente
      mutate(`ttm_sum` = if_else(ttm_com>0,ttm_com,0)) %>% mutate(`ttm_sum` = sum(`ttm_sum`,na.rm=TRUE)) %>% 
      mutate(`%ttm+`   = if_else(ttm_com>0,ttm_com/(pp_com-tls_com-ttm_com),0)) %>% 
      mutate(`%ttm`    = if_else(ttm_com<0,ttm_com/`ttm_sum`,`%ttm+`)) %>% select(-`ttm_sum`) %>%
      mutate(`%ttm_t&t`= ttm_com/pp_com) %>%
      mutate_at(c("%tls","%ttm"),~if_else(is.na(.)|is.infinite(.),0,.)) %>%
      #### [==> check that trade balances summarize(sum(ttm_com))
        
      #### [e] finally, we proceed to check that basic prices are recoverable, and, thus,
      #### to indicate how this prices can be recovered from any vector when the one
      #### at BASIC PRICES is UNKNOWN
      mutate(`+ttm` = if_else(`%ttm`>=0,((pp_com/(1+`%tls`))/(1+`%ttm`))*`%ttm`,0)) %>%
      mutate(`ttm`  = if_else(`%ttm`<0,`%ttm`*sum(`+ttm`,na.rm=TRUE),`+ttm`)) %>%
      #### [==> again, the implicit tax rates needs to be computed differently to avoid
      #### null numbers, in this case of a sector for which households consume no value (G46)
      mutate(`tls`  = if_else(pp_com>0,pp_com - (pp_com/(1+`%tls`)),pp_excess-`ttm`)) %>% 
      mutate(bp_com = pp_com-tls-ttm) %>% mutate(pp_com_check = round(pp_com-(bp_com +tls_com+ttm_com),5)) %>%
      mutate_at(c("bp_com"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% 
      mutate(flag_null = if_else(pp_com==0,"No values: % by global excess",NA)) %>% 
      mutate(pp_excess_tot=sum(pp_com-bp_com,na.rm=TRUE)/sum(pp_com,na.rm=TRUE)) %>% 
      mutate(pp_excess_sec=(pp_com-bp_com)/sum(pp_com,na.rm=TRUE)) %>%
      select(cpa_code,pp_excess_tot,pp_excess_sec,`%tls`,`%ttm`,`%ttm_t&t`,flag_null,pp_com_check)
    }
    
    if(x=="to_purchasers"|x=="to_producers"){
      temp_pp_margins %<>%
      #### [d] now we can calculate the IMPLICIT RATES for the NEW VECTORS
      #### [==> for TAXES we need to use excess total instead of net "pp_com" to avoid the
      #### problem with sectors that have 0 amounts for purchaser prices.
      #### [NOTE] since we talk about consumption and not output, there are sectors with no
      #### HH demand and, hence, NO TAXES ON PRODUCTS
      mutate(`%tls` = if_else(pp_com==0,tls_com/(pp_excess+tls_com),tls_com/(bp_com+tls_com))) %>%
      #### [ATTENTION] it is crucial to perform the sum outside the if_else statemente
      mutate(`ttm_sum` = if_else(ttm_com>0,ttm_com,0)) %>% mutate(`ttm_sum` = sum(`ttm_sum`,na.rm=TRUE)) %>% 
      mutate(`%ttm+`   = if_else(ttm_com>0,ttm_com/(bp_com+tls_com+ttm_com),0)) %>% 
      mutate(`%ttm`    = if_else(ttm_com<0,ttm_com/`ttm_sum`,`%ttm+`)) %>% select(-`ttm_sum`) %>%
      mutate(`%ttm_t&t`= ttm_com/bp_com) %>%
      mutate_at(c("%tls","%ttm"),~if_else(is.na(.)|is.infinite(.),0,.)) %>%
      mutate(flag_0=if_else(pp_com==0,1,0)) %>%
      #### [==> check that trade balances summarize(sum(ttm_com))
      
      #### [e] finally, we proceed to check that basic prices are recoverable, and, thus,
      #### to indicate how this prices can be recovered from any vector when the one
      #### at BASIC PRICES is UNKNOWN
      mutate(`+ttm` = if_else(`%ttm`>=0,((bp_com/(1-`%tls`))/(1-`%ttm`))*`%ttm`,0)) %>%
      mutate(`ttm`  = if_else(`%ttm`<0,`%ttm`*sum(`+ttm`,na.rm=TRUE),`+ttm`)) %>%
      #### [==> again, the implicit tax rates needs to be computed differently to avoid
      #### null numbers, in this case of a sector for which households consume no value (G46)
      mutate(`tls`  = (bp_com/(1-(`%tls`)))-bp_com) %>% mutate(pp_com = bp_com+tls+ttm) %>%
      mutate(bp_com_check = round(bp_com -(pp_com-tls-ttm),5)) %>%mutate(pp_com = bp_com+tls+ttm) %>%
      mutate(tls_check = round(tls_com-tls,5),ttm_check = round(ttm_com-ttm,5)) %>%
      mutate(pp_com = bp_com+tls+ttm) %>% mutate_at(c("bp_com"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% 
      mutate(flag_null = if_else(bp_com==0,"No values: % by global excess",NA)) %>% 
      mutate(pp_excess_tot=sum(pp_com-bp_com,na.rm=TRUE)/sum(pp_com,na.rm=TRUE)) %>% 
      mutate(pp_excess_sec=(pp_com-bp_com)/sum(pp_com,na.rm=TRUE)) %>%
      select(cpa_code,pp_excess_tot,pp_excess_sec,`%tls`,`%ttm`,`%ttm_t&t`,flag_0,flag_null,pp_com_check)
    }
    purchasers_margins %<>% bind_rows(temp_pp_margins %>% mutate(year=year_n))
    return(purchasers_margins)
  }
  
  consumer_to_basic <- function(dataset,x,y) { 
    #consumer_to_basic(temp_emissions,"to_producers",2019)
    #### validated that the purchasers prices are retrievable from output basics using the
    #### formulas with given vectors [15/03/24]
    #### [NOTE] technically this is computed from CPAs not NACEs
    if(x=="to_basics"){
      temp_dataset <- dataset %>% as.data.frame %>% rownames_to_column("code") %>% set_names(c("cpa_code","pp_com")) %>%
      left_join(tax_trade_margins(x,y) %>% ungroup %>% filter(year==y),by=c("cpa_code")) %>% as_tibble %>% 
      mutate(`+ttm` = if_else(`%ttm`>=0,((pp_com/(1+`%tls`))/(1+`%ttm`))*`%ttm`,0)) %>% 
      mutate(`ttm`  = if_else(`%ttm`<0,`%ttm`*sum(`+ttm`,na.rm=TRUE),`+ttm`)) %>% 
      #### [==> again, the implicit tax rates needs to be computed differently to avoid
      #### null numbers, in this case of a sector for which households consume no value (G46)
      mutate(`tls`  = if_else(pp_com>0,pp_com - (pp_com/(1+`%tls`)),0)) %>% 
      mutate(bp_com = pp_com-`ttm`-`tls`,pp_com_check = round(pp_com-(bp_com +tls+ttm),5)) %>%
      mutate_at(c("bp_com"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% 
      select(cpa_code,bp_com) %>% rename("P3_S14"="bp_com","code_cpa"="cpa_code")
  }
    if(x=="to_purchasers"){
      temp_dataset <- dataset %>% as.data.frame %>% rownames_to_column("code") %>% set_names(c("cpa_code","bp_com")) %>%
      left_join(tax_trade_margins(x,y) %>% ungroup %>% filter(year==y),by=c("cpa_code")) %>% as_tibble %>%
      mutate(`+ttm` = if_else(`%ttm`>=0,((bp_com/(1-`%tls`))/(1-`%ttm`))*`%ttm`,0)) %>%
      mutate(`ttm`  = if_else(`%ttm`<0,`%ttm`*sum(`+ttm`,na.rm=TRUE),`+ttm`)) %>%
      mutate(`tls`  = (bp_com/(1-(`%tls`)))-bp_com) %>% 
      #### this corrects by changing taxes the cases where purchaser prices are absolute 0
      mutate_at(c("tls"),~if_else(flag_0==1,.-(bp_com+tls+ttm),.)) %>%
      mutate(pp_com = bp_com+tls+ttm) %>% mutate(bp_com_check = round(bp_com -(pp_com-tls-ttm),5)) %>%
      mutate_at(c("pp_com"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>%
      select(cpa_code,pp_com) %>% rename("P3_S14"="pp_com","code_cpa"="cpa_code")
    }
    if(x=="basics_to_producers"){
      temp_dataset <- dataset %>% as.data.frame %>% rownames_to_column("code") %>% set_names(c("cpa_code","bp_com")) %>%
      left_join(tax_trade_margins("to_producers",y) %>% ungroup %>% filter(year==y),by=c("cpa_code")) %>% as_tibble %>%
      mutate(`+ttm` = if_else(`%ttm`>=0,((bp_com/(1-`%tls`))/(1-`%ttm`))*`%ttm`,0)) %>%
      mutate(`ttm`  = if_else(`%ttm`<0,`%ttm`*sum(`+ttm`,na.rm=TRUE),`+ttm`)) %>% 
      mutate(fp_com = bp_com+`ttm`) %>% mutate_at(c("fp_com"),~if_else(is.nan(.)|is.infinite(.),0,.)) %>%
      select(cpa_code,fp_com) %>% rename("P3_S14"="fp_com","code_cpa"="cpa_code")
    }
    return(temp_dataset)
  }

supply.use  <- function(y,z,r){ # supply.use("2013","supply","city")
  if(r == "region"){
    use_table <- region_use_raw
    sup_table <- region_sup_raw
    codes_table <- code_iots_mad %>% select(nace_r2_d3r,cpa_r2,type) %>%
    set_names(c("nace_codes","cpa_codes","type"))
  }
  if(r == "city"){
    use_table <- city_use_raw
    sup_table <- city_sup_raw
    codes_table <- code_iots_city %>% select(nace_code,cpa_code,type) %>%
    set_names(c("nace_codes","cpa_codes","type"))
  }
  if(z == "supply"){
    sup_m   <- sup_table %>% ungroup
    sector  <- sup_m %>% dplyr::select(all_of(c(codes_table %>% filter(type=="sector") %>% 
    distinct(nace_codes) %>% unlist %>% as.vector()))) %>% names
    product <- sup_m %>% dplyr::select(code) %>% distinct(code) %>% filter(code%in%c(codes_table %>% 
    filter(type=="sector") %>% distinct(cpa_codes) %>% unlist)) %>% unlist %>% as.vector()
    output  <- c("CIFOBADJ","OP_RES","P1")
    final   <- c("TI","TS_BP")
    io      <- sup_m %>% filter(year==as.character(y)) %>% filter(code%in%c(product,output)) %>%
    dplyr::select(code,all_of(as.vector(sector)),all_of(as.vector(final))) %>% 
    tibble::column_to_rownames(var="code")
    io %<>% as.data.frame()
  }
  if(z == "use"){
    use_m <- use_table %>% ungroup
    sector  <- use_m %>% dplyr::select(all_of(c(codes_table %>% filter(type=="sector") %>% 
    distinct(nace_codes) %>% unlist %>% as.vector()))) %>% names
    product  <- use_m %>% dplyr::select(code) %>% distinct(code) %>% filter(code%in%c(codes_table %>% 
    filter(type=="sector") %>% distinct(cpa_codes) %>% unlist)) %>% unlist %>% as.vector()
    
    va_c <- use_m %>% filter(code%in%c(codes_table %>% filter(type%in%c("value")) %>% 
    distinct(nace_codes) %>% unlist %>% as.vector)) %>% distinct(code) %>% unlist %>% as.vector
    fd_c  <- use_m %>% dplyr::select(all_of(codes_table %>% filter(type=="final"&
    !nace_codes%in%c("P6_ES","P6_EU","P6_RW")) %>% 
    distinct(nace_codes) %>% unlist %>% as.vector)) %>% names
    tx_c <-  use_m %>% filter(code%in%c(codes_table %>% filter(type%in%c("value_m")) %>% #!&code=="CIFOBADJ"
    distinct(nace_codes) %>% unlist %>% as.vector)) %>% distinct(code) %>% unlist %>% as.vector
    # use_m %>% dplyr::select(all_of(codes_table %>% filter(type=="final") %>% 
    # distinct(nace_codes) %>% unlist %>% as.vector),P6) %>% transmute(p6 = P6-P6_ES-P6_EU-P6_RW)
    
    #### creation of the main flows matrix: products by industries (55x55)
    Z  <- use_m %>% filter(year==as.character(y)) %>% filter(code%in%c(product)) %>%
    dplyr::select(code,any_of(sector)) %>% tibble::column_to_rownames(var="code")
    #### creation of the value added matrix spanning the whole interindustry and final demand vectors (3x63)
    va <- use_m %>% filter(year==as.character(y)) %>% filter(code%in%c(va_c)) %>%
    dplyr::select(code,all_of(sector),all_of(fd_c)) %>% tibble::column_to_rownames(var="code")
    #### creation of the taxes and margins matrix spanning the whole interindustry and final demand vectors (4x55)
    vm <- use_m %>% filter(year==as.character(y)) %>% filter(code%in%c(tx_c)) %>%
    dplyr::select(code,all_of(sector)) %>% #mutate_at(c(sector),~if_else(code=="D21X31"|code=="OP_NRES",-1*.x,.x)) %>%
    tibble::column_to_rownames(var="code")
    #### creation of the final demand matrix spanning the country final demand components and domestic products (55x8)
    fd <- use_m %>% filter(year==as.character(y)) %>% filter(code%in%c(product)) %>%
    dplyr::select(code,all_of(fd_c)) %>% arrange(code) %>% tibble::column_to_rownames(var="code")
    #### creation of the export vector (55x8)
    ex <- use_m %>% filter(year==as.character(y)) %>% filter(code%in%c(product)) %>%
    dplyr::select(code,starts_with("P6_")) %>% arrange(code) %>% tibble::column_to_rownames(var="code")
    #transmute(P6 = rowSums(across(P6_ES:P6_RW),na.rm=TRUE))
    #### creation of the export vector summing through other countries interindustry and final demands (1x64)
    im <- use_m %>% filter(year==as.character(y)) %>% filter(code%in%c(paste0(product,"_IM"))) %>%
    dplyr::select(code,all_of(sector)) %>% arrange(code) %>% as.data.frame %>%
    column_to_rownames("code") #%>% summarize_at(c(sector),~sum(.,na.rm=TRUE)) %>% set_rownames("P7")
    mv <- bind_rows(im,vm) %>% colSums(.,na.rm=TRUE) %>% bind_cols(P7=.) %>% as.data.frame() %>% set_rownames(c(sector))
    #check that GDP = GVA ###### sum(va)-(sum(fd)+(sum(ex)-sum(mv)))
    #it is very important to include "vm" in the matrix
    io <- Z %>% bind_cols(fd,ex) %>% bind_rows(im) %>% bind_rows(vm,va)
    io %<>% bind_rows(colSums(.,na.rm=TRUE)) %>% magrittr::set_rownames(c(rownames(io),"P1"))
    io %<>% bind_cols((io %>% rowSums(.,na.rm=TRUE)) %>% cbind() %>% magrittr::set_colnames(c("P1")))
    io[is.na(io)] <- 0
    #check that total ouput sums match sum(io[118,2:56])-sum(io[1:55,63])
  }
  return(io)
}

sut.to.iot <- function(y,t,r,n,d){# sut.to.iot('2019',"",'TM',"D","city")
  # y <- "2019";t <- "";r <- "TM";n <- "D";d<-"city" 
  if(d == "region"){
    use <- region_use
    supply <- region_sup
    codes_table <- code_iots_mad %>% select(nace_r2_d3r,cpa_r2,type) %>%
    set_names(c("nace_codes","cpa_codes","type"))
  }
  if(d == "city"){
    use <- city_use
    supply <- city_sup
    codes_table <- code_iots_city %>% select(nace_code,cpa_code,type) %>%
    set_names(c("nace_codes","cpa_codes","type"))
  }
  sector  <- supply %>% select(all_of(as.vector(codes_table %>% filter(type=="sector") %>% distinct(nace_codes) %>% 
  unlist))) %>% names %>% as.vector
  product  <- paste0("CPA_",supply %>% select(all_of(as.vector(codes_table %>% filter(type=="sector") %>% distinct(nace_codes) %>% 
  unlist))) %>% names %>% as.vector)
  va_c <- use %>% filter(code%in%c(codes_table %>% filter(type%in%c("value")) %>% 
  distinct(nace_codes) %>% unlist %>% as.vector)) %>% distinct(code) %>% unlist %>% as.vector
  fd_c  <- use %>% dplyr::select(all_of(codes_table %>% filter(type=="final"|
  nace_codes%in%c("P6_ES","P6_EU","P6_RW")) %>% distinct(nace_codes) %>% unlist %>% as.vector)) %>% names
  tx_c <-  use %>% filter(code%in%c(codes_table %>% filter(type%in%c("value_m")) %>% #!&code=="CIFOBADJ"
  distinct(nace_codes) %>% unlist %>% as.vector)) %>% distinct(code) %>% unlist %>% as.vector
  # definition of the sectors  
  sector <- sector[!sector%in%c(paste0(t))]
  product <- product[!product%in%c(paste0(paste0("CPA_",t)))]
  
  ########################################
  #### EUROSTAT S&U Manual p. 348-357 ####
  ########################################
  
  #### [1] === [INPUT COEFFICIENTS OF USE TABLE] 
  #### Use matrix for intermediates (product by industry)
  #### Matrix B = [b_{ij}] = [u_{ij}/x_{j}] is built from U = [u_{ij}], which contains the coefficients relating 
  #### the total commodity i demanded by sector j, and the total output vector x_{j}.
  U <- use %>% filter(code%in%c(paste0(product))) %>% filter(year==y) %>% select(all_of(c("code",
  sector))) %>% tibble::column_to_rownames("code") %>% as.matrix()
  Um <- use %>% filter(code%in%c(paste0(product,"_IM"))) %>% filter(year==y) %>% select("code",
  c(sector)) %>% tibble::column_to_rownames("code") %>% as.matrix() %>%
  set_rownames(c(rownames(U)))
  if(stringr::str_sub(r,2) == "T"){U <- U + Um}
  #### === g = Column vector of industry output
  g <- use %>% filter(year==y) %>% filter(code=="P1") %>% select(all_of(sector)) %>% unlist %>% as.numeric
  #### B = U * inv(diag(g)) Input requirements for products per unit of output of an industry (intermediates)
  B  <- U%*%diag(c(1/g)) %>% as.data.frame() %>% set_names(c(colnames(U))) %>% as_tibble() %>%
  mutate_at(c(sector),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% as.matrix()
  Bm <- Um%*%diag(c(1/g)) %>% as.data.frame() %>% set_names(c(colnames(U))) %>% as_tibble() %>%
  mutate_at(c(sector),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% as.matrix()
  #### L = W * inv(diag(g)) Input requirements for value added per unit of output of an industry (primary input)
  W <- use %>% filter(code%in%c("D21X31","CIFOBADJ","OP_NRES","OP_RES","D29X39","D1","B2A3G")) %>% filter(year==y) %>% 
  select(all_of(c("code",sector))) %>% tibble::column_to_rownames("code") %>% as.matrix()
  #### Y = Final demand matrix (product by category)
  FD <- use %>% filter(code%in%c(paste0(product))) %>% filter(year==y) %>% select(code,starts_with("P3"),
  starts_with("P5"),starts_with("P6")) %>% tibble::column_to_rownames("code") %>% as.matrix
  FDM <- use %>% filter(!code%in%c(paste0(sector))) %>% filter(year==y) %>% select(code,starts_with("P3"),
  starts_with("P5"),starts_with("P6")) %>% tibble::column_to_rownames("code") %>% as.matrix
  fd <- FD %>% as.data.frame %>% transmute(y = rowSums(across(P3_S14:P6_RW),na.rm=TRUE)) %>% 
  unlist %>% as.vector()
  
  #### [2] === [MARKET SHARE COEFFICIENTS OF SUPPLY TABLE]: 
  #### Industry Source of Commodity Outputs (industry by product)
  #### A) Matrix D = [d_{ij}] = [v_{ij}/q_{j}] is built from V = [v_{ij}], which contains the coefficients relating 
  #### the total output of industry i by commodity j, and denotes the fraction of total commodity j output
  #### that was produced by industry i
  V <- supply %>% filter(code%in%c(product)) %>% filter(year==y) %>% select(all_of(c("code",
  paste0(sector)))) %>% tibble::column_to_rownames("code") %>% as.matrix %>% t
  VT <- supply %>% filter(code%in%c(product)) %>% filter(year==y) %>% select(all_of(c("code",
  paste0(sector)))) %>% tibble::column_to_rownames("code") %>% as.matrix
  #### q = Column vector of product output
  q <- use %>% filter(code%in%c(paste0(product))) %>% filter(year==y) %>% select("code","P1") %>% 
  tibble::column_to_rownames("code") %>% unlist %>% as.numeric
  #### C = VT * inv(diag(g)) Product-mix matrix (share of each product in output of an industry)
  C <- VT%*%diag(c(1/g)) %>% as.data.frame() %>% set_colnames(c(rownames(V))) %>% 
  mutate_at(c(sector),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% as.matrix()
  #### D = V * inv(diag(q)) Market shares matrix (contribution of each industry to the output of a product)
  D <- V%*%diag(c(1/q)) %>% as.data.frame() %>% set_colnames(c(colnames(V))) %>%
  mutate_at(c(paste0(product)),~if_else(is.nan(.)|is.infinite(.),0,.)) %>% as.matrix()
  
  if(n == "A"){
  #### === [MODEL A] === #### [negatives]
  #### Product-by-product input-output table based on product technology assumption
  #### Each product is produced in its own specific way, irrespective of the industry where it is produced
  #### Transformation matrix                      [T = inv(VT) * diag(q)]
  TM0  <- solve(VT); TMv <- diag(q)
  TM   <- solve(VT)%*%diag(q)
  #### Input coefficients intermediates           [A = U * T * inv((diag(q))]
  A  <- U%*%TM%*%solve(diag(q)) %>% 
  set_colnames(colnames(V))
  #### Input coefficients value added             [E = W * T]
  R  <- W%*%TM %>% set_colnames(colnames(V))
  #### Intermediates                              [S = U * T]
  Z  <- U%*%TM %>% set_colnames(colnames(V))
  #### Intermediates                              [S = U * T]
  Zm  <- Um%*%TM %>% set_colnames(colnames(V))
  #### Final demand                               [Y = Y]
  Y  <- FD
  #### Total output                               [q = inv(I - A) * y]
  p1   <- solve(diag(nrow(A))-A)%*%fd %>%
  set_colnames("P1")
  #### Leontief Inverse
  L <- solve(diag(nrow(A))-A)
  }
  
  if(n == "B"){
  #### === [MODEL B] === #### [no negatives]
  #### Product-by-product input-output table based on industry technology assumption
  #### Each industry has its own specific way of production, irrespective of its product mix
  #### Transformation matrix                      [T = inv(diag(g)) * V]
  TM0  <- V; TMv <- solve(diag(g))
  TM <- solve(diag(g))%*%V
  #### Input coefficients intermediates           [A = U * T * inv(diag (q))]
  A  <- U%*%TM%*%solve(diag(q)) %>% 
  set_colnames(colnames(V)) %>%
  set_rownames(colnames(V))
  #### Value added                                [E = W * T]
  R  <- W%*%TM
  #### Intermediates                              [S = U * T]
  Z  <- U%*%TM
  #### Intermediates                              [S = U * T]
  Zm  <- Um%*%TM %>% set_colnames(colnames(V))
  #### Final demand                               [Y = Y]
  Y  <- FD                                                                           
  #### Total output                               [q = inv(I - A) * y]
  p1   <- solve(diag(nrow(A))-A)%*%fd %>%
  set_colnames("P1")
  #### Leontief Inverse
  L <- solve(diag(nrow(A))-A)
  }
  
  if(n == "C"){
  #### === [MODEL C] === #### [negatives]
  #### Industry-by-industry input-output table based on fixed industry sales structure assumption
  #### Each industry has its own specific sales structure, irrespective of its product mix
  #### Transformation matrix                      [T = diag(g) * inv(VT)]
  TM0  <- solve(VT); TMv <- diag(g)
  TM <- diag(g)%*%solve(VT)
  #### Input coefficients intermediates           [A = T * U * inv(diag (g))]
  A  <- TM%*%U%*%solve(diag(g)) %>% 
  set_colnames(rownames(V)) %>%
  set_rownames(rownames(solve(VT)))
  #### Value added                                [W = W]
  R  <- W
  #### Intermediates                              [B = T * U]
  Z  <- TM%*%U %>% 
  set_rownames(rownames(solve(VT)))
  #### Intermediates                              [B = T * U]
  Zm  <- TM%*%Um %>% 
  set_rownames(rownames(solve(VT)))
  #### Final demand                               [F = T * Y]
  Y  <- TM%*%FD
  #### Total output                               [g = inv[I - T * U * inv(diag(g))] * T * y]
  p1   <- solve(diag(nrow(A))-(TM%*%U%*%
  solve(diag(g))))%*%TM%*%fd %>%
  set_colnames("P1") %>%
  set_rownames(rownames(A))
  #### Leontief Inverse
  L <- solve(diag(nrow(A))-A)
  }
  
  if(n == "D"){
  #### === [MODEL D] === #### [no negatives]
  #### Industry-by-industry input-output table based on fixed product sales structure assumption
  #### Each product has its own specific sales structure, irrespective of the industry where it is produced
  #### Transformation matrix                      [T = V * inv(diag(q))]
  TM0  <- V; TMv <- solve(diag(q))
  TM  <- V%*%solve(diag(q))
  #### Input coefficients intermediates           [A = T * U * inv(diag (g))]
  A   <- TM%*%U%*%solve(diag(g)) %>% 
  set_colnames(rownames(V))%>% 
  set_rownames(rownames(V))
  #### Value added                                [W = W]
  R   <- W
  #### Intermediates                              [B = T * U]
  Z   <- TM%*%U
  #### Intermediates                              [B = T * U]
  Zm  <- TM%*%Um
  #### Final demand                               [F = T * Y]
  Y  <- TM%*%FD
  #### Total output                               [g = inv[I - T * U * inv(diag(g))] * T * y]
  p1   <- solve(diag(nrow(A))-(TM%*%U%*%
  solve(diag(g))))%*%TM%*%fd %>%
  set_colnames("P1") %>%
  set_rownames(rownames(A))
  #### Leontief Inverse
  L   <- solve(diag(nrow(A))-A)
  }
  
  if(r=='C')  {return(C)}
  if(r=='V')  {return(V)}
  if(r=='P1'){return(p1)}
  if(r=='B1G'){return(R)}
  if(r=='P2M'){return(Zm)}
  if(r=='TM')  {return(TM)}
  if(r=='P3AP6'){return(Y)}
  if(r=='TM0')  {return(TM0)}
  if(r=='TMv')  {return(TMv)}
  if(r=='Z' | r=='ZT') {return(Z)}
  if(r=='A' | r=='AT') {return(A)}
  if(r=='L' | r=='LT') {return(L)}
}

#### [NEED TO PROCESS THE IMPORT V]
nio.mad.matrix <- function(y,t,r,n,d){# nio.mad.matrix('2013',"",'L',"D","region")
  # y <- "2019";t<-"";r<-"M";n<-"D";d <- "city"
  #### [A] Inputs
  if(d == "region"){
    use_table <- region_use
    sup_table <- region_sup
    codes_table <- code_iots_mad %>% select(nace_r2_d3r,cpa_r2,type) %>%
    set_names(c("nace_codes","cpa_codes","type"))
  }
  if(d == "city"){
    use_table <- city_use
    sup_table <- city_sup
    codes_table <- code_iots_city %>% select(nace_code,cpa_code,type) %>%
    set_names(c("nace_codes","cpa_codes","type"))
  }
  m_sut <-  sut.to.iot(y,"",paste0("Z"),n,d) %>% 
  as.data.frame() %>% rownames_to_column("code")
  m_sutm <- sut.to.iot(y,"",paste0("P2M"),n,d)%>% 
  as.data.frame() %>% rownames_to_column("code")
  m_sutw <- sut.to.iot(y,"",paste0("B1G"),n,d)%>% 
  as.data.frame() %>% rownames_to_column("code")
  m_suto <- sut.to.iot(y,"",paste0("P1"),n,d)%>% 
  as.data.frame() %>% rownames_to_column("code")
  m_suty <- sut.to.iot(y,"",paste0("P3AP6"),n,d)%>% 
  as.data.frame() %>% rownames_to_column("code")
  
  sector  <- m_sut %>% as.data.frame %>% distinct(code) %>% filter(!code%in%c(codes_table %>% 
  filter(!type=="sector") %>% distinct(nace_codes) %>% unlist)) %>% unlist %>% sort %>% as.vector
  va_c <- use_table %>% filter(code%in%c(codes_table %>% filter(type%in%c("value")) %>% 
  distinct(nace_codes) %>% unlist %>% as.vector)) %>% distinct(code) %>% unlist %>% as.vector
  fd_c  <- use_table %>% dplyr::select(all_of(codes_table %>% filter(type=="final"|
  nace_codes%in%c("P6_ES","P6_EU","P6_RW")) %>% distinct(nace_codes) %>% unlist %>% as.vector)) %>% names
  tx_c <-  use_table %>% filter(code%in%c(codes_table %>% filter(type%in%c("value_m")) %>% #!&code=="CIFOBADJ"
  distinct(nace_codes) %>% unlist %>% as.vector)) %>% distinct(code) %>% unlist %>% as.vector
  # definition of the sectors  
  sector <- sector[!sector%in%c(paste0(t))]
  if(n == "A" | n == "B"){sector <- sector[!sector%in%c(paste0("CPA_",t))]}
  
  #### [B] Definition of data sets
  #### ==> matrix of interindustry flows
  Z   <- m_sut %>% filter(code %in% c(sector)) %>% mutate(rowname=code)%>%column_to_rownames("rowname") %>% 
  dplyr::select(all_of(sector))
  #### ==> BP to PP adjustment block
  VM  <- m_sutw %>% filter(code %in% c(tx_c)) %>% tibble::column_to_rownames("code") %>% select(all_of(sector))
  #### ==> import vector (adjusted to PP)
  P7   <- m_sutm %>% filter(code %in% c(sector,c(code_fiot64 %>% filter(type=="vimport")%>% select(induse) %>% 
  unlist %>% as.vector)))%>% select(paste0(sector)) %>% summarize_all(~sum(.)) %>% t() %>% magrittr::set_colnames("P7")
  #### ==> import matrix (unadjusted)
  P7M  <-  m_sutm %>% filter(code %in% c(sector)) %>% select(paste0(sector)) %>% set_rownames(c(paste0(rownames(Z),"_IM")))
  P7MI <- P7M+diag(as.vector(colSums(VM)))
  #### ==> output vector
  P1   <- m_suto %>% filter(code %in% c(sector)) %>% column_to_rownames("code") %>% select(P1)
  #### ==> final demand block
  F0  <- m_suty %>% filter(code %in% c(paste0(sector))) %>% column_to_rownames("code")
  #### ==> gross domestic product
  GDP  <- m_suty %>% filter(code %in% c(paste0(sector))) %>% bind_cols(P7) %>% bind_cols(cbind("VM" = colSums(VM))) %>%
  column_to_rownames("code") %>% mutate(P6 = rowSums(across(starts_with("P6")),na.rm=TRUE)) %>% 
  transmute(FD=rowSums(across(P3_S14:P5M))+(P6-(P7+VM)))
  #### ==> total consumption
  P3  <- m_suty %>% select(code,starts_with("P3")) %>%  filter(code %in% c(sector)) %>%
  column_to_rownames("code") %>% transmute(P3=rowSums(across(starts_with("P3"))))
  #### ==> domestic demand
  P3AP5  <- m_suty %>% select(code,P3_S14,P3_S15,P3_S13,P51G,P5M) %>%  filter(code %in% c(paste0(sector))) %>%
  column_to_rownames("code") %>% transmute(P3AP5=rowSums(across(c("P3_S14","P3_S15","P3_S13","P51G","P5M")),na.rm=TRUE))
  #### ==> value added block
  B1G  <- m_sutw %>% filter(code%in%c(va_c)) %>% column_to_rownames("code") %>% select(any_of(sector))
  #### ==> vector of total imported imputs
  P2 <- m_sutm %>% filter(code%in%c(sector)) %>% select(sector) %>% summarize_all(~sum(.))
  #### ==> vector of total exports
  P6 <- m_suty %>% select(code,starts_with("P6_")) %>% filter(code%in%c(unique(sector))) %>% 
  as.data.frame() %>% column_to_rownames("code") %>% transmute(P6 = rowSums(across(P6_ES:P6_RW),
  na.rm=TRUE))
  #### ==> domestic demand
  P3AP7  <- m_suty %>% dplyr::select(code,P3_S14:P5M) %>% filter(code %in% c(paste0(sector))) %>%
  column_to_rownames("code") %>% bind_cols(P7) %>% transmute(P3AP7=rowSums(across(P3_S13:P7)))
  P3AP7M  <- m_suty %>% dplyr::select(code,P3_S14:P5M) %>% filter(code %in% c(paste0(sector))) %>%
  column_to_rownames("code") %>% bind_cols(P7MI %>% summarize_all(~sum(.)) %>% t() %>% 
  magrittr::set_colnames("P7")) %>% transmute(P3AP7=rowSums(across(P3_S13:P7)))
  #### ==> vector of household consumption
  P3S14 <-  m_suty %>% select(code,"P3_S14") %>% filter(code%in%c(unique(sector))) %>% column_to_rownames("code")
  #### ==> vector of total investment demand
  P5 <- m_suty %>% dplyr::select(code,"P51G","P5M") %>% filter(code %in% c(paste0(sector))) %>% 
  column_to_rownames("code") %>% transmute(P5=rowSums(across(P51G:P5M)))
  #### ==> vector of total labor compensation
  D1 <- m_sutw %>% filter(code %in% c("D1")) %>% select(paste0(sector)) %>% t() %>% magrittr::set_colnames("D1")
  #### ==> vector of profits
  #### ==> vector of total gross operating surplus and mixed income
  B2A3G <- m_sutw %>% filter(code %in% c("B2A3G")) %>% select(paste0(sector)) %>% t() %>% 
  magrittr::set_colnames("B2A3G")
  
  #### ==> [IMPORTANT] matrix of flows including domestic and imported
  if(stringr::str_sub(r,2) == "T"){
  Z <- m_sut %>% bind_rows(m_sutm) %>% select(code,all_of(sector)) %>% filter(code%in%c(sector,paste0(sector))) %>%
  group_by(code) %>% summarize_at(c(sector),~sum(.,na.rm=TRUE)) %>% column_to_rownames("code") %>% as.matrix()}
  
  #### [C] Derived matrices
  M <- Z %>% bind_cols(F0) %>% bind_cols(P1) %>% bind_rows(P7M) %>% bind_rows(VM) %>% 
  bind_rows(B1G) %>% mutate_at(c(sector,fd_c,"P1"),~replace_na(.,0))
  M %<>% bind_rows(M %>% summarize_all(~sum(.,na.rm=TRUE)) %>% set_rownames("P1")) 
  M %<>% mutate_all(~replace_na(.,0))
  #### ==> [IMPORTANT] matrix of imported flows
  if(stringr::str_sub(r,2) == "M"){Z <- m_sutm %>% column_to_rownames("code") %>% as.matrix()}
  #### ==> matrix of technical coefficients (demand)
  A <- as.matrix(Z)%*%solve(diag(as.vector(unlist(P1)))) %>% set_colnames(colnames(Z)) %>% as.matrix()
  #### ==> matrix of technical coefficients (demand)
  VM_m <- as.matrix(VM)%*%solve(diag(as.vector(unlist(P1)))) %>% set_colnames(colnames(Z)) %>% as.matrix()
  #### ==> matrix of technical coefficients (supply)
  B <- solve(diag(as.vector(unlist(P1))))%*%as.matrix(Z) %>% set_rownames(rownames(Z)) %>% as.matrix()
  #### ==> Leontief Inverse
  L  <- solve(diag(nrow(A))-A)
  #### ==> Ghosh Inverse [PENDING: GT should have P6 and not P7M]
  G  <- solve(diag(nrow(A))-A)
  #### ==> vertically integrated inputs matrix
  H  <- as.matrix(A)%*%as.matrix(L)
  if(r=='M') {return(M)}
  if(r=='Z') {return(Z)}
  if(r=='A') {return(A)}
  if(r=='B') {return(B)}
  if(r=='L') {return(L)}
  if(r=='G') {return(G)}
  if(r=='H') {return(H)}
  if(r=='ZT'){return(Z)}
  if(r=='P1'){return(P1)}
  if(r=='P2'){return(P2)}
  if(r=='F0'){return(F0)}
  if(r=='P3'){return(P3)}
  if(r=='P5'){return(P5)}
  if(r=='P6'){return(P6)}
  if(r=='P7'){return(P7)}
  if(r=='D1'){return(D1)}
  if(r=='AM'){return(A_m)}
  if(r=='P7M'){return(P7M)}
  if(r=='GDP'){return(GDP)}
  if(r=='B1G'){return(B1G)}
  if(r=='VMT'){return(VM_m)}
  if(r=='P3AP5'){return(P3AP5)}
  if(r=='P3AP7'){return(P3AP7)}
  if(r=='B2A3G'){return(B2A3G)}
  if(r=='P3S14'){return(P3S14)}
  if(r=='P3AP5M'){return(P3AP5M)}
  if(r=='P3AP7M'){return(P3AP7M)}
  if(r=='AT' | r=='AM'){return(A)}
  if(r=='BT' | r=='BM'){return(B)}
  if(r=='LT' | r=='LM'){return(L)}
  if(r=='GT' | r=='GM'){return(G)}
  if(r=='HT' | r=='HM'){return(H)}
}

wio.to.nio.city  <- function(x,y,z){ # wio.to.nio.city("ES","2019","industry")
  #x <- "ES";y <- "2019";z<-"industry"
  if(z == "industry"){figaro <- city_mrio_industry}
  if(z == "product") {figaro <- city_mrio_product}
  codes  <- figaro %>% dplyr::select(code) %>% separate(code,c("cou","sector"), sep = "_", extra = "merge")
  sec    <- codes %>% distinct(sector) %>% filter(!sector%in%c(code_fiot64 %>% filter(type%in%c("value","vimport")&!induse=="B1G") %>% 
  dplyr::select(induse) %>% unlist)) %>% unlist
  va_c   <- codes %>% filter(!sector%in%c(sec,"D21X31","OP_RES","OP_NRES")) %>% distinct(sector) %>% unlist #"D21X31","OP_RES","OP_NRES"
  fd_c   <- code_fiot64 %>% filter(type=="final") %>% dplyr::select(induse) %>% filter(induse%in%c("P3_S13",
  "P3_S14","P3_S15","P51G","P5M")) %>% unlist
  cou    <- figaro %>% dplyr::select(code) %>% separate(code,c("cou","sec"), sep = "_", extra = "merge") %>%
  distinct(cou) %>% filter(!cou==x) %>% unlist # filter(cou!="W2") %>% 
  sector_list <- paste0(x,'_',sec)
  sector_all_list <- list()
  for(n in c(cou)){sector_all_list[[n]] <- paste0(n,'_',sec)}
  sector_all_list <- sector_all_list %>% unlist %>% set_names(c())
  fd_all_list <- list()
  for(n in c(cou)){fd_all_list[[n]] <- paste0(n,'_',fd_c)}
  fd_all_list <- fd_all_list %>% unlist %>% set_names(c())
  out <- figaro %>% filter(year==as.character(y)) %>% dplyr::select(code,any_of(sector_list)) %>%
  dplyr::select(-code) %>% summarise_all(~sum(.))
  tot <- figaro %>% filter(year==as.character(y)) %>% filter(code%in%c(sector_list)) %>%
  dplyr::select(-year) %>% column_to_rownames("code") %>% transmute(tot = rowSums(across(where(is.numeric))))
  Z  <- figaro %>% filter(year==as.character(y)) %>% filter(code%in%c(sector_list)) %>%
  dplyr::select(code,any_of(sector_list)) %>% tibble::column_to_rownames(var="code")
  va <- figaro %>% filter(year==as.character(y)) %>% filter(code%in%c(paste0("W2_",va_c))) %>%
  dplyr::select(code,starts_with(paste0(x,"_"))) %>% tibble::column_to_rownames(var="code")
  vm <- figaro %>% filter(year==as.character(y)) %>% filter(code%in%c(paste0("W2_",c("D21X31","OP_RES","OP_NRES")))) %>%
  dplyr::select(code,starts_with(paste0(x,"_"))) %>% tibble::column_to_rownames(var="code")
  fd <- figaro %>% filter(year==as.character(y)) %>% filter(code%in%c(sector_list)) %>%
  dplyr::select(code,any_of(paste0(x,"_",fd_c))) %>% tibble::column_to_rownames(var="code")
  ex <- figaro %>% filter(year==as.character(y)) %>% filter(code%in%c(sector_list)) %>% 
  dplyr::select(code,any_of(sector_all_list),any_of(fd_all_list)) %>% tibble::column_to_rownames(var="code") %>% 
  transmute(P6 = rowSums(across(where(is.numeric))))
  im <- figaro %>% filter(year==as.character(y)) %>% filter(code%in%c(sector_all_list)) %>%
  dplyr::select(code,starts_with(paste0(x,"_"))) %>% separate(code,c("cou","code"), sep = "_", extra = "merge") %>%
  arrange(code) %>% dplyr::select(-cou) %>% group_by(code) %>% summarize_at(c(paste0(x,"_",c(sec,fd_c))),~sum(.)) %>% 
  tibble::column_to_rownames(var="code") %>% set_rownames(paste0("M_",sec))
  mv <- bind_rows(im,vm) %>% dplyr::select(-c(paste0(x,"_",fd_c))) %>% colSums(.) %>% bind_cols(P7=.)
  #me <- bind_cols(xv,mv) %>% magrittr::set_colnames(c("P6","P7")) %>% mutate(XM = P6-P7)
  #check that GDP = GVA ###### sum(va)-(sum(fd)+(sum(ex)-sum(mv)))
  #[ATTENTION] it is very important to include "vm" in the matrix
  io <- Z %>% bind_cols(fd,ex) %>% bind_rows(im) %>% bind_rows(vm,va)
  io %<>% bind_rows(io %>% colSums(.,na.rm=TRUE)) %>% magrittr::set_rownames(c(rownames(io),"P1"))
  io %<>% bind_cols((io %>% rowSums(.,na.rm=TRUE)) %>% cbind() %>% magrittr::set_colnames(c("P1")))
  io[c(seq(nrow(io)-3,nrow(io),1)),c(seq(ncol(io)-6,ncol(io),1))] <- 0
  rownames(io) <- c(sec,paste0("M_",sec),rownames(vm)%>% stringr::str_remove("W2_"),rownames(va) %>% stringr::str_remove("W2_"),"P1")
  colnames(io) <- c(sec,colnames(fd) %>% stringr::str_remove(paste0(x,"_")),"P6","P1")
  #check that total ouput sums match sum(io[-135,2:65])-sum(io[1:64,72])
  io %<>% mutate_all(~replace_na(.,0)) %>% rownames_to_column("code")
  if(z=="industry"){
  io %<>% mutate(code = recode(code,!!!setNames(as.vector(c(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code) %>% unlist,
  paste0(c(code_iots_city %>% filter(type=="sector") %>% distinct(nace_code) %>% unlist),"_IM"),
  "D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")),c(io$code %>% unique))))
  }
  if(z=="product"){
  io %<>% mutate(code = recode(code,!!!setNames(as.vector(c(code_iots_city %>% filter(type=="sector") %>% distinct(cpa_code) %>% unlist,
  paste0(c(code_iots_city %>% filter(type=="sector") %>% distinct(cpa_code) %>% unlist),"_IM"),
  "D21X31","OP_RES","OP_NRES","D1","D29X39","B2A3G","P1")),c(io$code %>% unique))))
  }
  return(io)
}

nio.city.matrix <- function(x,y,t,r,k){
  # nio.city.matrix('AR','2010',"",'Z',"industry","42")
  # x <- "ES";y <- "2019";t <- "";r <- "GDP";k <- "industry";n <- "64"
  if(k == "industry"){figaro <- wio.to.nio.city(x,y,k) %>% mutate(cou = paste0(x),year = paste0(y))}
  if(k == "product") {figaro <- wio.to.nio.city(x,y,k) %>% mutate(cou = paste0(x),year = paste0(y))}
  #### [A] Definition of the main grouping variables
  final <- c("P3_S13","P3_S14","P3_S15","P51G","P5M")
  codes <- figaro %>% dplyr::distinct(code) %>% filter(!code%in%c(code_fiot64 %>% filter(type%in%c("vimport","value")) %>% 
  dplyr::select(induse) %>% unlist,"P1")&!grepl("_IM",code)&!code%in%c(t)) %>% mutate(rowname=code)%>% column_to_rownames("rowname")
  if(k == "product") {codes %<>% filter(!code%in%c(paste0("CPA_",t)))}
  if(k == "industry"){codes %<>% filter(!code%in%c(paste0(t)))}
  if(k == "product") {M <- figaro %>% filter(cou %in% c(x)&year %in% c(y)) %>% filter(!code%in%c(paste0("CPA_",t),paste0("CPA_",t,"_IM"))) %>%
  dplyr::select(code,all_of(codes$code),all_of(final),P6,P1)}
  if(k == "industry"){M <- figaro %>% filter(cou %in% c(x)&year %in% c(y)) %>% filter(!code%in%c(paste0(t),paste0(t,"_IM"))) %>% 
  dplyr::select(code,all_of(codes$code),all_of(final),P6,P1)}
  #### == codes for variable selection
  va_c <- M %>% filter(code%in%c("D1","B2A3G","D29X39")) %>% distinct(code) %>% unlist %>% as.vector
  fd_c <- M %>% select(all_of(c("P3_S13","P3_S14","P3_S15","P51G","P5M"))) %>% names %>% as.vector
  tx_c <- M %>% filter(code%in%c("D21X31","OP_RES","OP_NRES")) %>% distinct(code) %>% unlist %>% as.vector

  #### [B] Definition of data sets
  #### ==> matrix of interindustry flows
  Z   <- M %>% filter(code %in% c(codes$code)) %>% mutate(rowname=code)%>%column_to_rownames("rowname") %>% 
  dplyr::select(all_of(codes$code))
  #### ==> BP to PP adjustment block
  VM  <- M %>% filter(code %in% c(code_fiot64 %>% filter(type=="vimport") %>% dplyr::select(induse) %>% unlist)) %>% 
  tibble::column_to_rownames("code") %>% dplyr::select(codes$code)
  #### ==> import vector (adjusted to PP) [Incluir los impuestos indirectos]
  P7   <- M %>% filter(code %in% c(paste0(codes$code,"_IM"),c(code_fiot64 %>% filter(type=="vimport")%>%dplyr::select(induse) %>% unlist)))%>%
  dplyr::select(paste0(codes$code)) %>% summarize_all(~sum(.)) %>% t() %>% magrittr::set_colnames("P7")
  #### ==> import matrix (unadjusted)
  P7M  <- M %>% filter(code %in% c(paste0(codes$code,"_IM")))%>%mutate(code=stringr::str_remove(code,"M_"))%>%
  column_to_rownames("code") %>% dplyr::select(any_of(codes$code))
  P7M   <- P7M+diag(as.vector(colSums(VM)))
  #### ==> import matrix (unadjusted)
  P7MI  <- M %>% filter(code %in% c(paste0(codes$code,"_IM"),tx_c))%>%mutate(code=stringr::str_remove(code,"M_"))%>%
  column_to_rownames("code") %>% dplyr::select(any_of(codes$code)) %>% summarize_all(~sum(.)) %>% t() %>% 
  magrittr::set_colnames("P7M")
  #### ==> output vector
  P1   <- M %>% filter(code %in% c(codes$code)) %>% mutate(rowname=code)%>% column_to_rownames("rowname") %>% dplyr::select(P1)
  #### ==> final demand block
  F0  <- M %>% dplyr::select(code,all_of(final),"P6") %>%  filter(code %in% c(paste0(codes$code))) %>% bind_cols(P7) %>%
  column_to_rownames("code")
  #### ==> domestic demand
  P3AP5  <- M %>% dplyr::select(code,all_of(final)) %>%  filter(code %in% c(paste0(codes$code))) %>%
  column_to_rownames("code") %>% transmute(P3AP5=rowSums(across(all_of(final))))
  P3AP5M <- P3AP5 %>% bind_cols(M %>% dplyr::select(code,all_of(final)) %>%  filter(code %in% c(paste0(codes$code,"_IM"))) %>%
  column_to_rownames("code") %>% transmute(P3AP5M=rowSums(across(all_of(final))))) %>%
  transmute(P3AP5 = P3AP5+P3AP5M)
  #### ==> domestic demand
  P3AP6  <- M %>% dplyr::select(code,P3_S13:P6) %>%  filter(code %in% c(paste0(codes$code))) %>%
  column_to_rownames("code") %>% transmute(P3AP6=rowSums(across(P3_S13:P6)))
  #### ==> domestic demand
  P3AP7  <- M %>% dplyr::select(code,all_of(final)) %>%  filter(code %in% c(paste0(codes$code))) %>%
  column_to_rownames("code") %>% bind_cols(P7) %>% transmute(P3AP7=rowSums(across(P3_S13:P7)))
  P3AP7M  <- M %>% dplyr::select(code,all_of(final)) %>%  filter(code %in% c(paste0(codes$code))) %>%
  column_to_rownames("code") %>% bind_cols(P7MI) %>% transmute(P3AP7=rowSums(across(P3_S13:P7M)))
  #### ==> total consumption
  P3  <- M %>% dplyr::select(code,starts_with("P3")) %>%  filter(code %in% c(codes$code)) %>%
  column_to_rownames("code") %>% transmute(FD=rowSums(across(starts_with("P3"))))
  #### ==> value added block
  B1G  <- M %>% mutate(rowname=code) %>% column_to_rownames("rowname") %>% filter(code%in%c(code_fiot64 %>% filter(type=="value") %>% 
  dplyr::select(induse) %>% unlist)) %>% dplyr::select(any_of(codes$code))
  #### ==> vector of total imported imputs
  P2 <- M %>% filter(code%in%c(codes$code)) %>% dplyr::select(codes$code) %>% summarize_all(~sum(.))
  #### ==> vector of total exports
  P6 <-  M %>% dplyr::select(code,starts_with("P6")) %>% filter(code%in%c(unique(codes$code))) %>% as.data.frame() %>%
  column_to_rownames("code")
  #### ==> vector of household consumption
  P3_S14 <-  M %>% dplyr::select(code,"P3_S14") %>% filter(code%in%c(unique(codes$code))) %>% as.data.frame() %>%
  column_to_rownames("code")
  #### ==> vector of household consumption + Imports
  P3_S14M  <- P3_S14 %>% bind_cols(M %>% filter(code %in% c(paste0(codes$code,"_IM"))) %>% tibble::column_to_rownames("code") %>% 
  dplyr::select(P3_S14) %>% as.data.frame() %>% rename("P3_S14M"="P3_S14")) %>% 
  transmute(P3_S14M = P3_S14+P3_S14M)
  #### ==> vector of total investment demand
  P5 <-  M %>% dplyr::select(code,"P51G","P5M") %>%  filter(code %in% c(paste0(codes$code))) %>% as.data.frame() %>%
  column_to_rownames("code") %>% transmute(P5=rowSums(across(P51G:P5M)))
  #### ==> vector of total labor compensation
  D1 <- M %>% filter(code %in% c("D1")) %>% dplyr::select(paste0(codes$code)) %>% t() %>% magrittr::set_colnames("D1")
  #### ==> vector of total gross operating surplus and mixed income
  B2A3G <- M %>% filter(code %in% c("B2A3G")) %>% dplyr::select(paste0(codes$code)) %>% t() %>% magrittr::set_colnames("B2A3G")
  #### ==> gross domestic product
  GDP  <- M %>% dplyr::select(code,all_of(final),"P6") %>%  filter(code %in% c(paste0(codes$code))) %>% bind_cols(P7) %>%
  column_to_rownames("code") %>% mutate(FD=rowSums(across(all_of(c(final,"P6"))))) %>% 
  mutate(GDP=rowSums(across(all_of(c(final))))+(P6-P7))
  
  #### ==> [IMPORTANT] matrix of flows including domestic and imported
  if(stringr::str_sub(r,2) == "T"){
    Z <- M %>% select(code,codes$code) %>% filter(code%in%c(codes$code,paste0(codes$code,"_IM"))) %>%
    mutate_at(c("code"),~stringr::str_replace_all(.,setNames(c(codes$code),c(paste0(codes$code,"_IM"))))) %>%
    group_by(code) %>% summarize_at(c(codes$code),~sum(.,na.rm=TRUE)) %>% column_to_rownames("code")
  }
  
  #### [C] Derived matrices
  output_vect <- diag(c(1/t(P1))); output_vect[is.infinite(output_vect)] <- 0
  #### ==> matrix of technical coefficients (demand)
  if(stringr::str_sub(r,1,1) == "A" | stringr::str_sub(r,1,1) == "L"){A <- as.matrix(Z)%*%output_vect %>% 
  as.data.frame() %>% magrittr::set_colnames(codes$code) %>% mutate_all(~replace_na(.,0) %>% replace(.,is.infinite(.),0))}
  #### ==> matrix of technical coefficients (supply)
  if(stringr::str_sub(r,1,1) == "B" | stringr::str_sub(r,1,1) == "G"){B <- output_vect%*%as.matrix(Z) %>% 
  as.data.frame() %>% magrittr::set_rownames(codes$code) %>% mutate_all(~replace_na(.,0) %>% replace(.,is.infinite(.),0))}
  #### ==> Leontief Inverse
  if(stringr::str_sub(r,1,1) == "L"){L  <- solve(as.matrix(diag(length(A))-A))}
  #### ==> Ghosh Inverse [PENDING: GT should have P6 and not P7M]
  if(stringr::str_sub(r,1,1) == "G"){G  <- solve(as.matrix(diag(length(B))-B))}
  #### ==> vertically integrated inputs matrix
  if(r == "H"){A <- as.matrix(Z)%*%output_vect %>% as.data.frame() %>% magrittr::set_colnames(codes$code) %>%
  mutate_all(~replace_na(.,0) %>% replace(.,is.infinite(.),0))
  H  <- as.matrix(A)%*%as.matrix(L)}
  if(r=='M') {return(M)}
  if(r=='Z') {return(Z)}
  if(r=='A') {return(A)}
  if(r=='B') {return(B)}
  if(r=='L') {return(L)}
  if(r=='G') {return(G)}
  if(r=='H') {return(H)}
  if(r=='AT'){return(A)}
  if(r=='BT'){return(B)}
  if(r=='LT'){return(L)}
  if(r=='GT'){return(G)}
  if(r=='HT'){return(H)}
  if(r=='P1'){return(P1)}
  if(r=='P2'){return(P2)}
  if(r=='F0'){return(F0)}
  if(r=='P3'){return(P3)}
  if(r=='P5'){return(P5)}
  if(r=='P6'){return(P6)}
  if(r=='P7'){return(P7)}
  if(r=='D1'){return(D1)}
  if(r=='ZT'){return(Z)}
  if(r=='P7M'){return(P7M)}
  if(r=='GDP'){return(GDP)}
  if(r=='B1G'){return(B1G)}
  if(r=='P3AP5'){return(P3AP5)}
  if(r=='P3AP6'){return(P3AP6)}
  if(r=='P3AP7'){return(P3AP7)}
  if(r=='B2A3G'){return(B2A3G)}
  if(r=='P3S14'){return(P3_S14)}
  if(r=='P3AP5M'){return(P3AP5M)}
  if(r=='P3AP7M'){return(P3AP7M)}
  if(r=='P3S14M'){return(P3_S14M)}
}

gras_update <- function(u,v,X0,epsilon,max.iter){
    # raw code from Oliver Reiter
    # http://zauster.gitlab.io/blog/2018/02/28/gras-algorithm-in-rcpparmadillo/
    # example from Temurshoev, U., R.E. Miller and M.C. Bouwmeester (2013), 
    # A note on the GRAS method, Economic Systems Research, 25, pp. 361-367.
    # X0 <- matrix(c(7,3,5,-3,2,9,8,.1,-2,.1,2,.001),ncol=4,nrow=3,byrow=TRUE)
    # u <- c(15, 26, -1);v <- c(9, 16, 17, -2)
    # u <- as.numeric(unlist(u_vector));v <- as.numeric(v_vector);X0 <- as.matrix(X0)
    # ==== dimensions of vectors and matrix do not match
    if(missing(epsilon)) {epsilon <- 0.0001}
    if(missing(max.iter)) {max.iter <- 10000}
    sinv <- function(x) {
      x <- 1 / as.vector(x)
      x[!is.finite(x)] <- 1
      return(x)
    }
    X0<- X0; 
    X0[is.na(X0)] <- 0;X0[X0==0] <- 1
    v <- v; v[v==0] <- 1
    u <- u; u[u==0] <- 1
    m <- nrow(X0)
    n <- ncol(X0)
    P <- X0
    N <- abs(X0)
    N[X0 >= 0] <- 0
    P[X0 < 0] <- 0
    r <- rep(1, m)
    s <- rep(1, n)
    error <- 1
    iter <- 1
    while((error > epsilon) & (iter <= max.iter)) {
          s.old <- s
          pj.r <- t(P) %*% r
          nj.r <- t(N) %*% sinv(r)
          s <- sinv(2 * pj.r) * (v + sqrt(v^2 + 4 * pj.r * nj.r))
          s.alt <- -sinv(v) * nj.r
          s[s == 0] <- s.alt[s == 0]
  
          pi.s <- P %*% s
          ni.s <- N %*% sinv(s)
          r <- sinv(2 * pi.s) * (u + sqrt(u^2 + 4 * pi.s * ni.s))
          r.alt <- -sinv(u) * ni.s
          r[r == 0] <- r.alt[r == 0]
          
          diff <- abs(s.old - s)
          error <- max(diff)
          error.pos <- which.max(diff)
          iter <- iter + 1
    }
    print(paste0('Number of iterations ',iter,' and maximum error of ',error))
      r <- as.vector(r)
      s <- as.vector(s)
      X <- sweep(r * P, 2, s, "*") - sweep(sinv(r) * N, 2, sinv(s), "*")
      X[is.na(X0)] <- 0;X[mapply(is.infinite, X0)] <- 0
      return(X)
}

##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################