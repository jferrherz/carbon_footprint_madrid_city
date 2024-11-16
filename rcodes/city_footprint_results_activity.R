##################################################################################################################################
## Block C - DETAILED RESULTS
##################################################################################################################################

  #################### 
  # [A] FIGURES
  #########################################################
  library(latex2exp)

  #### [a] plot of gdp related emissions
  temp_city_emissions_gdp <- city_emissions_gdp %>% filter(year==year_n) %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(code,ref_area,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("GDP"="obs_value") %>% 
  left_join(code_industries %>% distinct(code,name_short),by=c("code")) %>%
  mutate_at(c("code"),~if_else(code=="B","B_E",.)) %>% mutate_at(c("name_short"),
  ~if_else(code=="B_E","Mining & energy supply",.)) %>% mutate_at(c("name_short"),~paste0(.," (",code,")")) %>% 
  filter(!code%in%c("A","T"))
    
    temp_city_emissions_gdp %>% ggplot(aes(x = GDP, y = reorder(name_short,GDP),
    fill=factor(ref_area,levels=c(rev(unique(temp_city_emissions_gdp$ref_area)[c(3,4,2,6,1,5)]))))) + 
    geom_bar(stat="identity") + scale_fill_brewer(palette="YlOrRd") + labs(fill='') + ylab("NACE (rev.2)") + 
    xlab(TeX("kt$\\CO_2$")) + guides(fill=guide_legend(nrow=2)) +
    theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "bottom")
    ggsave(file.path(getwd(),"paper","graphics",paste0("emisiones_gdp_madrid_area_industry_ES30_",
    year_n,".pdf")),height = 5,width = 10)
    
    city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
    filter(cou=="ES30"&!grepl("ES",counter)) %>% select(cou,code,obs_value,year) %>% 
    filter(year==year_n) %>% summarize_at(c("obs_value"),~round(sum(.,na.rm=TRUE),0))

  #### [b] evolution of emissions across the period by geographical area
  city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(ref_area,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("GDP"="obs_value") %>%
  pivot_wider(names_from = "year",values_from = "GDP") %>% mutate(`2010_b`=!!sym(paste0(year_str_io))) %>%
  mutate_at(c(as.character(seq(year_str_io,year_end_city))),~.*100/`2010_b`) %>% select(-`2010_b`) %>%
  pivot_longer(!!sym(paste0(year_str_io)):!!sym(paste0(year_end_city)),
  names_to = "year",values_to = "GDP") %>%
  
    ggplot(aes(y = GDP, x = year,group=ref_area,color=ref_area)) + 
    geom_line(size=1)  + labs(fill='') + ylab("") + scale_colour_brewer(palette="RdYlBu") +
    xlab("") + ylab(TeX("$\\CO_2$ emissions (2010=100)")) + guides(fill=guide_legend(nrow=2)) +
    scale_x_discrete(breaks = seq(2010, 2021, by = 2)) +
    theme(text = element_text(size = 25),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "bottom",legend.title = element_blank())
  
  ggsave(file.path(getwd(),"paper","graphics",paste0("emisiones_crecimiento_por_pais_",
  year_n,".pdf")),height = 6,width = 8)
  
  #### [c] plot of spending related emissions
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
    factor(ref_area,levels=c(rev(unique(ref_area)[c(3,4,2,6,1,5)]))))) + 
    geom_bar(stat="identity") + scale_fill_brewer(palette="YlOrRd") + 
    labs(fill='') + ylab("COICOP groups (2 digits)") + 
    xlab(TeX("kt$\\CO_2$")) + guides(fill=guide_legend(nrow=2)) +
    theme(text = element_text(size = 15),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "bottom")
  
  ggsave(file.path(getwd(),"paper","graphics",paste0("emisiones_consumption_madrid_area_ES30_",
  year_n,".pdf")),height = 5,width = 10)
  
  #### [d] pie chart
  library(RColorBrewer)
  emiss_sector_driver <- city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% 
  left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(cou,code,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) 
  
  emiss_sector_supplier <- city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("counter_code"),~stringr::str_sub(.,1,1)) %>% 
  left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(cou,counter_code,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE))
  
  mycolors <- rev(colorRampPalette(brewer.pal(5,"RdYlBu"))(18))
  emiss_sector_driver %>% filter(year==year_n) %>% mutate(type="Driver") %>%
  bind_rows(emiss_sector_supplier %>% rename("code"="counter_code") %>% 
  mutate(type="Supplier") %>% filter(year==year_n)) %>% group_by(type) %>%
  mutate_at(c("obs_value"),~./sum(.,na.rm=TRUE)) %>% ungroup %>% 
  mutate_at(c("code"),~if_else(.=="B","B_E",.)) %>%
  arrange(rev(type)) %>%  mutate(code_null="") %>%
    ggplot(aes(y=obs_value, x=factor(type,levels=c("Supplier","Driver")),fill=code,
    label=if_else(obs_value > 0.05,code, code_null))) + 
    geom_bar(stat="identity") + scale_y_continuous(labels = scales::percent) +
    geom_text(size = 8, position = position_stack(vjust = 0.5)) +
    xlab("") + ylab(TeX("Distribution of $\\CO_2$ emissions (%)")) +
    guides(fill=guide_legend(nrow=2)) + scale_fill_manual(values = mycolors) +
    theme(text = element_text(size = 25),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "none")
  
  ggsave(file.path(getwd(),"paper","graphics",paste0("emisiones_pie_chart_",
  year_n,".pdf")),height = 6,width = 8)
  
  #### [d] evolution of scopes
  library(RColorBrewer)
  mycolors <- rev(colorRampPalette(brewer.pal(5,"BrBG"))(5))
  total_direct_gdp_co2_change <- city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,
  names_to = "counter",values_to = "obs_value") %>% filter(cou=="ES30") %>% select(cou,code,
  obs_value,year) %>% group_by(year) %>% summarize_at(c("obs_value"),~round(sum(.,na.rm=TRUE),0)) %>%
  left_join(city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",
  values_to = "obs_value") %>% filter(cou=="ES30"&grepl("ES30",counter)) %>% 
  select(cou,code,obs_value,year) %>% group_by(year) %>% summarize_at(c("obs_value"),
  ~round(sum(.,na.rm=TRUE),0)),by=c("year")) %>% set_names(c("year","total","direct")) %>%
  mutate_at(c("total"),~.-direct) %>% mutate(ratio=total/direct)
    
  total_direct_gdp_co2_change %>% pivot_longer(`total`:`ratio`,names_to="scope",
  values_to="obs_value") %>% pivot_wider(names_from="year",values_from="obs_value") %>% 
  mutate(base=`2010`) %>% mutate_at(c(as.character(2010:2021)),~.*100/base) %>%
  select(-base) %>% pivot_longer(`2010`:`2021`,names_to="year",values_to="obs_value") %>%
  mutate_at(c("scope"),~case_when(.=="direct"~"Scope 1",.=="total"~"Scopes 2-3",
  .=="ratio"~"Ratio S2-S3 to S1")) %>%
   ggplot(aes(y = obs_value, x = year,group=factor(scope,levels=c("Scope 1","Scopes 2-3",
   "Ratio S2-S3 to S1")),colour=factor(scope,levels=c("Scope 1","Scopes 2-3",
   "Ratio S2-S3 to S1")))) + geom_line(size=1)  + labs(fill='') + ylab("") +
    xlab("") + ylab(TeX("Normalized $\\CO_2e$ (2010=100)")) + guides(fill=guide_legend(nrow=2)) +
    scale_x_discrete(breaks = seq(2010, 2021, by = 2)) + #scale_colour_brewer(type = "div") +
    scale_color_manual(values=mycolors[c(2,4,5)])+
    theme(text = element_text(size = 25),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "bottom",legend.title = element_blank())
  
  ggsave(file.path(getwd(),"paper","graphics",paste0("total_direct_series.pdf")),height = 6,width = 8)
  
  #################### 
  # [B] ANALYSIS
  #########################################################
  #### [a] geographica distribution of emissions
  city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(ref_area,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("GDP"="obs_value")
  
  #### [b] geographica distribution of emissions
  emiss_sector_rank <- city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(cou,ref_area,counter_code,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) 
  
    #### [1] sectoral ranking within each area's contribution 
    emiss_sector_rank %>% group_by(cou,ref_area,year) %>% 
    mutate_at(c("obs_value"),~./sum(.,na.rm=TRUE)) %>%
    group_by(cou,ref_area,year) %>% top_n(3,obs_value) %>% View
    
    #### [2] sectoral ranking within all area's contribution 
    emiss_sector_rank %>% group_by(cou,year) %>% 
    mutate_at(c("obs_value"),~./sum(.,na.rm=TRUE)) %>%
    group_by(cou,ref_area,year) %>% top_n(5,obs_value) %>% View
    
  #### [c] geographica distribution of emissions
  emiss_sector_driver <- city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  #mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% 
  left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(cou,code,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) 
  
  emiss_sector_driver %>%  filter(year==2019) %>% group_by(year) %>% top_n(10,obs_value) %>%
  select(cou,code) %>% left_join(emiss_sector_driver %>% pivot_wider(names_from = "year",
  values_from = "obs_value"), by=c("cou","code")) %>% print(n=10)
  
  emiss_sector_supplier <- city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  #mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% 
  left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(cou,counter_code,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE))
  
  emiss_sector_supplier %>%  filter(year==2019) %>% group_by(year) %>% top_n(10,obs_value) %>%
  select(cou,counter_code) %>% left_join(emiss_sector_supplier %>% pivot_wider(names_from = "year",
  values_from = "obs_value"), by=c("cou","counter_code")) %>% print(n=10)
  
  #### [d] ratio of China to United States
  ratio_cn_us <- city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(ref_area,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("GDP"="obs_value") %>%
  filter(ref_area%in%c("China","United States")) %>% 
  pivot_wider(names_from="ref_area",values_from="GDP") %>% 
  mutate(ratio=`China`/`United States`)
  
  city_emissions_gdp %>% filter(cou=="ES30") %>% select(-kgC02_eq) %>%
  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  separate(counter,c("counter_cou","counter_code"),sep="_",extra="merge") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% left_join(code_countries %>% 
  mutate_at(c("ref_area"),~if_else(cou=="CN","China",.)) %>%
  mutate_at(c("ref_area"),~if_else(cou=="US","United States",.)) %>%
  distinct(ref_area,cou),by=c("counter_cou"="cou")) %>% group_by(ref_area,counter_code,year) %>% 
  summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% rename("GDP"="obs_value") %>%
  filter(ref_area%in%c("China","United States")) %>% 
  pivot_wider(names_from="ref_area",values_from="GDP") %>%
  filter(year%in%c(year_str_io,year_end_city)) %>% print(n=100)
  
  #### [d] Import and factor weights by country
  imports <- city_mrio_industry_final %>% mutate(import_demand=rowSums(across(`ES30_A`:`ES30_T`),na.rm=TRUE)) %>%
  select(code,import_demand,year) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>%
  filter(cou%in%c("CN","US","DE")&year%in%c(year_str_io,year_end_city)) %>% pivot_wider(names_from = "year",values_from = "import_demand")
  
  factors <- city_mrio_env_accounts %>% select(code,factor,year) %>% separate(code,c("cou","code"),sep="_",extra="merge") %>%
  filter(cou%in%c("CN","US","DE")&year%in%c(year_str_io,year_end_city)) %>% pivot_wider(names_from = "year",values_from = "factor") %>%
  mutate(`%`=(!!sym(paste0(year_end_city))-!!sym(paste0(year_str_io)))/!!sym(paste0(year_str_io)))
  
  #### [e] emissions industry: G
  city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% filter(cou=="ES30"&code=="G") %>% 
  select(cou,code,obs_value,year) %>% filter(year==year_n) %>% summarize_at(c("obs_value"),
  ~round(sum(.,na.rm=TRUE),0))
  
  #### [f] kgCO2 per unit of GDP
  kgCO2_euro_gdp_direct <- city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% filter(cou=="ES30"&grepl("ES30",counter)) %>% select(cou,code,obs_value,year) %>%
  group_by(cou,code,year) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(city_mrio_industry_final %>% 
  separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(code%in%c(as.vector(unlist(code_iots_city %>% 
  filter(type=="sector") %>% distinct(nace_code))))) %>% select(cou,code,GDP,year) %>% filter(cou=="ES30") %>%
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% group_by(cou,code,year) %>% summarize_at(c("GDP"),~sum(.,na.rm=TRUE)) %>%
  mutate_at(c("year"),~as.character(.)),by=c("cou","code","year")) 
  
  kgCO2_euro_gdp <- city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% filter(cou=="ES30") %>% select(cou,code,obs_value,year) %>% 
  group_by(cou,code,year) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(city_mrio_industry_final %>% 
  separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(code%in%c(as.vector(unlist(code_iots_city %>% 
  filter(type=="sector") %>% distinct(nace_code))))) %>% select(cou,code,GDP,year) %>% filter(cou=="ES30") %>%
  mutate_at(c("code"),~stringr::str_sub(.,1,1)) %>% group_by(cou,code,year) %>% summarize_at(c("GDP"),~sum(.,na.rm=TRUE)) %>%
  mutate_at(c("year"),~as.character(.)),by=c("cou","code","year")) 
  
  kgCO2_euro_gdp_c <- city_emissions_gdp %>%  pivot_longer(`AR_A`:`ZA_T`,names_to = "counter",values_to = "obs_value") %>% 
  filter(cou=="ES30") %>% select(cou,code,obs_value,year) %>% 
  group_by(cou,code,year) %>% summarize_at(c("obs_value"),~sum(.,na.rm=TRUE)) %>% left_join(city_mrio_industry_final %>% 
  separate(code,c("cou","code"),sep="_",extra="merge") %>% filter(code%in%c(as.vector(unlist(code_iots_city %>% 
  filter(type=="sector") %>% distinct(nace_code))))) %>% select(cou,code,GDP,year) %>% filter(cou=="ES30") %>%
  group_by(cou,code,year) %>% summarize_at(c("GDP"),~sum(.,na.rm=TRUE)) %>%
  mutate_at(c("year"),~as.character(.)),by=c("cou","code","year")) %>%
  mutate(code_2d=stringr::str_sub(code,1,1)) %>% filter(code_2d=="C")
  
  mycolors <- rev(colorRampPalette(brewer.pal(5,"RdYlBu"))(18))
  kgCO2_euro_gdp %>% mutate(kgCO2gdp=obs_value/GDP) %>% select(cou:year,kgCO2gdp) %>% ungroup %>% 
  pivot_wider(names_from = "year",values_from = "kgCO2gdp") %>% mutate(`2010_b`=!!sym(paste0(year_str_io))) %>% 
  mutate_at(c(as.character(year_str_io:year_end_city)),~./`2010_b`) %>% select(-`2010_b`) %>%
  mutate(type=if_else(!!sym(paste0(year_end_city))>0.60,"stagnant","progressive")) %>% filter(!code=="T") %>% 
  mutate(growth=(!!sym(paste0(year_end_city))-!!sym(paste0(year_str_io)))/!!sym(paste0(year_str_io))) %>% mutate_at(c("code"),~if_else(code=="B","B_E",.))  %>% 
  pivot_longer(!!sym(paste0(year_str_io)):!!sym(paste0(year_end_city)),names_to = "year",values_to = "kgCO2gdp") %>% 
    ggplot(aes(y = kgCO2gdp, x = year,group=code,color=code)) + 
    geom_line()  + labs(fill='') + ylab("") + scale_fill_manual(values = mycolors) + 
    scale_x_discrete(breaks = seq(2010, 2021, by = 2)) + facet_wrap(~type,ncol=1) + 
    xlab("") + ylab(TeX("kg$\\CO_2$ per unit of GDP (2010=100)")) + 
    guides(color=guide_legend(nrow=2)) + theme(text = element_text(size = 18),axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),legend.position = "bottom",legend.title = element_blank(),
    strip.background = element_blank(),strip.text.x = element_blank())

  ggsave(file.path(getwd(),"paper","graphics",paste0("factores_crecimiento_por_sector_",
  year_n,".pdf")),height = 7,width = 6)
  
##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################