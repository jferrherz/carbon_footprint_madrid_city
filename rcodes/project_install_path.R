#################### 
# Working directory and packages
#########################################################
################# packages
library(readxl);library(stringr);library(tidyr);
library(magrittr);library(survey);library(srvyr);
library(dplyr);library(tibble);library(readr);
library(readxl);library(ggplot2);library(purrr)
library(openxlsx);library(kableExtra);library(matrixcalc)
################# general path
path_chpter           <- getwd()
database_path         <- file.path(gsub("/03 - UPM - Environmental/environmental-extentions/rcodes","",
                         dirname(rstudioapi::getActiveDocumentContext()$path)),"Databases")
files_path_figaro     <- list.files(file.path(getwd(),"datasets"))
path_classif          <- file.path(database_path,"Classifications")
path_ecv              <- file.path(database_path,"Database_INE_ECV")
path_riot             <- file.path(database_path,"Database_ISM_IO")
path_exiobase         <- file.path(database_path,"EXIOBASE")
path_figaro           <- file.path(database_path,"FIGARO")
path_figaro_reg       <- file.path(database_path,"FIGARO REG")
path_eurostat         <- file.path(database_path,"EUROSTAT")
path_envir            <- file.path(database_path,"Environmental")
path_euklems          <- file.path(database_path,"EUKLEMS")
path_ine_ees          <- file.path(database_path,"Database_INE_EES")
path_figaro_ind_ind   <- file.path(path_figaro,"Annual EU inter-country input-output tables industry by industry")
path_figaro_prod_prod <- file.path(path_figaro,"Annual EU inter-country input-output tables product by product")
path_figaro_supply    <- file.path(path_figaro,"Annual EU inter-country supply tables")
path_figaro_use       <- file.path(path_figaro,"Annual EU inter-country use tables")
path_figaro_pyp       <- file.path(path_figaro,"Supply, use and input-output tables - previous year's prices")
path_figaro_cyp       <- file.path(path_figaro,"Supply, use and input-output tables - current year's prices")
path_figaro_naio      <- file.path(path_figaro,"National Supply, Use and Input-Output tables")
path_figaro_data      <- file.path(getwd(),"datasets")



##################################################################################################################################
## END OF SCRIPT
##################################################################################################################################
