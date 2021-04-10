#######################################################################################################
# Author: Michel Szklo
# April 2020
# 
# This script consolidates data on Brazilian Public Health Budgets and Spending 
# (SIOPS - Sistema de Informações sobre Orçamentos Públicos em Saúde)
# at the municipality level.
# 
# CSVs files where downloaded from the link below and consolidated here into a single dataset
#
# http://siops-asp.datasus.gov.br/CGI/deftohtm.exe?SIOPS/serhist/municipio/mIndicadores.def
#
#
#######################################################################################################

# =================================================================
# 0. Set-up
# =================================================================

rm(list=ls())

# packages
packages<-c('readr',
            'tidyverse',
            'dplyr',
            'RCurl',
            'tidyr',
            'scales',
            'RColorBrewer',
            'plotly',
            'ggplot2',
            'sf',
            'sp',
            'xlsx',
            'httr',
            'mapview',
            'stringdist')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

raw <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/SIOPS/"
raw_uf <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/SIOPS_UF/"
output <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/data/"


# =================================================================
# 1. Importing CSVs files for municipalities
# =================================================================

# list of all csv files to be imported
temp <- list.files(path = raw, pattern = "*.csv")
# importing all csv into a list of dataframes
SIOPS <- lapply(temp,
                function(x)
                  read.csv(paste0(raw,x), 
                           row.names = NULL,
                           header = T, 
                           sep = ";",
                           skip = 3))
# replacing "," with "."
SIOPS <- lapply(SIOPS,
                function(x)
                  as.data.frame(lapply(x, function(y)
                                         gsub(",", ".", y))))
# removing last column of all dataframes (they provide the total of all years, not useful)
SIOPS <- lapply(SIOPS,
                function(x)
                  x[1:(ncol(x)-1)])
# reshaping all dataframes from wide to long
SIOPS <- lapply(SIOPS,
                function(x)
                  pivot_longer(x,
                               cols = all_of(colnames(x)[2:length(colnames(x))]),
                               names_to = "year",
                               values_to = "value"))
# mergin all dataframes into 1 single dataframe
SIOPS <- Reduce(function(x, y) merge(x, y, all = T, by = c("Munic.BR", "year")), SIOPS, accumulate = F)


# =================================================================
# 2. Data management
# =================================================================


# changing columns names
var_names <- c(
  "cod_mun",
  "ano",
  "pct_rliquidatotal",
  "pct_transfintergovliq",
  "pct_transfsaude",
  "pct_transfuniaosaude",
  "pct_transfuniaosus",
  "pct_rimpostosetransfconst",
  "despsaude_pcapita",
  "pct_desppessoal_desptotal",
  "pct_despmedicamentos",
  "pct_servicoster_desptotal",
  "pct_despinvest_desptotal",
  "pct_transfsus_desptotal",
  "pct_recproprios_ec29",
  "desppessoal",
  "desprecpropriosaude_pcapita",
  "desprecproprio",
  "desptotalsaude",
  "pop",
  "rimpostosetransfconst",
  "rtransfuniaosaude_pcapita",
  "rtransfuniaosaude"
)

colnames(SIOPS) <- var_names


# converting to numeric

SIOPS <- lapply(SIOPS, as.character)
SIOPS[3:23] <- lapply(SIOPS[3:23], as.numeric)

SIOPS <- as.data.frame(SIOPS)


# spliting municipality variable and adjusting year variable
SIOPS <- SIOPS %>%
  mutate(cod_mun = as.character(cod_mun), ano = as.character(ano)) %>% 
  mutate(nome_mun = substr(cod_mun, 8, nchar(cod_mun)),
         cod_uf = as.numeric(substr(cod_mun,1,2)),
         cod_mun = substr(cod_mun, 1, 6),
         ano = as.numeric(substr(ano, 2, 5)))

# SIOPS <- SIOPS %>% 
#   mutate(target_ec29 = 15)
# SIOPS$target_ec29[SIOPS$ano==2000] <- 7
# SIOPS$target_ec29[SIOPS$ano==2001] <- 8.6
# SIOPS$target_ec29[SIOPS$ano==2002] <- 10.2
# SIOPS$target_ec29[SIOPS$ano==2003] <- 11.8
# 
# SIOPS <- SIOPS %>% 
#   mutate(ano = as.character(ano)) %>% 
#   mutate(data = as.Date(ano, format=c('%Y'))) %>% 
#   mutate(ano = as.numeric(ano))

SIOPS$pct_recproprios_ec29[SIOPS$pct_recproprios_ec29<0] <- NA
SIOPS$pct_recproprios_ec29[SIOPS$pct_recproprios_ec29>100] <- NA


# generating variable of change in pct_recpróprios

# for (i in seq(1,12,1)){
#   if(i<10){
#     varname1 <- paste0("change0",i,"_pct_recproprios")
#     varname2 <- paste0("var0",i,"_pct_recproprios")
#     SIOPS <- SIOPS %>%
#       group_by(cod_mun) %>%
#       mutate(!!varname1 := dplyr::lead(pct_recproprios_ec29,i) - pct_recproprios_ec29,
#              !!varname2 := (dplyr::lead(pct_recproprios_ec29,i) - pct_recproprios_ec29)/ pct_recproprios_ec29) %>% 
#       # mutate(!!varname1 := pct_recproprios_ec29 - dplyr::lag(pct_recproprios_ec29,i),
#       #        !!varname2 := (pct_recproprios_ec29 - dplyr::lag(pct_recproprios_ec29,i))/dplyr::lag(pct_recproprios_ec29,i)) %>% 
#       ungroup()
#   }else{
#     varname1 <- paste0("change",i,"_pct_recproprios")
#     varname2 <- paste0("var",i,"_pct_recproprios")
#     SIOPS <- SIOPS %>%
#       group_by(cod_mun) %>%
#       mutate(!!varname1 := dplyr::lead(pct_recproprios_ec29,i) - pct_recproprios_ec29,
#              !!varname2 := (dplyr::lead(pct_recproprios_ec29,i) - pct_recproprios_ec29)/ pct_recproprios_ec29) %>% 
#       # mutate(!!varname1 := pct_recproprios_ec29 - dplyr::lag(pct_recproprios_ec29,i),
#       #        !!varname2 := (pct_recproprios_ec29 - dplyr::lag(pct_recproprios_ec29,i))/dplyr::lag(pct_recproprios_ec29,i)) %>% 
#       ungroup()
#   }
# }
# 
# 
# # generating distance to target variables
# 
# SIOPS <- SIOPS %>%
#   mutate(dist_ec29 = - (pct_recproprios_ec29 - 15)/15,
#          achieved_ec29 = ifelse(pct_recproprios_ec29 > target_ec29, 1, 0)) %>% 
#   mutate(dist_ec29_pct_desp = (dist_ec29 * desptotalsaude) / desptotalsaude,
#          dist_ec29_desp = (dist_ec29 * desptotalsaude),
#          dist_ec29_desp_pc = (dist_ec29 * desptotalsaude) / pop,
#          desprecproprio_desptotal_pc = desprecpropriosaude_pcapita / despsaude_pcapita)


# generating main mechanical outcomes variables
# -----------------------------------------------------------------

SIOPS <- SIOPS %>% 
  mutate(desppessoal_pcapita = despsaude_pcapita * pct_desppessoal_desptotal / 100,
         despinvest_pcapita = despsaude_pcapita * pct_despinvest_desptotal / 100,
         despmedicamentos_pcapita = despsaude_pcapita * pct_despmedicamentos / 100,
         despservicoster_pcapita = despsaude_pcapita * pct_servicoster_desptotal / 100,
         despexrecproprio_pcapita = despsaude_pcapita - desprecpropriosaude_pcapita)





# =================================================================
# 3. Importing CSV file for States
# =================================================================
UF <- read.csv(paste0(raw_uf,"Tabela Estados IBGE.csv"),
               row.names = NULL,
               header = T,
               sep = ";",
               encoding = "UTF-8")

colnames(UF) <- c("cod_uf","UF","estado")


SIOPS_UF <- read.csv(paste0(raw_uf,"D.Total Saúde_UF.csv"),
                     row.names = NULL,
                     header = T,
                     sep = ";",
                     skip = 3)


SIOPS_UF <- SIOPS_UF[1:ncol(SIOPS_UF)-1]

SIOPS_UF <- as.data.frame(lapply(SIOPS_UF, function(x) gsub(",", ".", x)))

SIOPS_UF <- pivot_longer(SIOPS_UF,
                         cols = all_of(colnames(SIOPS_UF)[2:length(colnames(SIOPS_UF))]),
                         names_to = "year",
                         values_to = "desptotalsaude_uf")

SIOPS_UF <- SIOPS_UF %>% 
  mutate(ano = substr(year,2,5), desptotalsaude_uf = as.character(desptotalsaude_uf)) %>% 
  mutate(ano = as.numeric(ano), desptotalsaude_uf = as.numeric(desptotalsaude_uf)) %>% 
  select(-year)

colnames(SIOPS_UF)[1] <- "estado"

SIOPS_UF <- left_join(SIOPS_UF, UF, by = "estado")


# =================================================================
# 3. Merging states and municipalities data
# =================================================================

SIOPS <- left_join(SIOPS,SIOPS_UF, by = c("ano","cod_uf"))


# =================================================================
# 4. Saving
# =================================================================

saveRDS(SIOPS, paste0(raw,"SIOPS.rds"))



# =================================================================
# 5. Exporting main variables
# =================================================================

SIOPS_out <- SIOPS %>% 
  select(c("cod_mun", "ano", "cod_uf", "UF", "nome_mun", "pop", "despsaude_pcapita", "desppessoal_pcapita", "despinvest_pcapita", "despmedicamentos_pcapita", "despservicoster_pcapita", "despexrecproprio_pcapita","desprecpropriosaude_pcapita", "pct_recproprios_ec29", "target_ec29","dist_ec29", "desptotalsaude_uf"))

write.table(SIOPS_out, paste0(raw,"SIOPS_select.csv"), fileEncoding = "latin1", sep = ",", row.names = F)



