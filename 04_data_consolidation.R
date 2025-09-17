#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts consolidates the final dataset
#
#
#######################################################################################################

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
            'ggplot2',
            'xlsx',
            'stringdist',
            'textclean',
            'readstata13')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)



# SET PATH FOR EC 29-2000 ON YOUR COMPUTER
# ------------------------------------

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


raw <- paste0(dir,"data/")



# 1. List of municipalities
# =================================================================
mun_list <- read.csv(paste0(raw,"lista_mun/lista_mun_2000.csv"), encoding = "UTF-8") %>% 
  rowwise() %>% 
  mutate(ano = list(seq.int(1996,2015))) %>% 
  ungroup() %>% 
  unnest(ano) %>% 
  rename("cod_mun" = 1)



# 2. Expenditure deflator
# =================================================================
deflator <- read.csv(paste0(raw,"inflacao/igp_di.csv"), encoding = "Latin1", sep = ";") %>% 
  rename(ano = 1,
         deflator_saude = 2)


# 3. Importing FINBRA data
# =================================================================

finbra <- read.csv(paste0(raw,"Finbra/FINBRA.csv"), encoding = "UTF-8") %>% 
  select(ano,cod_mun,nome_mun,cod_uf,uf,pop,pop2000,everything()) %>% 
  select(-c(dep_gambiental,dep_orgagraria))

names(finbra) <- gsub("desp","finbra_desp",names(finbra))

finbra_receita <- read.csv(paste0(raw,"Finbra/FINBRA_receita.csv"), encoding = "UTF-8") %>% 
  rename(finbra_reccorr=reccorr,
         finbra_rectribut = rectribut,
         finbra_rectransf = rectransf,
         finbra_recorc = recorc,
         finbra_impostos_total = impostos_total,
         finbra_iptu = iptu,
         finbra_iss = iss) %>%
  mutate_at(c("finbra_reccorr","finbra_rectribut","finbra_rectransf","finbra_recorc","finbra_iptu","finbra_iss","finbra_impostos_total"), function(x) gsub(",","",x)) %>% 
  mutate_at(c("finbra_reccorr","finbra_rectribut","finbra_rectransf","finbra_recorc","finbra_iptu","finbra_iss","finbra_impostos_total"),as.numeric)

finbra <- finbra %>% left_join(finbra_receita, by = c("ano","cod_mun"))
rm(finbra_receita)


finbra_passivo <- read.csv(paste0(raw,"Finbra/FINBRA_passivo.csv"), encoding = "UTF-8") %>%
  rename(finbra_passivo = passivo,
         finbra_passivo_fin = passivo_fin)


finbra <- finbra %>% left_join(finbra_passivo, by = c("ano","cod_mun"))
rm(finbra_passivo)


finbra[8:33] <- lapply(finbra[8:33], function(x) as.numeric(gsub(",","",x),digits = 15))


# "correcting errors in data

vars_correct <- names(finbra)[!(names(finbra) %in% c("ano","cod_mun","nome_mun","cod_uf","uf","pop","pop2000","finbra_desp_o","finbra_desp_c","finbra_reccorr","finbra_rectribut","finbra_rectransf","finbra_recorc"))]

finbra <- finbra %>% 
  mutate_at(vars_correct, function(x) ifelse(.$finbra_desp_o<x,NA,x))

rm(vars_correct)

finbra <- finbra %>% 
  mutate(finbra_desp_outros_nature = finbra_desp_o - (finbra_desp_pessoal + finbra_desp_investimento),
         finbra_desp_outros_area =  finbra_desp_o - 
           (finbra_desp_saude_san + finbra_desp_transporte + finbra_desp_educ_cultura + finbra_desp_assist_prev),
         finbra_rec_outros = finbra_reccorr - (finbra_rectribut + finbra_rectransf),
         finbra_desp_nao_saude = finbra_desp_o - finbra_desp_saude_san)


# 4. Importing SIOPS data
# =================================================================

siops <- readRDS(paste0(raw,"SIOPS/SIOPS.rds")) %>% 
  select(ano, cod_mun,nome_mun,UF,cod_uf,estado,pop,everything()) %>% 
  mutate(pct_recproprios_ec29 = pct_recproprios_ec29/100,
         cod_mun  = as.numeric(cod_mun)) %>% 
  filter(!is.na(cod_mun))

names(siops)[8:33] <- paste("siops",names(siops)[8:33], sep = "_")
siops <- siops %>%
  group_by(cod_mun,ano) %>% 
  mutate(siops_despoutros_pcapita = sum(siops_desppessoal_pcapita,
                                        siops_despinvest_pcapita,
                                        siops_despservicoster_pcapita,
                                        siops_despmedicamentos_pcapita, 
                                        na.rm = T)) %>% 
  ungroup() %>% 
  mutate(check =ifelse(siops_despoutros_pcapita>siops_despsaude_pcapita*1.01,1,0)) %>%
  mutate(siops_despoutros_pcapita = ifelse(check==1,NA,siops_despoutros_pcapita),
         siops_desppessoal_pcapita = ifelse(check==1,NA,siops_desppessoal_pcapita),
         siops_despinvest_pcapita = ifelse(check==1,NA,siops_despinvest_pcapita),
         siops_despservicoster_pcapita = ifelse(check==1,NA,siops_despservicoster_pcapita),
         siops_despmedicamentos_pcapita = ifelse(check==1,NA,siops_despmedicamentos_pcapita)) %>% 
  mutate(siops_despoutros_pcapita =  siops_despsaude_pcapita - siops_despoutros_pcapita) %>% 
  select(-check)


# 5. Transferências Fundo a Fundo
# =================================================================

fns <- data.frame(read.dta13(paste0(raw,"FNS/faf_2000_2015.dta"))) 
fns <- fns %>% 
  filter(tp_repasse == "MUNICIPAL") %>% 
  rename(cod_mun = co_municipio_ibge)


fns[20] <- lapply(fns[20], function(x) as.numeric(gsub(",",".",x)))

fns_ab <- fns %>% 
  filter(bloco == "ATENÇÃO BÁSICA") %>% 
  group_by(cod_mun,ano) %>% 
  summarise(transf_faf_ab = sum(vl_liquido, na.rm = T)) %>% 
  ungroup()

fns_pabfixo <- fns %>% 
  filter(bloco == "ATENÇÃO BÁSICA" & componente=="PISO DA ATENÇÃO BÁSICA FIXO - PAB FIXO") %>% 
  group_by(cod_mun,ano) %>% 
  summarise(transf_faf_pabfixo = sum(vl_liquido, na.rm = T)) %>% 
  ungroup() 

fns_pabvar <- fns %>% 
  filter(bloco == "ATENÇÃO BÁSICA" & componente=="PISO DA ATENÇÃO BÁSICA VARIÁVEL") %>% 
  group_by(cod_mun,ano) %>% 
  summarise(transf_faf_pabvar = sum(vl_liquido, na.rm = T)) %>% 
  ungroup() 

fns <- fns %>% 
  group_by(cod_mun,ano) %>% 
  summarise(transf_faf = sum(vl_liquido, na.rm = T)) %>% 
  ungroup()

fns <- fns %>% 
  left_join(fns_ab, by = c("cod_mun","ano")) %>% 
  left_join(fns_pabfixo, by = c("cod_mun","ano")) %>% 
  left_join(fns_pabvar, by = c("cod_mun","ano")) 

rm(fns_ab)
rm(fns_pabfixo)
rm(fns_pabvar)

# 6. Merging Datasus Data
# =================================================================

# infra
infra <- data.frame(read.dta13(paste0(raw,"Infra/infra_psf.dta")))

# sinasc (birth - access to health)
sinasc <- read.csv(paste0(raw,"SINASC/SINASC_final.csv"), encoding = "UTF-8") %>% 
  mutate(birth_premature = 1-birth_gest_37plus)

sinasc_f <- read.csv(paste0(raw,"SINASC/birth_fem.csv"), encoding = "Latin-1", sep = ";", fileEncoding = "Windows-1252") %>% 
  mutate(cod_mun = substr(Município,1,6)) %>% 
  select(-c("Município")) %>% 
  mutate(cod_mun = as.numeric(cod_mun)) %>%
  filter(!is.na(cod_mun) & substr(cod_mun,3,6)!="0000")
sinasc_f[,1:13] <- lapply(sinasc_f[,1:13], function(x) as.numeric(gsub("-","0",x)))

sinasc_f <- sinasc_f %>% 
  pivot_longer(cols = colnames(sinasc_f[1:13]),
               names_to = "ano",
               values_to = "birth_f") %>% 
  mutate(ano = as.numeric(substr(ano,2,5)))

sinasc_f[is.na(sinasc_f)] <- 0

sinasc_m <- read.csv(paste0(raw,"SINASC/birth_masc.csv"), encoding = "Latin-1", sep = ";", fileEncoding = "Windows-1252") %>% 
  mutate(cod_mun = substr(Município,1,6)) %>% 
  select(-c("Município")) %>% 
  mutate(cod_mun = as.numeric(cod_mun)) %>%
  filter(!is.na(cod_mun) & substr(cod_mun,3,6)!="0000")
sinasc_m[,1:13] <- lapply(sinasc_m[,1:13], function(x) as.numeric(gsub("-","0",x)))

sinasc_m <- sinasc_m %>% 
  pivot_longer(cols = colnames(sinasc_m[1:13]),
               names_to = "ano",
               values_to = "birth_m") %>% 
  mutate(ano = as.numeric(substr(ano,2,5)))

sinasc_m[is.na(sinasc_m)] <- 0

sinasc <- sinasc %>% 
  left_join(sinasc_f, by = c("ano","cod_mun")) %>% 
  left_join(sinasc_m, by = c("ano","cod_mun")) %>% 
  mutate(birth_sexratio = ifelse(birth_f==0,NA,birth_m/birth_f)) %>% 
  select(-c("birth_f","birth_m"))

rm(sinasc_f,sinasc_m)

# sim
sim <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_infant.dta")))
sim_mc <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_child.dta")))
sim_ma1 <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_20_29.dta")))
sim_ma2 <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_30_39.dta")))
sim_ma3 <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_40_49.dta")))
sim_ma4 <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_50_59.dta")))
sim_ma5 <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_60_69.dta")))
sim_ma6 <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_70_79.dta")))
sim_ma7 <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_80.dta")))

sim_ma <- sim_ma1 %>% 
  full_join(sim_ma2, by = c("ano","cod_mun")) %>% 
  full_join(sim_ma3, by = c("ano","cod_mun")) %>% 
  full_join(sim_ma4, by = c("ano","cod_mun")) %>% 
  full_join(sim_ma5, by = c("ano","cod_mun")) %>% 
  full_join(sim_ma6, by = c("ano","cod_mun")) %>% 
  full_join(sim_ma7, by = c("ano","cod_mun")) 

rm(sim_ma1,sim_ma2,sim_ma3,sim_ma4,sim_ma5,sim_ma6,sim_ma7)

sim_mm <- read.csv(paste0(raw,"SIM/maternal_mortality.csv"))
sim_mf <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_fem1049.dta")))

# population

import_treat_tabnet_pop <- function(csv,object,year_start,year_end,varname,skip){
  
  
  
  df_cols_n <-year_end - year_start + 1 
  
  df <- read.csv(file = csv, encoding = "Latim-1", sep = ";", skip = skip, fileEncoding = "Windows-1252")
  df <- df %>% 
    mutate(cod_mun = substr(Município,1,6)) %>% 
    select(-c("Município")) %>% 
    mutate(cod_mun = as.numeric(cod_mun)) %>%
    filter(!is.na(cod_mun) & substr(cod_mun,3,6)!="0000")
  
  df[,1:df_cols_n] <- lapply(df[,1:df_cols_n], function(x) as.numeric(gsub("-","0",x)))
  
  df <- df %>% 
    pivot_longer(cols = colnames(df[1:df_cols_n]),
                 names_to = "ano",
                 values_to = varname) %>% 
    mutate(ano = as.numeric(substr(ano,2,5)))
  
  df[is.na(df)] <- 0
  
  assign(paste0(object),df,envir = .GlobalEnv)
  
}

# # pop 1996 & 1997
# import_treat_tabnet_pop(paste0(raw,"pop/pop_96_97.csv"),"pop_96_97",1996,1997,"pop_96_97",3)

# pop 1996 - population counting
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12.csv"),"pop",1996,2012,"pop",3)
# pop_96 <- pop_96 %>% 
#   filter(ano==1996)
# import_treat_tabnet_pop(paste0(raw,"pop/pop_97_15_tcu.csv"),"pop",1997,2015,"pop",3)
# 
# pop <- pop_96 %>% 
#   bind_rows(pop) %>% 
#   arrange(cod_mun,ano)


# pop 1-4 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_01_04.csv"),"pop_01_04",1996,2012,"pop_01_04",4)

# pop 5-9 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_05_09.csv"),"pop_05_09",1996,2012,"pop_05_09",4)

# pop 10-19 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_10_19.csv"),"pop_10_19",1996,2012,"pop_10_19",4)

# pop 20-29 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_20_29.csv"),"pop_20_29",1996,2012,"pop_20_29",4)

# pop 30-39 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_30_39.csv"),"pop_30_39",1996,2012,"pop_30_39",4)

# pop 40-49 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_40_49.csv"),"pop_40_49",1996,2012,"pop_40_49",4)

# pop 50-59 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_50_59.csv"),"pop_50_59",1996,2012,"pop_50_59",4)

# pop 60-69 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_60_69.csv"),"pop_60_69",1996,2012,"pop_60_69",4)

# pop 70-79 year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_70_79.csv"),"pop_70_79",1996,2012,"pop_70_79",4)

# pop 80+ year
import_treat_tabnet_pop(paste0(raw,"pop/pop_96_12_80.csv"),"pop_80",1996,2012,"pop_80",4)


# pop women 10-49 years
import_treat_tabnet_pop(paste0(raw,"pop/pop_fem_1049.csv"),"pop_fem_10_49",1998,2010,"pop_fem_10_49",5)


# sia
sia <- read.csv(paste0(raw,"SIA/SIA_final.csv"), encoding = "UTF-8")

# sia microdata
sia_nprod <- read.csv(paste0(raw,"SIA_micro/sia_mun.csv"))
sia_ncnes <- read.csv(paste0(raw,"SIA_micro/sia_ncnes_mun.csv"))


ams <- data.frame(read.dta13(paste0(raw,"AMS/ams.dta")))

names(ams)[3:43] <- paste("ams",names(ams)[3:43], sep = "_")


# ADD VARIABLE AFTER MERGES
# %>% 



# 7. Infrastructure data (PSF Romero's files)
# =================================================================

leitos <- data.frame(read.dta13(paste0(raw,"Infra/Leitos.dta"))) %>%
  filter(ano>=1998) %>% 
  mutate(leitos = ifelse(ano==2004,
                         (dplyr::lead(leitos,1)-dplyr::lag(leitos,1))/2 + dplyr::lag(leitos,1),
                         leitos))


# 8. Atlas 2013 data (Baseline controls)
# ==============================================================

atlas <- read.csv(paste0(raw,"Atlas2013/atlas_data.csv"), encoding = "UTF-8", sep = ";") %>%
  rename(ano=1) %>%
  rename(cod_mun = Codmun6)
colnames(atlas) <- tolower(colnames(atlas))

atlas <- atlas %>% 
  select(cod_mun,ano,espvida,mort1,mort5,e_anosestudo,t_analf18m,gini,pind,pmpob,rdpc) %>% 
  mutate(pind = pind/100,
         pmpob = pmpob/100)

atlas_wide <- atlas %>% 
  pivot_wider(id_cols = cod_mun,
              names_from = ano,
              values_from = c(espvida,mort1,mort5,e_anosestudo,t_analf18m,gini,pind,pmpob,rdpc))

atlas <- atlas %>% 
  filter(ano!=1991)



# 9. Censo 2000-2010
# ==============================================================
censo <- data.frame(read.dta13(paste0(raw,"censo/censo.dta"))) %>% 
  mutate(drop = cod_mun) %>% 
  mutate(drop = substr(as.character(drop),3,6)) %>% 
  filter(drop!="0000") %>% 
  select(-drop) %>% 
  filter(!is.na(cod_mun))


# 10. Electoral Data
# ==============================================================
second_term <- read.csv(paste0(raw,"TSE/second_term.csv"), encoding = "UTF-8") %>% 
  distinct(cod_mun, .keep_all = T)

margin <- read.csv(paste0(raw,"TSE/margin2000.csv"), encoding = "UTF-8") %>% 
  distinct(cod_mun, .keep_all = T)





# 11. SIH - Hospitalization - Maternal and Infant
# ==============================================================

load(paste0(raw,"SIH/sih_98_18.RData")) 


sih <- readRDS(file = paste0(raw,"SIH/sih_res_1996_2010_agegroup.rds")) %>% 
  rename(hi = hi_m,
         hc = hc_m,
         ha_20_29 = ha_20_29_m,
         ha_30_39 = ha_30_39_m,
         ha_40_49 = ha_40_49_m,
         ha_50_59 = ha_50_59_m,
         ha_60_69 = ha_60_69_m,
         ha_70_79 = ha_70_79_m,
         ha_80 = ha_80_m)


# sih <- sih_98_18 %>% 
#   select(mun_code,year,nh_u1y,nh_amen_u1y,nh_icd_pregn_puerp,
#          nh_25to44y,nh_45to54y,nh_55to64y,nh_65to74y,nh_75more,
#          nh_amen_pc_25to44y,nh_amen_pc_45to54y,nh_amen_pc_55to64y,nh_amen_pc_65to74y,nh_amen_pc_75more,
#          nh_notamen_pc_25to44y,nh_notamen_pc_45to54y,nh_notamen_pc_55to64y,nh_notamen_pc_65to74y,nh_notamen_pc_75more) %>% 
#   mutate(sih_adult = nh_25to44y + nh_45to54y + nh_55to64y + nh_65to74y + nh_75more,
#          sih_adult_icsap = nh_amen_pc_25to44y + nh_amen_pc_45to54y + nh_amen_pc_55to64y + nh_amen_pc_65to74y + nh_amen_pc_75more,
#          sih_adult_nicsap = nh_notamen_pc_25to44y + nh_notamen_pc_45to54y + nh_notamen_pc_55to64y + nh_notamen_pc_65to74y + nh_notamen_pc_75more) %>% 
#   rename(cod_mun = mun_code,
#          ano = year,
#          sih_infant = nh_u1y,
#          sih_infant_icsap = nh_amen_u1y,
#          sih_maternal = nh_icd_pregn_puerp) %>% 
#   rename(sih_25_44 = nh_25to44y,
#          sih_25_44_icsap = nh_amen_pc_25to44y,
#          sih_25_44_nicsap = nh_notamen_pc_25to44y,
#          sih_45_54 = nh_45to54y,
#          sih_45_54_icsap = nh_amen_pc_45to54y,
#          sih_45_54_nicsap = nh_notamen_pc_45to54y) %>% 
#   mutate(sih_infant_nicsap = sih_infant - sih_infant_icsap) %>% 
#   filter(ano<=2010) %>% 
#   mutate(cod_mun = as.numeric(cod_mun)) 
# 
# 
# rm(sih_98_18)



# 12. SIAB HR data
# ==============================================================
load(paste0(raw,"SIAB/siab_complete_total.RData"))
siab <- siab_complete_total
rm(siab_complete_total)
siab <- siab %>%
  select(mun_code,year,
         regist_pers,regist_pers_pacs,regist_pers_psf,
         accomp_especif,accomp_especif_pacs,accomp_especif_psf,
         visit_cha,visit_cha_pacs,visit_cha_psf,
         cons_especif,cons_especif_pacs,cons_especif_psf) %>% 
  rename(ano = year,
         cod_mun = mun_code) %>% 
  mutate(cod_mun = as.numeric(cod_mun))

names(siab)[3:14] <- paste("siab",names(siab)[3:14],sep = "_")

siab <- siab %>% 
  mutate(siab_visit_cons = siab_visit_cha + siab_cons_especif,
         siab_visit_cons_pacs = siab_visit_cha_pacs + siab_cons_especif_pacs,
         siab_visit_cons_psf = siab_visit_cha_psf + siab_cons_especif_psf)


# 14. Firjan Fiscal Index
# ==============================================================

firjan <- read.csv(paste0(raw,"firjan_index_build/firjan.csv")) %>% select(-X)


# 15. GDP per capita
# ==============================================================

gdp <- read.csv(file = paste0(raw,"PIB_municipal/pib_mun.csv"), encoding = "UTF-8", sep = ",", skip = 1) %>% 
  select(-X) %>% 
  mutate(adj = (X1999 - X1996)/3) %>% 
  mutate(X1997 = X1996 + adj,
         X1998 = X1996 + 2*adj)
gdp <- gdp %>%
  mutate(cod_mun = as.numeric(substr(as.character(Código),1,6))) %>% 
  pivot_longer(cols = grep("X",names(gdp),value = T),
               names_to = "ano",
               values_to = "gdp_mun") %>% 
  arrange(cod_mun,ano) %>% 
  mutate(ano = as.numeric(substr(ano,2,5))) %>% 
  select(cod_mun,ano,gdp_mun)


# 16. Bolsa Familia per capita
# ==============================================================

pbf <- read.csv(file = paste0(raw,"PBF/pbf.csv"), encoding = "UTF-8", sep = ",") %>% 
  rename(pbf_pcapita = pbf_pc)

# 16. Health system data
# ==============================================================
# private insurance
insurance <- data.frame(read.dta13(paste0(raw,"insurance.dta")))


# hospitalization flows
sih_flow <- read.csv(file = paste0(raw,"SIH/sih_flows.csv")) %>% rename(ano = year)
new_vars <- sapply(names(sih_flow)[3:8], function(x) paste0(x,"_na"),simplify = "array", USE.NAMES = F)
sih_flow[new_vars] <- sih_flow[3:8]

sih_flow <- sih_flow %>% 
  mutate_at(names(sih_flow)[3:8], ~ if_else(is.na(.), 0, .))


# 16. Munic data
# ==============================================================

munic <- read.csv(paste0(raw,"munic/munic2002.csv"), sep = ";")
munic <- munic %>%
  mutate(gov_plan = ifelse(gov_plan=="Sim",1,0),
         digital_health_records = ifelse(digital_health_records=="Sim",1,0)) %>% 
  mutate_at(vars(total_gov_empl,gov_emp_low,gov_emp_mid,gov_emp_high,
                 total_council_empl,council_emp_low,council_emp_mid,council_emp_high),
            as.numeric) %>% 
  mutate(share_gov_emp_low = gov_emp_low/total_gov_empl,
         share_gov_emp_mid = gov_emp_mid/total_gov_empl,
         share_gov_emp_high = gov_emp_high/total_gov_empl,
         share_council_emp_low = council_emp_low/total_council_empl,
         share_council_emp_mid = council_emp_mid/total_council_empl,
         share_council_emp_high = council_emp_high/total_council_empl) %>% 
  select(-gov_emp_low,-gov_emp_mid,-gov_emp_high,-council_emp_low,-council_emp_mid,-council_emp_high)

# 17. IQIM data
# ==============================================================

iqim <- read.csv(paste0(raw,"IQIM/IQIM09.csv"), sep = ";") %>% 
  select(codufmun, IQIM) %>% 
  rename(cod_mun = codufmun,
         iqim09 = IQIM)


# 17. RAIS data
# ==============================================================

rais <- readRDS(paste0(raw,"RAIS/rais_consol.rds"))
rais <- rais %>%
  ungroup() %>% 
  # rename(cod_mun = id_municipio) %>% 
  # mutate(cod_mun = substr(cod_mun,1,6)) %>% 
  # mutate(cod_mun = as.numeric(cod_mun),
  #        ano = as.numeric(ano)) %>% 
  select(-sigla_uf)


# 17. Merging all
# ==============================================================

df <- mun_list %>% 
  # mergin deflator
  left_join(deflator, by = "ano") %>% 
  # merging FINBRA data
  left_join(finbra %>% select(-c(uf,nome_mun,pop2000,pop,cod_uf)), by = c("ano","cod_mun")) %>% 
  # merging SIOPS data
  left_join(siops %>% select(-pop), by = c("ano","cod_mun")) %>%
  # merging Trasnferencias Fundo a Fundo
  left_join(fns, by = c("ano","cod_mun")) %>%
  # datasus - infra
  left_join(infra %>% select(-pop), by = c("ano","cod_mun")) %>% 
  mutate(ACS_popprop = ACS_popprop / 100,
         eSF_popprop = eSF_popprop / 100) %>% 
  # datasus - sinasc
  left_join(sinasc, by = c("ano","cod_mun")) %>% 
  # datasus - sim
  left_join(sim, by = c("ano","cod_mun")) %>% 
  left_join(sim_mm, by = c("ano","cod_mun")) %>% 
  left_join(sim_mc, by = c("ano","cod_mun")) %>%
  mutate(mc2 = mi + mc,
         mc2_ext = mi_ext + mc_ext,
         mc2_resp = mi_resp + mc_resp,
         mc2_infec = mi_infec + mc_infec,
         mc2_illdef = mi_illdef + mc_illdef,
         mc2_cong = mi_cong + mc_cong,
         mc2_out = mi_out + mc_out,
         mc2_icsap = mi_icsap + mc_icsap,
         mc2_nicsap = mi_nicsap + mc_nicsap) %>% 
  left_join(sim_ma, by = c("ano","cod_mun")) %>%
  left_join(sim_mf, by = c("ano","cod_mun")) %>% 
  mutate(mm = ifelse(is.na(mm),0,mm)) %>% 
  left_join(pop, by = c("ano","cod_mun")) %>%
  left_join(pop_01_04, by = c("ano","cod_mun")) %>% 
  mutate(pop_0_04 = birth_nasc_vivos + pop_01_04) %>% 
  left_join(pop_05_09, by = c("ano","cod_mun")) %>% 
  left_join(pop_10_19, by = c("ano","cod_mun")) %>% 
  left_join(pop_20_29, by = c("ano","cod_mun")) %>% 
  left_join(pop_30_39, by = c("ano","cod_mun")) %>% 
  left_join(pop_40_49, by = c("ano","cod_mun")) %>% 
  left_join(pop_50_59, by = c("ano","cod_mun")) %>% 
  left_join(pop_60_69, by = c("ano","cod_mun")) %>% 
  left_join(pop_70_79, by = c("ano","cod_mun")) %>% 
  left_join(pop_80, by = c("ano","cod_mun")) %>% 
  left_join(pop_fem_10_49, by = c("ano","cod_mun")) %>% 
  mutate(birth_fertility = birth_nasc_vivos / pop_fem_10_49) %>% 
  left_join(sia, by = c("ano","cod_mun")) %>%
  left_join(sia_ncnes, by = c("ano","cod_mun")) %>%
  left_join(sia_nprod, by = c("ano","cod_mun")) %>%
  # leitos
  left_join(leitos, by = c("ano","cod_mun")) %>% 
  mutate(leitos_pc = leitos/pop*1000,
         hospital = ifelse(leitos>0,1,0)) %>% 
  # atlas
  left_join(atlas, by = c("ano","cod_mun")) %>%
  left_join(atlas_wide, by = "cod_mun") %>%
  # censo
  left_join(censo, by = c("ano","cod_mun")) %>%
  # electoral
  left_join(second_term, by = c("cod_mun")) %>%
  left_join(margin %>% select(-municipio), by = "cod_mun") %>% 
  # ams
  left_join(ams, by = c("ano","cod_mun")) %>%
  mutate(ams_hospital_nmun = ifelse(!is.na(ams_hospital_est) & !is.na(ams_hospital_fed),0,NA)) %>%
  mutate(ams_hospital_nmun = ams_hospital_est + ams_hospital_fed) %>% 
  mutate(ams_hr_all = ams_hr_superior + ams_hr_technician + ams_hr_elementary + ams_hr_admin) %>% 
  # SIH
  left_join(sih,by = c("ano","cod_mun")) %>% 
  # siab data
  left_join(siab, by = c("ano","cod_mun")) %>% 
  left_join(firjan, by = "cod_mun") %>% 
  left_join(gdp, by = c("ano","cod_mun")) %>% 
  left_join(pbf, by = c("ano","cod_mun")) %>% 
  mutate(pbf_pcapita = ifelse(is.na(pbf_pcapita) & ano<1998,0,pbf_pcapita)) %>% 
  left_join(insurance, by = c("ano","cod_mun")) %>% 
  left_join(sih_flow, by = c("ano","cod_mun")) %>% 
  left_join(munic, by = "cod_mun") %>% 
  left_join(iqim, by = "cod_mun") %>% 
  left_join(rais, by = c("ano","cod_mun"))


# creating dummies for the presence of hospitals

dummy_vars <- c("ams_hospital_all",
                "ams_hospital_all_esp",
                "ams_hospital_est",
                "ams_hospital_est_esp",
                "ams_hospital_fed",
                "ams_hospital_fed_esp",
                "ams_hospital_mun",
                "ams_hospital_mun_esp",
                "ams_hospital_pvt",
                "ams_hospital_pvt_esp",
                "ams_hospital_nmun",
                "ams_hospital_pub",
                "ams_hospital_pub_esp")

for (v in dummy_vars){
  newvar <-  paste0("d_",v)
  df <- df %>% 
    mutate(!!newvar :=  ifelse(!!sym(v)>0,1,0))
}



# 18. Deflating variables
# ==============================================================

exclude_vars <- grep("siops_pct",names(df), invert = T,value = T)
siops_vars <- grep("siops",names(df), value = T)
siops_vars <- siops_vars[siops_vars %in% exclude_vars]


finbra_vars <- grep("finbra",names(df),value = T)
finbra_vars_new <- sapply(finbra_vars, function(x) paste0(x,"_pcapita"), simplify = "array", USE.NAMES = F)
df[finbra_vars_new] <- df[finbra_vars]


fns_vars <- grep("^transf_faf",names(df),value = T)
fns_vars_new <- sapply(fns_vars, function(x) paste0(x,"_pcapita"),simplify = "array", USE.NAMES = F)
df[fns_vars_new] <- df[fns_vars]



df <- df %>% 
  mutate_at(siops_vars, `/`, quote(deflator_saude)) %>%
  mutate_at(finbra_vars_new, `/`, quote(pop)) %>% 
  mutate_at(finbra_vars_new, `/`, quote(deflator_saude)) %>% 
  select(-all_of(finbra_vars)) %>% 
  mutate_at(fns_vars_new, `/`, quote(pop)) %>% 
  mutate_at(fns_vars_new, `/`, quote(deflator_saude)) %>% 
  select(-all_of(fns_vars))




# 19. Creating mortality rates
# ==============================================================


# mortality (infant,maternal,child adult, adult 1, adult 2, elderly)
sim_vars <- c(grep("^mi",names(df), value = T),
              grep("^mm",names(df), value = T),
              grep("^mc",names(df), value = T),
              grep("^ma",names(df), value = T),
              grep("^me",names(df), value = T),
              grep("^mf",names(df), value = T))



# transforming NA mi into 0
df <- df %>% 
  mutate_at(sim_vars, ~ if_else(is.na(.), 0, .))



# infant and maternal mortality
sim_vars <- c(grep("^mi",names(df), value = T),
              grep("^mm",names(df), value = T))

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(birth_nasc_vivos)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# Child mortality (1-4)
sim_vars <- c(grep("^mc$",names(df), value = T),
              grep("^mc_",names(df), value = T))


sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_01_04)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# Child mortality (0-4)
sim_vars <- (grep("^mc2",names(df), value = T))


sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_0_04)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))



# Adult Mortality (20-29)
sim_vars <- grep("^ma_20_29",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_20_29)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))

# Adult Mortality (30-39)
sim_vars <- grep("^ma_30_39",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_30_39)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))

# Adult Mortality (40-49)
sim_vars <- grep("^ma_40_49",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_40_49)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Mortality (50-59)
sim_vars <- grep("^ma_50_59",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_50_59)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Mortality (60-69)
sim_vars <- grep("^ma_60_69",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_60_69)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Mortality (70-79)
sim_vars <- grep("^ma_60_69",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_60_69)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Mortality (80)
sim_vars <- grep("^ma_80",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_80)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# Female Mortality 1 (10-49)
sim_vars <- grep("^mf",names(df), value = T)

sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop_fem_10_49)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))




# 20. Creating hospitalization rates
# ==============================================================


# hospitalization (infant,child adult, adult 1, adult 2, elderly)
sih_vars <- c(grep("^hi",names(df), value = T),
              grep("^hc",names(df), value = T),
              grep("^ha",names(df), value = T))



# transforming NA mi into 0
df <- df %>% 
  mutate_at(sih_vars, ~ if_else(is.na(.), 0, .))



# infant hospitalization
sih_vars <- c(grep("^hi$",names(df), value = T),
              grep("^hi_",names(df), value = T))

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(birth_nasc_vivos)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))


# Child hospitalization (1-4)
sih_vars <- c(grep("^hc$",names(df), value = T),
              grep("^hc_",names(df), value = T))


sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_01_04)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Hospitalization (20-29)
sih_vars <- grep("^ha_20_29",names(df), value = T)

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_20_29)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))

# Adult Hospitalization (30-39)
sih_vars <- grep("^ha_30_39",names(df), value = T)

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_30_39)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))

# Adult Hospitalization (40-49)
sih_vars <- grep("^ha_40_49",names(df), value = T)

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_40_49)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Hospitalization (50-59)
sih_vars <- grep("^ha_50_59",names(df), value = T)

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_50_59)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Hospitalization (60-69)
sih_vars <- grep("^ha_60_69",names(df), value = T)

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_60_69)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Hospitalization (70-79)
sih_vars <- grep("^ha_60_69",names(df), value = T)

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_70_79)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))


# Adult Hospitalization (80)
sih_vars <- grep("^ha_80",names(df), value = T)

sih_vars_new <- sapply(sih_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sih_vars_new] <- df[sih_vars]

df <- df %>% 
  mutate_at(sih_vars_new, `/`, quote(pop_80)) %>% 
  mutate_at(sih_vars_new, `*`, quote(1000))

df[sih_vars_new] <- lapply(df[sih_vars_new], function(x) replace(x,is.infinite(x),0))



# 20. Creating per capita figurues for specific variables
# ==============================================================

# vars
infra_vars <- c("ACS_I", "eSF_I")
sia_vars <- c("sia","sia_ab",grep("^sia_nprod",names(df),value = T))
ams_vars <- grep("^ams_",names(df), value = T)
siab_vars <- grep("siab",names(df),value = T)


# SIA 0 adj: transforming NA into 0
df <- df %>% 
  mutate_at(sia_vars, ~ if_else(is.na(.), 0, .))


# per capita figures
vars <- c(infra_vars,sia_vars,siab_vars,"gdp_mun")
vars_new <- sapply(vars, function(x) paste0(x,"_pcapita"),simplify = "array", USE.NAMES = F)

df[vars_new] <- df[vars]

df <- df %>% 
  mutate_at(vars_new,`/`,quote(pop))

# per capita * 1000 inhabitants figures
# ams_cnes_vars <- grep("ams_cnes",names(df),value = T)
sia_ncnes_vars <- grep("sia_ncnes",names(df),value = T)

vars <- c(sia_ncnes_vars,ams_vars)
vars_new <- sapply(vars, function(x) paste0(x,"_pcapita"),simplify = "array", USE.NAMES = F)
df[vars_new] <- df[vars]
df <- df %>% 
  mutate_at(vars_new,`/`,quote(pop)) %>% 
  mutate_at(vars_new,`*`,1000) 

# 19. Health spending in the neighboring municipalities
# ==============================================================

mun_neighbors <- readRDS(paste0(raw,"mun_neighbors.RDS"))

df_neighbor <- df %>%
  select(ano,cod_mun) %>%
  full_join(mun_neighbors, by = "cod_mun") %>% 
  left_join(df %>% 
              select(ano,cod_mun,siops_desptotalsaude,siops_despsaude_pcapita,finbra_desp_saude_san_pcapita) %>% 
              rename(cod_mun_neighbor = cod_mun),
            by = c("cod_mun_neighbor","ano")) %>% 
  filter(cod_mun!=cod_mun_neighbor) %>% 
  group_by(ano,cod_mun) %>% 
  summarise(siops_desptotalsaude_neighbor = mean(siops_desptotalsaude, na.rm = T),
            siops_despsaude_pcapita_neighbor = mean(siops_despsaude_pcapita, na.rm = T),
            finbra_desp_saude_san_pcapita_neighbor = mean(finbra_desp_saude_san_pcapita, na.rm = T))

df <- df %>% 
  left_join(df_neighbor, by = c("ano","cod_mun"))

# 20. FISCAL RESPONSABILITY LAW: municipalities must not spend more than 60% of its current net revenue in personnel
# ==============================================================

df <- df %>% 
  mutate(lrf = finbra_desp_pessoal_pcapita / finbra_reccorr_pcapita,
         lrf_dummy = ifelse(lrf<0.6,1,0))


# 21. Changes in SIOPS spending 2000 - 2005
# ==============================================================

df <- df %>% 
  mutate(change_05_finbra_desp_saude_san_pcapita = dplyr::lead(finbra_desp_saude_san_pcapita,5) - finbra_desp_saude_san_pcapita,
         change_05_siops_despsaude_pcapita = dplyr::lead(siops_despsaude_pcapita,5) - siops_despsaude_pcapita)

# 22. Share of spending to total spending (finbra)
# ==============================================================

df <- df %>% 
  mutate(finbra_desp_pessoal_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_pessoal_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_investimento_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_investimento_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_outros_nature_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_outros_nature_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_adm_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_adm_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_saude_san_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_saude_san_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_transporte_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_transporte_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_educ_cultura_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_educ_cultura_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_hab_urb_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_hab_urb_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_assist_prev_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_assist_prev_pcapita / finbra_desp_o_pcapita,0),
         finbra_desp_outros_area_share = ifelse(finbra_desp_o_pcapita!=0,finbra_desp_outros_area_pcapita / finbra_desp_o_pcapita,0),
         finbra_rectransf_share = ifelse(finbra_recorc_pcapita!=0,finbra_rectransf_pcapita / finbra_recorc_pcapita,0),
         finbra_rectribut_share = ifelse(finbra_recorc_pcapita!=0,finbra_rectribut_pcapita / finbra_recorc_pcapita,0),
         finbra_rec_outros_share = ifelse(finbra_recorc_pcapita!=0,finbra_rec_outros_pcapita / finbra_recorc_pcapita,0),
         siops_desprecpropriosaude_share = ifelse(siops_despsaude_pcapita!=0,siops_desprecpropriosaude_pcapita / siops_despsaude_pcapita,0),
         siops_despexrecproprio_share = ifelse(siops_despsaude_pcapita!=0,siops_despexrecproprio_pcapita / siops_despsaude_pcapita,0),
         siops_desppessoal_share = ifelse(siops_despsaude_pcapita!=0,siops_desppessoal_pcapita / siops_despsaude_pcapita,0),
         siops_despinvest_share = ifelse(siops_despsaude_pcapita!=0,siops_despinvest_pcapita / siops_despsaude_pcapita,0),
         siops_despservicoster_share = ifelse(siops_despsaude_pcapita!=0,siops_despservicoster_pcapita / siops_despsaude_pcapita,0),
         siops_despoutros_share = ifelse(siops_despsaude_pcapita!=0,siops_despoutros_pcapita / siops_despsaude_pcapita,0))
# checking if values sum 1
# mutate(sum_check = finbra_desp_saude_san_share + finbra_desp_transporte_share + finbra_desp_educ_cultura_share +
#          finbra_desp_hab_urb_share + finbra_desp_assist_prev_share,
#        sum_check2 = finbra_rectransf_share + finbra_rectribut_share) %>% 
# mutate(finbra_desp_saude_san_share = ifelse(sum_check>1,NA,finbra_desp_saude_san_share),
#        finbra_desp_saude_san_pcapita = ifelse(sum_check>1,NA,finbra_desp_saude_san_pcapita),
#        finbra_desp_transporte_share = ifelse(sum_check>1,NA,finbra_desp_transporte_share),
#        finbra_desp_transporte_pcapita = ifelse(sum_check>1,NA,finbra_desp_transporte_pcapita),
#        finbra_desp_educ_cultura_share = ifelse(sum_check>1,NA,finbra_desp_educ_cultura_share),
#        finbra_desp_educ_cultura_pcapita = ifelse(sum_check>1,NA,finbra_desp_educ_cultura_pcapita),
#        finbra_desp_hab_urb_share = ifelse(sum_check>1,NA,finbra_desp_hab_urb_share),
#        finbra_desp_hab_urb_pcapita = ifelse(sum_check>1,NA,finbra_desp_hab_urb_pcapita),
#        finbra_desp_assist_prev_share = ifelse(sum_check>1,NA,finbra_desp_assist_prev_share),
#        finbra_desp_assist_prev_pcapita = ifelse(sum_check>1,NA,finbra_desp_assist_prev_pcapita),
#        finbra_desp_outros_nature_pcapita = ifelse(sum_check>1,NA,finbra_desp_outros_nature_pcapita),
#        finbra_desp_outros_nature_share = ifelse(sum_check>1,NA,finbra_desp_outros_nature_share),
#        finbra_desp_outros_area_pcapita = ifelse(sum_check>1,NA,finbra_desp_outros_area_pcapita),
#        finbra_desp_outros_area_share = ifelse(sum_check>1,NA,finbra_desp_outros_area_share),
#        finbra_reccorr_pcapita = ifelse(sum_check2>1,NA,finbra_reccorr_pcapita),
#        finbra_rectransf_pcapita = ifelse(sum_check2>1,NA,finbra_rectransf_pcapita),
#        finbra_rectransf_share = ifelse(sum_check2>1,NA,finbra_rectransf_share),
#        finbra_rectribut_pcapita = ifelse(sum_check2>1,NA,finbra_rectribut_pcapita),
#        finbra_rectribut_share = ifelse(sum_check2>1,NA,finbra_rectribut_share),
#        finbra_rec_outros_pcapita = ifelse(sum_check2>1,NA,finbra_rec_outros_pcapita),
#        finbra_rec_outros_share = ifelse(sum_check2>1,NA,finbra_rec_outros_share)
# ) %>% 
# select(-c("sum_check","sum_check2"))


# 23. Finbra: splitting non health spending into social and non social
# ==============================================================
df <- df %>% 
  mutate(finbra_despsocial_pcapita = finbra_desp_nao_saude_pcapita - finbra_desp_outros_area_pcapita)


# 24. saving
# ==============================================================
df <- df %>% filter(ano<=2015)
saveRDS(df, paste0(raw,"processed/CONSOL_DATA.rds"))

