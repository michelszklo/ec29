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

dir <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# ------------------------------------


raw <- paste0(dir,"data/")



# 1. List of municipalities
# =================================================================
mun_list <- read.csv(paste0(raw,"lista_mun/lista_mun_2000.csv"), encoding = "UTF-8") %>% 
  rowwise() %>% 
  mutate(ano = list(seq.int(1998,2015))) %>% 
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
  rename(finbra_reccorr=reccorr) %>% 
  filter(ano<=2010)

finbra <- finbra %>% left_join(finbra_receita, by = c("ano","cod_mun"))
  
rm(finbra_receita)


finbra[8:33] <- lapply(finbra[8:33], function(x) as.numeric(gsub(",","",x),digits = 15))


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
  select(-check)


# 5. Transferências Fundo a Fundo
# =================================================================

fns <- data.frame(read.dta13(paste0(raw,"FNS/faf_2000_2015.dta"))) %>% 
  filter(tp_repasse == "MUNICIPAL") %>% 
  rename(cod_mun = co_municipio_ibge)


fns[20] <- lapply(fns[20], function(x) as.numeric(gsub(",",".",x)))

fns_ab <- fns %>% 
  filter(bloco == "ATENÇÃO BÁSICA") %>% 
  group_by(cod_mun,ano) %>% 
  summarize(transf_faf_ab = sum(vl_liquido, na.rm = T)) %>% 
  ungroup()

fns_pabfixo <- fns %>% 
  filter(bloco == "ATENÇÃO BÁSICA" & componente=="PISO DA ATENÇÃO BÁSICA FIXO - PAB FIXO") %>% 
  group_by(cod_mun,ano) %>% 
  summarize(transf_faf_pabfixo = sum(vl_liquido, na.rm = T)) %>% 
  ungroup() 

fns_pabvar <- fns %>% 
  filter(bloco == "ATENÇÃO BÁSICA" & componente=="PISO DA ATENÇÃO BÁSICA VARIÁVEL") %>% 
  group_by(cod_mun,ano) %>% 
  summarize(transf_faf_pabvar = sum(vl_liquido, na.rm = T)) %>% 
  ungroup() 

fns <- fns %>% 
  group_by(cod_mun,ano) %>% 
  summarize(transf_faf = sum(vl_liquido, na.rm = T)) %>% 
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
sinasc <- read.csv(paste0(raw,"SINASC/SINASC_final.csv"), encoding = "UTF-8")

# sim
sim <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse.dta")))

sim_mm <- read.csv(paste0(raw,"SIM/maternal_mortality.csv"))

pop_40 <- read.csv(paste0(raw,"SIM/pop40.csv")) %>% rename(pop40=pop)

pop_40_96 <- read.csv(paste0(raw,"pop/pop40_1996.csv")) %>% rename(pop40_96 = pop40)

sim_adt <- data.frame(read.dta13(paste0(raw,"SIM/sim_collapse_adult.dta")))

# sia
sia <- read.csv(paste0(raw,"SIA/SIA_final.csv"), encoding = "UTF-8")


ams <- data.frame(read.dta13(paste0(raw,"AMS/ams.dta"))) 

# ADD VARIABLE AFTER MERGES
# %>% 



# 7. Infrastructure data (PSF Romero's files)
# =================================================================

leitos <- data.frame(read.dta13(paste0(raw,"Infra/Leitos.dta"))) %>%
  filter(ano>=1998) 


# 8. Atlas 2013 data (Baseline controls)
# ==============================================================

atlas <- read.csv(paste0(raw,"Atlas2013/atlas_data.csv"), encoding = "UTF-8", sep = ";") %>%
  rename(ano=1) %>%
  filter(ano!=1991) %>% 
  rename(cod_mun = Codmun6)
colnames(atlas) <- tolower(colnames(atlas))
atlas <- atlas %>% 
  select(cod_mun,ano,espvida,mort1,mort5,e_anosestudo,t_analf18m,gini,pind,pmpob,rdpc) %>% 
  mutate(pind = pind/100,
         pmpob = pmpob/100)


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
elect <- read.csv(paste0(raw,"TSE/run_reelection.csv"), encoding = "UTF-8") %>% 
  distinct(cod_mun, .keep_all = T) %>% 
  filter(!is.na(cod_mun)) %>% 
  filter(!(cod_mun==420860 & reelect==1))
  


# 11. Merging all
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
  left_join(infra, by = c("ano","cod_mun")) %>% 
  # datasus - sinasc
  left_join(sinasc, by = c("ano","cod_mun")) %>% 
  # datasus - sim
  left_join(sim, by = c("ano","cod_mun")) %>% 
  left_join(sim_adt, by = c("ano","cod_mun")) %>%
  left_join(sim_mm, by = c("ano","cod_mun")) %>% 
  mutate(mm = ifelse(is.na(mm),0,mm)) %>% 
  left_join(pop_40, by = c("ano","cod_mun")) %>%
  left_join(pop_40_96 %>% select(-ano), by = c("cod_mun")) %>%
  mutate(pop40 = ifelse(ano==1999,dplyr::lead(pop40,1),pop40),
         pop40 = ifelse(ano==1998,dplyr::lead(pop40,2),pop40)) %>% 
  # mutate(share40 = pop40/pop) %>% 
  # mutate(share40 = ifelse(ano==1999,dplyr::lead(share40,1),share40),
  #        share40 = ifelse(ano==1998,dplyr::lead(share40,2),share40)) %>% 
  # mutate(pop40 = ifelse(ano<2000,pop*share40,pop)) %>% 
  # select(-share40) %>% 
  # datasus - sia
  left_join(sia, by = c("ano","cod_mun")) %>%
  # leitos
  left_join(leitos, by = c("ano","cod_mun")) %>% 
  mutate(leitos_pc = leitos/pop*1000,
         hospital = ifelse(leitos>0,1,0)) %>% 
  # atlas
  left_join(atlas, by = c("ano","cod_mun")) %>%
  # censo
  left_join(censo, by = c("ano","cod_mun")) %>%
  # electoral
  left_join(elect, by = c("ano","cod_mun")) %>%
  group_by(cod_mun) %>% 
  mutate(reelect_sample = max(reelect,na.rm = T),
         reelect_sample = ifelse(is.infinite(reelect_sample),0,1)) %>% 
  ungroup() %>% 
  # ams
  left_join(ams, by = c("ano","cod_mun")) %>%
  mutate(hospital_nmun = ifelse(!is.na(hospital_est) & !is.na(hospital_fed),0,NA)) %>%
  mutate(hospital_nmun = hospital_est + hospital_fed) %>% 
  mutate(hr_all = hr_superior + hr_technician + hr_elementary + hr_admin)

  # creating dummies for the presence of hospitals

      dummy_vars <- c("hospital_all",
                      "hospital_all_esp",
                      "hospital_est",
                      "hospital_est_esp",
                      "hospital_fed",
                      "hospital_fed_esp",
                      "hospital_mun",
                      "hospital_mun_esp",
                      "hospital_pvt",
                      "hospital_pvt_esp",
                      "hospital_nmun",
                      "hospital_pub",
                      "hospital_pub_esp")
      
      for (v in dummy_vars){
        newvar <-  paste0("d_",v)
        df <- df %>% 
          mutate(!!newvar :=  ifelse(!!sym(v)>0,1,0))
      }
      
  
      
# 12. Deflating variables
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
  
  


# 13. Creating mortality rates
# ==============================================================


# infant mortality
sim_vars <- grep("^mi",names(df), value = T)
sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(birth_nasc_vivos)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))

# leads
sim_vars_l1 <- sapply(sim_vars_new, function(x) paste0(x,"_l1"), simplify = "array", USE.NAMES = F)
df[sim_vars_l1] <- df[sim_vars_new]
sim_vars_l2 <- sapply(sim_vars_new, function(x) paste0(x,"_l2"), simplify = "array", USE.NAMES = F)
df[sim_vars_l2] <- df[sim_vars_new]
sim_vars_l3 <- sapply(sim_vars_new, function(x) paste0(x,"_l3"), simplify = "array", USE.NAMES = F)
df[sim_vars_l3] <- df[sim_vars_new]
sim_vars_l4 <- sapply(sim_vars_new, function(x) paste0(x,"_l4"), simplify = "array", USE.NAMES = F)
df[sim_vars_l4] <- df[sim_vars_new]
sim_vars_l5 <- sapply(sim_vars_new, function(x) paste0(x,"_l5"), simplify = "array", USE.NAMES = F)
df[sim_vars_l5] <- df[sim_vars_new]

df <- df %>% 
  group_by(cod_mun) %>% 
  mutate_at(sim_vars_l1, function(x) dplyr::lead(x,1)) %>% 
  mutate_at(sim_vars_l2, function(x) dplyr::lead(x,2)) %>% 
  mutate_at(sim_vars_l3, function(x) dplyr::lead(x,3)) %>% 
  mutate_at(sim_vars_l4, function(x) dplyr::lead(x,4)) %>% 
  mutate_at(sim_vars_l5, function(x) dplyr::lead(x,5)) %>% 
  ungroup()


# Maternal Mortality
df <- df %>% 
  mutate(tx_mm = mm/birth_nasc_vivos*1000)



# adult mortality

sim_vars <- grep("^ma",names(df), value = T)
sim_vars_new <- sapply(sim_vars, function(x) paste0("tx_",x),simplify = "array", USE.NAMES = F)
df[sim_vars_new] <- df[sim_vars]

df <- df %>% 
  mutate_at(sim_vars_new, `/`, quote(pop40)) %>% 
  mutate_at(sim_vars_new, `*`, quote(1000))

df[sim_vars_new] <- lapply(df[sim_vars_new], function(x) replace(x,is.infinite(x),0))


# leads
sim_vars_l1 <- sapply(sim_vars_new, function(x) paste0(x,"_l1"), simplify = "array", USE.NAMES = F)
df[sim_vars_l1] <- df[sim_vars_new]
sim_vars_l2 <- sapply(sim_vars_new, function(x) paste0(x,"_l2"), simplify = "array", USE.NAMES = F)
df[sim_vars_l2] <- df[sim_vars_new]
sim_vars_l3 <- sapply(sim_vars_new, function(x) paste0(x,"_l3"), simplify = "array", USE.NAMES = F)
df[sim_vars_l3] <- df[sim_vars_new]
sim_vars_l4 <- sapply(sim_vars_new, function(x) paste0(x,"_l4"), simplify = "array", USE.NAMES = F)
df[sim_vars_l4] <- df[sim_vars_new]
sim_vars_l5 <- sapply(sim_vars_new, function(x) paste0(x,"_l5"), simplify = "array", USE.NAMES = F)
df[sim_vars_l5] <- df[sim_vars_new]

df <- df %>% 
  group_by(cod_mun) %>% 
  mutate_at(sim_vars_l1, function(x) dplyr::lead(x,1)) %>% 
  mutate_at(sim_vars_l2, function(x) dplyr::lead(x,2)) %>% 
  mutate_at(sim_vars_l3, function(x) dplyr::lead(x,3)) %>% 
  mutate_at(sim_vars_l4, function(x) dplyr::lead(x,4)) %>% 
  mutate_at(sim_vars_l5, function(x) dplyr::lead(x,5)) %>% 
  ungroup()


# 14. Creating per capita figurues for specific variables
# ==============================================================

infra_vars <- c("ACS_I", "eSF_I")
sia_vars <- grep("^sia",names(df),value = T)
ams_vars <- c(grep("^hospital_",names(df), value = T),
         grep("^unity_",names(df), value = T),
         grep("^therapy_",names(df), value = T),
         grep("^hr_",names(df), value = T))

vars <- c(infra_vars,sia_vars,ams_vars)
vars_new <- sapply(vars, function(x) paste0(x,"_pcapita"),simplify = "array", USE.NAMES = F)

df[vars_new] <- df[vars]

df <- df %>% 
  mutate_at(vars_new,`/`,quote(pop)) %>% 
  mutate_at(vars_new,`*`,quote(1000))



# 15. Health spending in the neighboring municipalities
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

# 16. FISCAL RESPONSABILITY LAW: municipalities must not spend more than 60% of its current net revenue in personnel
# ==============================================================

df <- df %>% 
  mutate(lrf = ifelse(finbra_desp_pessoal_pcapita/finbra_desp_c_pcapita>0.6,1,0))


# 16. saving
# ==============================================================
df <- df %>% filter(ano<=2015)

saveRDS(df, paste0(raw,"CONSOL_DATA.rds"))



