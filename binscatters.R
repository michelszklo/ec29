#######################################################################################################
# Author: Michel Szklo
# April 2025
# 
# This script creates time scatter plots for "% of own resource spending in base line vs 
# change in variables between 1998-2000, 2000-2005 and 2000-2010 
# 
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
            'xlsx',
            'httr',
            'stringdist',
            'gridExtra',
            'binsreg',
            'lfe')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)

dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"

# --------------------------------------------------------------------------------------------------
output <- "C:/Users/mszklo/Documents/GitHub/ec29/outputs/new_scatters/"
raw <- paste0(dir,"data/")


load(paste0(dir,"regs.RData"))
# --------------------------------------------------------------------------------------------------

# 1. Census Data
# =================================================================


census <- read.csv(paste0(raw,"Atlas2013/atlas_data.csv"), encoding = "UTF-8", sep = ";") %>%
  rename(ano=1) %>%
  # filter(ano!=2010) %>% 
  rename(cod_mun = Codmun6)
colnames(census) <- tolower(colnames(census))
census <- census %>% 
  select(cod_mun,ano,e_anosestudo,t_analf18m,gini,pind,pmpob,rdpc,t_agua,t_lixo,t_luz,agua_esgoto,idhm) %>% 
  mutate(pind = pind/100,
         pmpob = pmpob/100)



# 2. Residuals of the main specification
# =================================================================

# samples
# -----------------------------------------------------------------

# for imr variables
df_imr <- df

# for spending variables
df_exp <- df

# dropping municipalities with outliers in spending
outliers <- df_exp %>% 
  mutate(s = log(finbra_desp_o_pcapita)) %>% 
  select(s,everything())

ndesv <- 5
x <- mean(outliers$s, na.rm = T)
sd <- sd(outliers$s, na.rm = T)
outliers <- outliers %>% 
  mutate(s1 = x - sd * ndesv,
         s2 = x + sd * ndesv) %>% 
  filter(s<=s1 | s>=s2) %>% 
  select(cod_mun) %>% 
  unique()

outliers <- outliers$cod_mun

df_exp <- df_exp %>% 
  filter(!(cod_mun %in% outliers))



# specifications
# -----------------------------------------------------------------

# spending outcomes
spec1 <- paste(" ~ ",0," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2 <- paste(" ~ ",0," + ", paste(baseline_controls, collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3 <- paste(" ~ ",0," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4 <- paste(" ~ ",0," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")

# imr outcomes
spec1_imr <- paste(" ~ ",0," + ",imr_controls," | cod_mun + uf_y_fe | 0 | cod_mun")
spec2_imr <- paste(" ~ ",0," + ", paste(c(baseline_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec3_imr <- paste(" ~ ",0," + ", paste(c(baseline_controls,tvarying_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec4_imr <- paste(" ~ ",0," + ", paste(c(baseline_controls,tvarying_controls,fiscal_controls,imr_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")
spec5_imr <- paste(" ~ ",0," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")



# function
# -----------------------------------------------------------------

residuals <- function(df,var,imr,spec){
  
  df_reg <- df %>% 
    filter(ano>=1998)
  

  if(imr==1){
    df_reg <- df_reg[complete.cases(df_reg[,imr_controls]),]
  }
  
  
  if(imr==1){
    spec_reduced<- get(paste0("spec",spec,"_imr"))
  } else{
    spec_reduced<- get(paste0("spec",spec))
  }
  
  weight_vector <- df_reg["peso_pop"] %>% unlist() %>% as.numeric()
  
  regformula <- as.formula(paste(var,spec_reduced))
  fit <- felm(regformula, data = df_reg,weights = weight_vector,exactDOF = T)
  
  
  resid <- fit$r.residuals %>%
    as.data.frame() %>% 
    rename_with(~paste0(var, spec), 1)
  
  
  df_reg <- df_reg %>% 
    cbind(resid)
  
  
  selected <- names(df_reg)[ncol(df_reg)]
  df_reg <- df_reg %>% 
    select(ano,cod_mun,all_of(selected))
  
  df <- df %>% 
    left_join(df_reg, by = c("cod_mun","ano"))
  
  return(df)
  
}



# 3. Generic functions for creating binscatters for IM and spending data
# =================================================================


# Mains Binscatter function
#------------------------------

# the function implements binscatters for 3 shifts (1998-2000, 2000-2005, 2000-2010)
# options to select regression specification, default number of bins or forced bins for each 1%

bs <- function(df,var,bins_w,label,folder,spec,imr,default,regline){
  
  
  
  # filter years
  df_reg <- df %>% 
    filter(ano>=1998)
  
  
  # guaranteeing balanced sample across specifications
  df_reg <- df_reg[complete.cases(df_reg[,"dist_ec29_baseline"]),]
  df_reg <- df_reg[complete.cases(df_reg[,var]),]
  df_reg <- df_reg[complete.cases(df_reg[,tvarying_controls]),]
  df_reg <- df_reg[complete.cases(df_reg[,fiscal_controls]),]
  
  
  # adding residuals if needed, according to spec (1-4 specifications of the paper)
  if(spec>0){
    
    df_reg <- df_reg %>% 
      residuals(var=all_of(var),imr=imr,spec=spec)
    
    var <- paste0(var,spec)
    
  } else {
    df_reg <- df_reg
    var <- var
  }
  
  # estimating shifts
  df_shift <- df_reg %>% 
    group_by(cod_mun) %>% 
    mutate(across(all_of(var), ~ lead(.x, 5) - .x, .names = "shift05_{.col}")) %>% 
    mutate(across(all_of(var), ~ lead(.x, 10) - .x, .names = "shift10_{.col}")) %>% 
    mutate(across(all_of(var), ~ .x - lag(.x, 2), .names = "shift98_{.col}")) %>% 
    ungroup() %>% 
    filter(ano==2000)
  
  # triming for ploting
  df_plot <- df_shift %>% 
    filter(dist_ec29_baseline>-0.2)
  
  
  # looping across shifts binscatter estimation and plots
  for(shift in c("shift05_","shift10_","shift98_")){
    
    siops <- grep("siops",var)
    
    if(length(siops) > 0 & shift=="shift98_"){
      next
    }
    
    var2 <- paste0(shift,var)
    
    
    if(shift=="shift05_"){
      years <- "2000 - 2005"
    } else if(shift=="shift10_"){
      years <- "2000 - 2010"
    } else {
      years <- "1998 - 2000"
    }
    
    label2 <- paste0(label," \n ",years)
    
    
    if(default==1){
      
      if(bins_w==1){
        est <- binsreg(df_plot[,var2] %>% unlist(),
                       df_plot$dist_ec29_baseline,
                       data = df_plot, 
                       weights = df_plot$peso_pop,
                       randcut = 1,
                       line = c(3,3),
                       ci = c(3,3),
                       cb = c(3,3))
      } else {
        
        est <- binsreg(df_plot[,var2] %>% unlist(),
                       df_plot$dist_ec29_baseline,
                       data = df_plot,
                       randcut = 1,
                       line = c(3,3),
                       ci = c(3,3),
                       cb = c(3,3))
      }
      

    } else {
      
      if(bins_w==1){
        
        est <- binsreg(df_plot[,var2] %>% unlist(),
                       df_plot$dist_ec29_baseline,
                       data = df_plot, 
                       weights = df_plot$peso_pop,
                       randcut = 1,
                       line = c(3,3),
                       ci = c(3,3),
                       cb = c(3,3),
                       nbins = 35,
                       binspos = "es")
        
      } else {
        
        est <- binsreg(df_plot[,var2] %>% unlist(),
                       df_plot$dist_ec29_baseline,
                       data = df_plot,
                       randcut = 1,
                       line = c(3,3),
                       ci = c(3,3),
                       cb = c(3,3),
                       nbins = 35,
                       binspos = "es")
      }
      
      
    }
    
    
    est$bins_plot
    
    
    bins_ci <- est$data.plot$`Group Full Sample`$data.ci
    bins_dots <- est$data.plot$`Group Full Sample`$data.dots
    bins_cb <- est$data.plot$`Group Full Sample`$data.cb
    bins_line <- est$data.plot$`Group Full Sample`$data.line
    
    bins_dots <- bins_dots %>% cbind(bins_ci %>% select(ci.l,ci.r)) %>% 
      mutate(below = 0) %>% 
      mutate(below = ifelse(x>0,1,below))
    bins_line <- bins_line %>% cbind(bins_cb %>% select(cb.l,cb.r))
    
    
    
    
    scatter <- ggplot(bins_line,
                      aes(x = x, y = fit)) +
      geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
      geom_line(color = "sienna2", alpha = 1) +
      geom_ribbon(aes(ymin = cb.l, ymax = cb.r),alpha = 0.3, fill = "sienna2") +
      geom_pointrange(size = 0.1, alpha = 0.7,color = "steelblue4",data = bins_dots,aes(x = x, y = fit, ymin = ci.l, ymax = ci.r)) +
      geom_smooth(color = "steelblue4", size = 0.6, data = bins_dots %>% filter(below==0), aes(x = x, y = fit), method = regline,se = FALSE)+
      geom_smooth(color = "steelblue4", size = 0.6, data = bins_dots %>% filter(below==1), aes(x = x, y = fit), method = regline,se = FALSE)+
      scale_x_continuous(limits = c(-0.2,0.15),breaks = seq(-0.2,0.15,0.05)) +
      labs(y = label2,
           x = "Distance to the EC29 target") +
      theme_light() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.title = element_text(size=12),
            axis.text = element_text(size = 13),
            legend.position="bottom", legend.box = "horizontal",
            legend.title = element_blank())
    
    filePDF <- paste0(output,folder,"binscatter_",var2,".pdf")
    
    ggsave(filePDF,
           plot = scatter,
           device = "pdf",
           width = 7, height = 6,
           units = "in")
    
    
    
    
    
  }
  
  
  
  
}


# Simpler Binscatter plot function
#---------------------------------- 

# for census variables and baseline binscatters

bs2 <- function(df,var,bins_w,label,folder,file){
  
  df_plot <- df[complete.cases(df[,var]),]
  df_plot <- df_plot %>% 
    filter(dist_ec29_baseline>-0.2)
  
  if(bins_w==1){
    est <- binsreg(df_plot[,var] %>% unlist(),
                   df_plot$dist_ec29_baseline,
                   data = df_plot, 
                   weights = df_plot$peso_pop,
                   randcut = 1,
                   line = c(3,3),
                   ci = c(3,3),
                   cb = c(3,3),
                   nbins = 35,
                   binspos = "es")
  } else {
    
    est <- binsreg(df_plot[,var] %>% unlist(),
                   df_plot$dist_ec29_baseline,
                   data = df_plot,
                   randcut = 1,
                   line = c(3,3),
                   ci = c(3,3),
                   cb = c(3,3),
                   nbins = 35,
                   binspos = "es")
    
  }
  
  
  est$bins_plot
  
  
  bins_ci <- est$data.plot$`Group Full Sample`$data.ci
  bins_dots <- est$data.plot$`Group Full Sample`$data.dots
  bins_cb <- est$data.plot$`Group Full Sample`$data.cb
  bins_line <- est$data.plot$`Group Full Sample`$data.line
  
  bins_dots <- bins_dots %>% cbind(bins_ci %>% select(ci.l,ci.r)) %>% 
    mutate(below = 0) %>% 
    mutate(below = ifelse(x>0,1,below))
  bins_line <- bins_line %>% cbind(bins_cb %>% select(cb.l,cb.r))
  
  
  
  
  scatter <- ggplot(bins_line,
                    aes(x = x, y = fit)) +
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 0, color = "#9e9d9d", size = 0.7, alpha = 1, linetype = "dotted") +
    geom_line(color = "sienna2", alpha = 1) +
    geom_ribbon(aes(ymin = cb.l, ymax = cb.r),alpha = 0.3, fill = "sienna2") +
    geom_pointrange(size = 0.1, alpha = 0.7,color = "steelblue4",data = bins_dots,aes(x = x, y = fit, ymin = ci.l, ymax = ci.r)) +
    geom_smooth(color = "steelblue4", size = 0.6, data = bins_dots %>% filter(below==0), aes(x = x, y = fit), method = "lm",se = FALSE)+
    geom_smooth(color = "steelblue4", size = 0.6, data = bins_dots %>% filter(below==1), aes(x = x, y = fit), method = "lm",se = FALSE)+
    scale_x_continuous(limits = c(-0.2,0.15),breaks = seq(-0.2,0.15,0.05)) +
    labs(y = label,
         x = "Distance to the EC29 target") +
    theme_light() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=12),
          axis.text = element_text(size = 13),
          legend.position="bottom", legend.box = "horizontal",
          legend.title = element_blank())
  
  filePDF <- paste0(output,folder,"/binscatter_",file,".pdf")
  
  ggsave(filePDF,
         plot = scatter,
         device = "pdf",
         width = 7, height = 6,
         units = "in")
  
  
}



# 4. Infant Mortality Binscatters
# =================================================================


variables <- cbind(c("tx_mi","tx_mi_icsap","tx_mi_nicsap"),
                   c("Shifts in Infant Mortality Rate",
                     "Shifts in Infant Mortality Rate - Amenable to Primary Care",
                     "Shifts in Infant Mortality Rate - Non-Amenable to Primary Care"))


# raw
for(i in 1:nrow(variables)){
  bs(df = df_imr,
     var = variables[i,1],
     bins_w = 1,
     label = variables[i,2],
     folder = "raw/",
     spec = 0,
     imr = 1,
     default = 0,
     regline = "lm")
  print(paste0(variables[i,2]," estimated"))
}


# looping spec 1 to 4

for(j in seq.int(1,4)){
  
  f <- paste0("spec",j,"/")
  
  for(i in 1:nrow(variables)){
    bs(df = df_imr,
       var = variables[i,1],
       bins_w = 1,
       label = variables[i,2],
       folder = f,
       spec = j,
       imr = 1,
       default = 0,
       regline = "lm")
    print(paste0(variables[i,2]," estimated"))
  }
  
}


# robustness
# ------------------------

bs(df = df_imr,
   var = "tx_mi",
   bins_w = 1,
   label = "Shifts in Infant Mortality Rate",
   folder = "robust/fit_loess_",
   spec = 3,
   imr = 1,
   default = 0,
   regline = "loess")


bs(df = df_imr,
   var = "tx_mi",
   bins_w = 1,
   label = "Shifts in Infant Mortality Rate",
   folder = "robust/fit_gam_",
   spec = 3,
   imr = 1,
   default = 0,
   regline = "gam")


bs(df = df_imr,
   var = "tx_mi",
   bins_w = 1,
   label = "Shifts in Infant Mortality Rate",
   folder = "robust/bins_",
   spec = 3,
   imr = 1,
   default = 1,
   regline = "lm")

bs(df = df_imr,
   var = "tx_mi",
   bins_w = 1,
   label = "Shifts in Infant Mortality Rate",
   folder = "robust/bins_fit_loess_",
   spec = 3,
   imr = 1,
   default = 1,
   regline = "loess")

bs(df = df_imr,
   var = "tx_mi",
   bins_w = 1,
   label = "Shifts in Infant Mortality Rate",
   folder = "robust/bins_fit_gam_",
   spec = 3,
   imr = 1,
   default = 1,
   regline = "gam")




# 4. Spending Binscatters
# =================================================================

variables <- cbind(c("finbra_desp_saude_san_pcapita","siops_despsaude_pcapita","siops_pct_recproprios_ec29"),
                   c("Shifts in Health and Sanitation Spending per capita",
                     "Shifts in Health Spending per capita",
                     "Shifts (p.p.) in % of Own Resource Spent on Health"))


# raw
for(i in 1:nrow(variables)){
  bs(df = df_exp,
     var = variables[i,1],
     bins_w = 1,
     label = variables[i,2],
     folder = "raw/",
     spec = 0,
     imr = 1,
     default = 0,
     regline = "lm")
  print(paste0(variables[i,2]," estimated"))
}


# looping spec 1 to 4

for(j in seq.int(1,4)){
  
  f <- paste0("spec",j,"/")
  
  for(i in 1:nrow(variables)){
    bs(df = df_exp,
       var = variables[i,1],
       bins_w = 1,
       label = variables[i,2],
       folder = f,
       spec = j,
       imr = 1,
       default = 0,
       regline = "lm")
    print(paste0(variables[i,2]," estimated"))
  }
  
}



# 5. Census var binscatters
# =================================================================

# census data
#------------------------------

census_var <- names(census)[3:ncol(census)]

census_shift <- census %>%
  group_by(cod_mun) %>% 
  mutate(across(all_of(census_var), ~ .x - lag(.x,1), .names = "shift91_{.col}")) %>% 
  mutate(across(all_of(census_var), ~ lead(.x,1) - .x, .names = "shift10_{.col}")) %>% 
  ungroup() %>% 
  filter(ano==2000) %>% 
  left_join(df %>% select(cod_mun,dist_ec29_baseline, ano) %>% filter(ano==2000),by = "cod_mun")


# Census Shifts 1991-2000
#------------------------------

variables <- cbind(grep("shift91",names(census_shift), value = T),
                   c("Shifts in Expected Years of Study  \n 1991-2000",
                     "Shifts in Iliteracy Rate (above 18y old) \n 1991-2000",
                     "Shifts in Gini Coefficient \n 1991-2000",
                     "Shifts in Population Below Extreme Poverty Line  \n 1991-2000",
                     "Shifts in Population Below Poverty Line  \n 1991-2000",
                     "Shifts in Income per capita  \n 1991-2000",
                     "Shifts in Access to Water Network  \n 1991-2000",
                     "Shifts in Access to Garbage Collection Service  \n 1991-2000",
                     "Shifts in Access to Electricity  \n 1991-2000",
                     "Shifts in Access to Sewage Network  \n 1991-2000",
                     "Shifts in IDH  \n 1991-2000"))

for(i in 1:nrow(variables)){
  bs2(census_shift,variables[i,1],variables[i,2],"census",variables[i,1])
}


# Census Shifts 2000-2010
#------------------------------

variables <- cbind(grep("shift10",names(census_shift), value = T),
                   c("Shifts in Expected Years of Study  \n 2000-2010",
                     "Shifts in Iliteracy Rate (above 18y old) \n 2000-2010",
                     "Shifts in Gini Coefficient \n 2000-2010",
                     "Shifts in Population Below Extreme Poverty Line  \n 2000-2010",
                     "Shifts in Population Below Poverty Line  \n 2000-2010",
                     "Shifts in Income per capita  \n 2000-2010",
                     "Shifts in Access to Water Network  \n 2000-2010",
                     "Shifts in Access to Garbage Collection Service  \n 2000-2010",
                     "Shifts in Access to Electricity  \n 2000-2010",
                     "Shifts in Access to Sewage Network  \n 2000-2010",
                     "Shifts in IDH  \n 2000-2010"))

for(i in 1:nrow(variables)){
  bs2(census_shift,variables[i,1],bins_w = 1,variables[i,2],"census",variables[i,1])
}



# 6. Baseline binscatters
# =================================================================

# imr and spending
variables <- cbind(c("tx_mi","tx_mi_icsap","tx_mi_nicsap",
                      "finbra_desp_saude_san_pcapita","siops_despsaude_pcapita","siops_pct_recproprios_ec29"),
                   c("Baseline Infant Mortality Rate",
                     "Baseline Infant Mortality Rate - Amenable to Primary Care",
                     "Baseline Infant Mortality Rate - Non-Amenable to Primary Care",
                     "Baseline Health and Sanitation Spending per capita",
                     "Baseline Health Spending per capita",
                     "Baseline % of Own Resource Spent on Health"))


for(i in 1:3){
  bs2(df_imr %>% filter(ano==2000),variables[i,1],bins_w = 1,variables[i,2],"baseline",variables[i,1])
}

for(i in 4:6){
  bs2(df_exp %>% filter(ano==2000),variables[i,1],bins_w = 1,variables[i,2],"baseline",variables[i,1])
}


variables <- cbind(census_var,
                   c("Baseline Expected Years of Study",
                     "Baseline Iliteracy Rate (above 18y old)",
                     "Baseline Gini Coefficient \n 2000-2010",
                     "Baseline Population Below Extreme Poverty Line",
                     "Baseline Population Below Poverty Line",
                     "Baseline Income per capita  \n 2000-2010",
                     "Baseline Access to Water Network",
                     "Baseline Access to Garbage Collection Service",
                     "Baseline Access to Electricity",
                     "Baseline Access to Sewage Network",
                     "Baseline IDH"))

for(i in 1:nrow(variables)){
  bs2(census_shift,variables[i,1],bins_w = 1,variables[i,2],"baseline",variables[i,1])
}

# baseline population
bs2(df_imr,"pop",bins_w = 0,"Baseline Population","baseline","pop")

