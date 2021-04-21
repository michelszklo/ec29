#######################################################################################################
# Author: Michel Szklo
# April 2021
# 
# This scripts runs regressions for public spending
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
            'readstata13',
            'lfe',
            'fastDummies')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


options(digits = 15)

path <- "C:/Users/Michel/Google Drive/DOUTORADO FGV/Artigos/EC 29-2000/"


# 1. Load data frame
# =================================================================
load("regs.RData")


# 2. functions
# =================================================================


regress <- function(y,output_name){
  
  for(df in c("df_below","df_above")){
    
    spec <- substr(df,4,8)
    obj <- paste0("out_",spec)
    
    df_reg <- get(df)
    ln_y <- paste0("ln_",y)
    df_reg[ln_y] <- sapply(df_reg[y], function(x) ifelse(x==0,x+0.000001,x))
    
    
    df_reg <- df_reg %>% 
      mutate_at(ln_y,log)
    
    df_reg <- df_reg[complete.cases(df_reg[,ln_y]),]
    
    formula1 <- as.formula(paste(ln_y,spec1))
    formula2 <- as.formula(paste(ln_y,spec2))
    formula3 <- as.formula(paste(ln_y,spec3))
    
    formula1_post <- as.formula(paste(ln_y,spec1_post))
    formula2_post <- as.formula(paste(ln_y,spec2_post))
    formula3_post <- as.formula(paste(ln_y,spec3_post))
    
    
    fit1 <- felm(formula = formula1, data = df_reg, exactDOF = TRUE)
    fit2 <- felm(formula = formula2, data = df_reg[complete.cases(df_reg[,ln_y]),], exactDOF = TRUE)
    fit3 <- felm(formula = formula3, data = df_reg[complete.cases(df_reg[,ln_y]),], exactDOF = TRUE)
    
    output_function <- function(sample,obj){
      out1 <- cbind.data.frame(fit1$coefficients,fit1$se) %>% 
        slice(1:13) %>% 
        rename(b = 1,
               se =2) %>% 
        mutate(lb = b - 1.96*se,
               ub = b + 1.96*se,
               year = seq.int(1998,2010),
               spec = paste0("spec1"),
               sample = sample)
      
      out2 <- cbind.data.frame(fit2$coefficients,fit2$se) %>% 
        slice(1:13) %>% 
        rename(b = 1,
               se =2) %>% 
        mutate(lb = b - 1.96*se,
               ub = b + 1.96*se,
               year = seq.int(1998,2010),
               spec = paste0("spec2"),
               sample = sample)
      
      out3 <- cbind.data.frame(fit3$coefficients,fit3$se) %>% 
        slice(1:13) %>% 
        rename(b = 1,
               se =2) %>% 
        mutate(lb = b - 1.96*se,
               ub = b + 1.96*se,
               year = seq.int(1998,2010),
               spec = paste0("spec3"),
               sample = sample)
      
      out <- rbind.data.frame(out1,out2,out3)
      out[is.na(out)] <- 0
      rownames(out) <- NULL
      
      assign(obj,out, envir = .GlobalEnv)
      
    }
    
    output_function(spec,obj)
    
    
  }
  
  out <- rbind(out_below,out_above)
  assign(output_name,out,envir = .GlobalEnv)
  
}
regplot <- function(df,y0,yf,ys){
  
  index <- grep(df,graph_yaxis)
  df_plot <- get(df) %>% as.data.frame()
  y_axis <- graph_yaxis[index,2]
  
  df_plot %>% 
    mutate(sample = ifelse(sample=="above","Above target","Below target"),
           spec = ifelse(spec=="spec1","1",spec),
           spec = ifelse(spec=="spec2","2",spec),
           spec = ifelse(spec=="spec3","3",spec)) %>% 
    mutate(spec = as.factor(spec)) %>%
    ggplot(aes(x=year, y=b, ymin = lb, ymax = ub, Group = sample, shape = spec))+
    geom_hline(yintercept = 0, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "dotted") +
    geom_pointrange(position = position_dodge(width=1), size = 0.4, alpha = 0.8, aes(color = sample)) +
    scale_shape_manual(values = c(8,16,15,17)) +
    scale_x_continuous(breaks = seq(1998,2010,1), limits = c(1997.5,2010.5)) +
    scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf),labels = comma) +
    theme_light() +
    labs(y = y_axis) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size=10),
          legend.position="bottom") +
    guides(shape=guide_legend(title="Specification"),
           color=guide_legend(title="Sample"))
}

output <- function(y,output_name,y0,yf,ys,graph_file){
  
  regress(y = y,
          output_name = output_name)
  plot <- regplot(df = output_name,
                  y0 = y0,
                  yf = yf,
                  ys = ys)
  
  ggsave(paste0("spending/",graph_file,".png"),
         plot = plot,
         device = "png",
         width = 7, height = 5,
         units = "in")
  ggsave(paste0("spending/",graph_file,".pdf"),
         plot = plot,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
  
}



# 3. Graph axis title map
# =================================================================

graph_yaxis <- rbind(cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
                     cbind('finbra_desp_transporte_pcapita','Trasnport Spending per capita - Total (log)'),
                     cbind('finbra_desp_educ_cultura_pcapita','Education and Culture Spending per capita (log)'),
                     cbind('finbra_desp_hab_urb_pcapita','Housing and Urban Spending per capita (log)'),
                     cbind('finbra_desp_assist_prev_pcapita','Social Security Spending per capita (log)'),
                     cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
                     cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
                     cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Transfers (log)'),
                     cbind('siops_desppessoal_pcapita','Health Spending per capita - Human Resources (log)'),
                     cbind('siops_despinvest_pcapita','Health Spending per capita - Investiment (log)'),
                     cbind('siops_despservicoster_pcapita','Health Spending per capita - 3rd parties services (log)'),
                     cbind('siops_despoutros_pcapita','Health Spending per capita - other expenditures (log)'))



# 4. Running regressions
# =================================================================

for(i in seq.int(1,5)){
  var <- graph_yaxis[i,1]
  output(var,var,-0.6,0.6,0.1,var)
}

for(i in seq.int(6,12)){
  var <- graph_yaxis[i,1]
  output(var,var,-0.8,0.8,0.1,var)
}

output(var,var,-0.8,0.8,0.1,var)


graph_yaxis[6,1]