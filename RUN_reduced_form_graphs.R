#--------------------------------------------------------------------------------
#--- Author: Michel Szklo
#--- April 2022
#--- Edits: Damian Clarke
#---        Most recent: 27/03/2025 (DC)
#--- 
#--- This scripts runs reduced form graphs for all outcomes.  The script relies
#---  on the following scripts / functions:
#---    - 08_regs_vars_specs.R / reduced_yearly_imr()
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#--- (0) Set-up
#--------------------------------------------------------------------------------
rm(list=ls())

#Set-up path for principal directory
if(Sys.getenv("USERNAME")=="dcc213") {
  dir <- "/home/dcc213/investigacion/2021/decentralization/github/"
} else if (Sys.getenv("USERNAME") == "damian") {
  dir <- "/home/damian/investigacion/2021/decentralization/github/"  
} else {
  dir <- "G:/My Drive/DOUTORADO FGV/Artigos/EC 29-2000/"
}
SRC <- paste0(dir,"source/")
DAT <- paste0(dir,"data/processed/")
TAB <- paste0(dir,"results/tables/")
FIG <- paste0(dir,"results/figures/")


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
            'fastDummies',
            'purrr',
            'boot',
            'broom',
            'modelsummary',
            'ggsci',
            'HonestDiD')
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)
options(digits = 15)

#Set Base year (first year in data)
YEAR <- 1998

rnames <- seq.int(YEAR,2010) %>%
  as.data.frame() %>% 
  slice(rep(1:n(), each = 2)) %>% 
  mutate(b = paste0("DistEC29 * ",.)) %>% 
  select(b) %>% 
  rbind("obs")

# Generate file for tabular output 
NRR <- (2010-YEAR+1)*2+1
table_main <- cbind(rnames,data.frame(matrix(nrow = NRR, ncol = 0)))
table_ab   <- cbind(rnames,data.frame(matrix(nrow = NRR, ncol = 0)))


#--------------------------------------------------------------------------------
#--- (1) Plotting functions for robustness plots 
#---     Accepts: variable name (var)
#---             dataframe with estimates, LB, UB, etc. (combined_df)
#--------------------------------------------------------------------------------
# Plotting functions
robustPlot <- function(var, combined_df) {
  graph <- ggplot(combined_df, aes(x=year, y=estimates, color = controls, linetype = controls)) + 
    geom_line(size=1.2) +
    geom_errorbar(aes(ymin = lb2, ymax = ub2), width=0.2, color = "gray") +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", linewidth = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(YEAR,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
    theme_light() +
    scale_color_viridis_d() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x = element_text(size=11),
          axis.text = element_text(size = 11),
          legend.position="bottom",
          legend.title = element_blank())
  ggsave(paste0(FIG,"robust/",var,".pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
}

robustPlotAB <- function(var, combined_df) {
  graph <- ggplot(combined_df, aes(x=year, y=estimates, color = target,linetype = controls)) + 
    geom_point(shape=15) +
    geom_errorbar(aes(ymin = lb2, ymax = ub2, color = target,linetype = controls), width=0.2) +
    geom_hline(yintercept = 0, color = "red", linewidth = 0.3, alpha = 1, linetype = "dashed") +
    geom_vline(xintercept = 2000, color = "#9e9d9d", linewidth = 0.5, alpha = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(YEAR,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
    theme_light() +
    labs(y = var_name,
         x = "Year",
         shape = "Specification") +
    theme(plot.title = element_text(size = 11, face = "bold"),
          axis.title.x = element_text(size=11),
          axis.text = element_text(size = 11),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 9)) +
    guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))
  ggsave(paste0(FIG,"robust/",var,"_ab.pdf"),
         plot = graph,
         device = "pdf",
         width = 7, height = 5,
         units = "in")
}


#--------------------------------------------------------------------------------
#--- (2) Load data and set up data frames
#--------------------------------------------------------------------------------
load(paste0(DAT,"regs.RData")) 
#df holds full data


# Examine spending data removing outliers as spenders 5sd above or below the mean
outliers <- df %>% 
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

df_noout <- df %>% 
  filter(!(cod_mun %in% outliers))


# Define variable sets
vars_finbra <- c('finbra_recorc_pcapita','finbra_desp_o_pcapita',
                 'finbra_desp_saude_san_pcapita','finbra_desp_nao_saude_pcapita',
                 'finbra_despsocial_pcapita','finbra_desp_outros_area_pcapita',
                 'gdp_mun_pcapita','pbf_pcapita','t_tx_mi_baseline',
                 'dist_ec29_baseline')

nonzero_finbra <- c('finbra_recorc_pcapita','finbra_desp_o_pcapita',
                    'finbra_desp_saude_san_pcapita','finbra_desp_nao_saude_pcapita',
                    'finbra_despsocial_pcapita','finbra_desp_outros_area_pcapita')

vars_siops <- c('siops_despsaude_pcapita','siops_desprecpropriosaude_pcapita',
                'siops_despexrecproprio_pcapita','siops_desppessoal_pcapita',
                'siops_despinvest_pcapita','siops_despservicoster_pcapita',
                'siops_despoutros_pcapita','gdp_mun_pcapita',
                'pbf_pcapita','t_tx_mi_baseline','dist_ec29_baseline')

nonzero_siops <- c('siops_despsaude_pcapita','siops_desprecpropriosaude_pcapita',
                   'siops_despexrecproprio_pcapita','siops_desppessoal_pcapita',
                   'siops_despinvest_pcapita','siops_despservicoster_pcapita')

# Filter for balanced data
df_balance_finbra <- df_noout[
  complete.cases(df_noout[vars_finbra]) &
    rowSums(df_noout[nonzero_finbra] == 0) == 0,
]

df_balance_siops <- df_noout[
  complete.cases(df_noout[vars_siops]) &
    rowSums(df_noout[nonzero_siops] == 0) == 0 &
    df_noout$siops_despoutros_pcapita > 0,
]



#--------------------------------------------------------------------------------
#--- () Descriptive graph
#--------------------------------------------------------------------------------
generate_spending_revenue_plots <- function(data, suffix = "") {
  # Spending labels and variable order
  labels <- c(
    finbra_desp_legislativa_pcapita = "Legislative",
    finbra_desp_judiciaria_pcapita = "Judiciary",
    finbra_desp_adm_pcapita = "Administration",
    finbra_desp_agricultura_pcapita = "Agriculture",
    finbra_desp_educ_cultura_pcapita = "Education & Culture",
    finbra_desp_hab_urb_pcapita = "Housing & Urbanism",
    finbra_desp_ind_com_pcapita = "Industry & Commerce",
    finbra_desp_saude_san_pcapita = "Health & Sanitation",
    finbra_desp_assist_prev_pcapita = "Social Assistance & Pensions",
    finbra_desp_transporte_pcapita = "Transport",
    finbra_desp_seguranca_pcapita = "Public Security"
  )

  vars_ordered <- c(
    "finbra_desp_saude_san_pcapita",
    "finbra_desp_legislativa_pcapita",
    "finbra_desp_judiciaria_pcapita",
    "finbra_desp_adm_pcapita",
    "finbra_desp_agricultura_pcapita",
    "finbra_desp_educ_cultura_pcapita",
    "finbra_desp_hab_urb_pcapita",
    "finbra_desp_ind_com_pcapita",
    "finbra_desp_assist_prev_pcapita",
    "finbra_desp_transporte_pcapita",
    "finbra_desp_seguranca_pcapita"
  )

  # Spending: mean per capita
  long_df <- data %>%
    group_by(ano) %>%
    summarise(across(all_of(vars_ordered), mean, na.rm = TRUE)) %>%
    pivot_longer(cols = all_of(vars_ordered), names_to = "variable", values_to = "mean_value") %>%
    mutate(label = labels[variable],
           label = factor(label, levels = labels[vars_ordered]))

  long_df_prop <- long_df %>%
    group_by(ano) %>%
    mutate(prop_value = mean_value / sum(mean_value))

  plot_totals <- ggplot(long_df, aes(x = ano, y = mean_value, fill = label)) +
    geom_area() +
    scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.95) +
    scale_x_continuous(breaks = 1998:2010, limits = c(1998, 2010)) +
    scale_y_continuous(breaks = seq(0, 2000, by = 500), limits = c(0, 2000)) +
    labs(x = "Year", y = "Mean R$/capita", fill = "Spending Category") +
    geom_vline(xintercept = c(2000, 2005), linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

  plot_props <- ggplot(long_df_prop, aes(x = ano, y = prop_value, fill = label)) +
    geom_area() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.95) +
    scale_x_continuous(breaks = 1998:2010, limits = c(1998, 2010)) +
    labs(x = "Year", y = "Share of Total Spending", fill = "Spending Category") +
    geom_vline(xintercept = c(2000, 2005), linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0(FIG, "descriptive/spendingTotals", suffix, ".pdf"), plot = plot_totals, width = 10, height = 6, dpi = 300)
  ggsave(paste0(FIG, "descriptive/spendingProportions", suffix, ".pdf"), plot = plot_props, width = 10, height = 6, dpi = 300)

  # Revenue
  df_revenue_long <- data %>%
    mutate(
      other_current_pcapita = finbra_reccorr_pcapita - finbra_rectribut_pcapita - finbra_rectransf_pcapita
    ) %>%
    select(ano, finbra_rectribut_pcapita, finbra_rectransf_pcapita, other_current_pcapita) %>%
    pivot_longer(cols = -ano, names_to = "category", values_to = "value") %>%
    group_by(ano, category) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      label = recode(category,
                     finbra_rectribut_pcapita = "Tax Revenue",
                     finbra_rectransf_pcapita = "Transfers",
                     other_current_pcapita = "Other Revenue"),
      label = factor(label, levels = c("Other Revenue", "Transfers", "Tax Revenue"))
    )

  df_revenue_prop <- df_revenue_long %>%
    group_by(ano) %>%
    mutate(prop_value = mean_value / sum(mean_value)) %>%
    ungroup()

  plot_rev_total <- ggplot(df_revenue_long, aes(x = ano, y = mean_value, fill = label)) +
    geom_area() +
    scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.95) +
    scale_x_continuous(breaks = 1998:2010, limits = c(1998, 2010)) +
    scale_y_continuous(breaks = seq(0, 2000, by = 500), limits = c(0, 2000)) +
    labs(x = "Year", y = "Mean Revenue per Capita (R$)", fill = "Revenue Category") +
    geom_vline(xintercept = c(2000, 2005), linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

  plot_rev_prop <- ggplot(df_revenue_prop, aes(x = ano, y = prop_value, fill = label)) +
    geom_area() +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = 1998:2010, limits = c(1998, 2010)) +
    scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.95) +
    labs(x = "Year", y = "Share of Revenue", fill = "Revenue Category") +
    geom_vline(xintercept = c(2000, 2005), linetype = "dashed", color = "red") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(paste0(FIG, "descriptive/revenueMeans", suffix, ".pdf"), plot = plot_rev_total, width = 10, height = 6, dpi = 300)
  ggsave(paste0(FIG, "descriptive/revenueProportions", suffix, ".pdf"), plot = plot_rev_prop, width = 10, height = 6, dpi = 300)
}
generate_spending_revenue_plots(df_balance_finbra, suffix = "_all")
generate_spending_revenue_plots(df_balance_finbra %>% filter(dist_ec29_baseline <= 0), suffix = "_above")
generate_spending_revenue_plots(df_balance_finbra %>% filter(dist_ec29_baseline > 0), suffix = "_below")

generate_spending_revenue_plots(df_balance_finbra %>% filter(dist_ec29_baseline <= -0.1), suffix = "_above10plus")
generate_spending_revenue_plots(df_balance_finbra %>% filter(dist_ec29_baseline <= -0.05&dist_ec29_baseline > -0.10), suffix = "_above5-10")
generate_spending_revenue_plots(df_balance_finbra %>% filter(dist_ec29_baseline > 0.05&dist_ec29_baseline < 0.1), suffix = "_below5-10")
generate_spending_revenue_plots(df_balance_finbra %>% filter(dist_ec29_baseline > 0.1), suffix = "_below10plus")



#--------------------------------------------------------------------------------
#--- (2) Run analysis for fiscal responses
#--------------------------------------------------------------------------------
yearly_folder <- "fiscal_response/"

var_map1 <- rbind(
  cbind('finbra_recorc_pcapita','Total Revenue per capita (log)'),
  cbind('finbra_desp_o_pcapita','Total Spending per capita (log)'),                 
  cbind('finbra_desp_saude_san_pcapita','Health and Sanitation Spending per capita (log)'),
  cbind('finbra_desp_nao_saude_pcapita','Non-Health Spending per capita (log)'),
  cbind('finbra_despsocial_pcapita','Non-Health Social Spending per capita (log)'),
  cbind('finbra_desp_outros_area_pcapita','Non-Social Spending per capita (log)'),
  cbind('finbra_impostos_total_pcapita', 'Total Tax Revenue (log)'),
  cbind('finbra_iptu_pcapita', 'Property Tax Revenue (log)'),
  cbind('finbra_iss_pcapita', 'Services Tax Revenue (log)'),
  cbind('finbra_passivo_pcapita','Total Liabilities (log)'),
  cbind('finbra_passivo_pcapita','Financial Liabilities (log)'),
  cbind('siops_despsaude_pcapita','Health Spending per capita - Total (log)'),
  cbind('siops_desprecpropriosaude_pcapita','Health Spending per capita - Own Resources (log)'),
  cbind('siops_despexrecproprio_pcapita','Health Spending per capita - Other Resources (log)'),
  cbind('siops_desppessoal_pcapita','Health Spending per capita - Personnel (log)'),
  cbind('siops_despinvest_pcapita','Health Spending per capita - Investment (log)'),
  cbind('siops_despservicoster_pcapita','Health Spending per capita - Outsourced (3rd parties services) (log)'),
  cbind('siops_despoutros_pcapita','Health Spending per capita - Admin, Management, others (log)')
)

#--------------------------------------------------------------------------------
#--- (2A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,18,1)) {
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  if (i %in% c(1,2,4,5,6)) {
    x_min <- -1
    x_max <- 2.5
    x_inc <- 0.25
  } else if (i %in% c(3, 7:18)) {
    x_min <- -3
    x_max <- 9.25
    x_inc <- 1
  } else {
    x_min <- -1000
    x_max <- 1000
    x_inc <- 100
  }

  res <- reduced_yearly_imr(var,var_name,df_noout,1,1998,-1000,1000,10,
                            paste0("1_cont_log_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df_noout,1,1998,x_min,x_max,x_inc,
                              paste0("1_cont_log_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (2B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,18,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()

  if (i %in% c(1,2,4,5,6)) {
    x_min <- -2.5
    x_max <- 2.5
    x_inc <- 0.5
  } else if (i %in% c(3, 7:18)) {
    x_min <- -10
    x_max <- 14
    x_inc <- 2
  } else {
    x_min <- -1000
    x_max <- 1000
    x_inc <- 100
  }


  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df_noout,1,1998,x_min,x_max,x_inc,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  ##table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}



#--------------------------------------------------------------------------------
#--- (3) Run analysis for fiscal responses with balanced samples
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#--- (3A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,18,1)) {
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% c(1, 2, 4, 5, 6)) {
    list(x_min = -1, x_max = 2.5, x_inc = 0.25)
  } else if (i %in% c(3, 7:18)) {
    list(x_min = -3, x_max = 9.25, x_inc = 1)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }


  ##Set sample and title
  if (i %in% seq(1,11,1)) {
    df_est <- df_balance_finbra
    fname  <- paste0("1_bal_fin_",i)
  } else {
    df_est <- df_balance_siops
    fname  <- paste0("1_bal_siops_",i)
  }

  res <- reduced_yearly_imr(var,var_name,df_est,1,1998,-1000,1000,10,
                            fname,weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df_est,1,1998,
                              vals$x_min,vals$x_max,vals$x_inc,
                              fname,weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (3B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,18,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% c(1, 2, 4, 5, 6)) {
    list(x_min = -2.5, x_max = 2.5, x_inc = 0.5)
  } else if (i %in% c(3, 7:18)) {
    list(x_min = -10, x_max = 14, x_inc = 2)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }



  ##Set sample and title
  if (i %in% seq(1,11,1)) {
    df_est <- df_balance_finbra
    fname  <- paste0("2_bal_fin_",i)
  } else {
    df_est <- df_balance_siops
    fname  <- paste0("2_bal_siops_",i)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df_est,1,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                 fname,weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}






#--------------------------------------------------------------------------------
#--- (3C) Implement Rambachan & Roth
#--------------------------------------------------------------------------------
for (i in c(2,3,4,5)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df_balance_finbra,1,1998,-1,2.5,0.25,"remove",weight = "peso_pop",year_cap = 2010,cont = 1,ramb_roth=T) # ec29baseline
}



#--------------------------------------------------------------------------------
#--- (3D) Proportions
#--------------------------------------------------------------------------------
var_map1 <- rbind(
  cbind('finbra_desp_saude_san_share','Health and Sanitation Spending per capita (share)'),
  cbind('finbra_desp_pessoal_share','Personnel (share)'),
  cbind('finbra_desp_investimento_share','Investment (share)'),
  cbind('finbra_desp_outros_nature_share','Other nature? (share)'),
  cbind('finbra_desp_adm_share', 'Admin (share)'),
  cbind('finbra_desp_transporte_share', 'Transport (share)'),
  cbind('finbra_desp_educ_cultura_share', 'Education and culture (share)'),
  cbind('finbra_desp_hab_urb_share', 'Habitational (share)'),
  cbind('finbra_desp_assist_prev_share', 'Assist prev (share)'),
  cbind('finbra_desp_outros_area_share', 'Other areas? (share)'),
  cbind('finbra_rectransf_share', 'Transfers (share)'),
  cbind('finbra_rectribut_share', 'Taxes (share)'),
  cbind('finbra_rec_outros_share','Other receipts (share)'), 
  cbind('siops_desprecpropriosaude_share','Health Spending per capita - Own Resources (share)'),
  cbind('siops_despexrecproprio_share','Health Spending per capita - Other Resources (share)'),
  cbind('siops_desppessoal_share','Health Spending per capita - Personnel (share)'),
  cbind('siops_despinvest_share','Health Spending per capita - Investment (share)'),
  cbind('siops_despservicoster_share','Health Spending per capita - Outsourced (3rd parties services) (share)'),
  cbind('siops_despoutros_share','Health Spending per capita - Admin, Management, others (share)'),
  cbind('finbra_desp_legislativa_pcapita', 'Legislative'),
  cbind('finbra_desp_judiciaria_pcapita', 'Judiciary'),
  cbind('finbra_desp_adm_pcapita', 'Administration'),
  cbind('finbra_desp_agricultura_pcapita', 'Agriculture'),
  cbind('finbra_desp_educ_cultura_pcapita', 'Education & Culture'),
  cbind('finbra_desp_hab_urb_pcapita', 'Housing & Urbanism'),
  cbind('finbra_desp_ind_com_pcapita', 'Industry & Commerce'),
  cbind('finbra_desp_saude_san_pcapita', 'Health & Sanitation'),
  cbind('finbra_desp_assist_prev_pcapita', 'Social Assistance & Pensions'),
  cbind('finbra_desp_transporte_pcapita', 'Transport'),
  cbind('finbra_desp_seguranca_pcapita', 'Public Security'),
  cbind('finbra_rectribut_pcapita', 'Tax Revenue'),
  cbind('finbra_rectransf_pcapita', 'Transfers'),
  cbind('finbra_reccorr_pcapita', 'Current Revenue')
  )

#--------------------------------------------------------------------------------
#--- (3E) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,33,1)) {
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% c(1:12, 16, 17)) {
    list(x_min = -0.5, x_max = 0.5, x_inc = 0.1)
  } else if (i %in% c(13, 14, 15, 18, 19)) {
    list(x_min = -1, x_max = 1, x_inc = 0.2)
  } else {
    list(x_min = -4, x_max = 4, x_inc = 0.5)
  }
  modval <- 3
  if (i %in% c(20:33)) {modval <- 1}

  ##Set sample and title
  if (i %in% c(1:13, 20:33)) {
    df_est <- df_balance_finbra
    fname  <- paste0("shares_",i)
  } else {
    df_est <- df_balance_siops
    fname  <- paste0("shares_",i)
  }

  res <- reduced_yearly_imr(var,var_name,df_est,modval,1998,
                            vals$x_min,vals$x_max,vals$x_inc,
                            fname,weight = "peso_pop",
                            year_cap = 2010, cont = 1, spec=1)
}


#--------------------------------------------------------------------------------
#--- (3F) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,33,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  output_list <- list()
 
  ##Set axis
  vals <- if (i %in% c(1:12, 16, 17)) {
    list(x_min = -0.5, x_max = 0.5, x_inc = 0.1)
  } else if (i %in% c(13, 14, 15, 18, 19)) {
    list(x_min = -1, x_max = 2, x_inc = 0.2)
  } else {
    list(x_min = -5, x_max = 5, x_inc = 1)
  }


  modval <- 3
  if (i %in% c(20:33)) {modval <- 1}

  ##Set sample and title
  if (i %in% c(1:13, 20:33)) {
    df_est <- df_balance_finbra
    fname  <- paste0("sharesAB_",i)
  } else {
    df_est <- df_balance_siops
    fname  <- paste0("sharesAB_",i)
  }

  res <- reduced_yearly_ab_imr(var,var_name,df_est,modval,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                 fname,weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=1)
}
#--------------------------------------------------------------------------------
#--- (4) Run analysis for access and production
#--------------------------------------------------------------------------------
# creating missing SIA variable
df <- df %>% 
  mutate(sia_nab_pcapita = sia_pcapita - sia_ab_pcapita)
yearly_folder <- "access_production/"

var_map <- rbind(
  cbind('ACS_popprop','Population covered (share) by Community Health Agents'),
  cbind('eSF_popprop','Population covered (share) by Family Health Agents'),
  cbind('siab_accomp_especif_pcapita','N. of People Visited by Primary Care Agents (per capita)'),
  cbind('siab_accomp_especif_pacs_pcapita','N. of People Visited by Community Health Agents (per capita)'),
  cbind('siab_accomp_especif_psf_pcapita','N. of People Visited by Family Health Agents (per capita)'),
  cbind('siab_visit_cons_pcapita','N. of Household Visits and Appointments (per capita)'),
  cbind('siab_visit_cons_pacs_pcapita','N. of Household Visits and Appointments from Community Health Agents (per capita)'),
  cbind('siab_visit_cons_psf_pcapita','N. of Household Visits and Appointments from Family Health Agents (per capita)'),
  cbind('sia_ncnes_amb_mun_pcapita','N. of Health Facilities with Ambulatory Service (per capita*1000)'),
  cbind('sia_ncnes_acs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Teams (per capita*1000)'),
  cbind('sia_ncnes_psf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Teams (per capita*1000)'),
  cbind('sia_ncnes_medcom_pcapita','N. of Health Facilities with Ambulatory Service and Community Doctors (per capita*1000)'),
  cbind('sia_ncnes_medpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Doctors (per capita*1000)'),
  cbind('sia_ncnes_enfacs_pcapita','N. of Health Facilities with Ambulatory Service and ACS Nurses (per capita*1000)'),
  cbind('sia_ncnes_enfpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nurses (per capita*1000)'),
  cbind('sia_ncnes_outpsf_pcapita','N. of Health Facilities with Ambulatory Service and PSF Nursing Assistants (per capita*1000)'),
  cbind('sia_pcapita','N. Outpatient Procedures (per capita)'),
  cbind('sia_ab_pcapita','N. Primary Care Outpatient Procedures (per capita)'),
  cbind('sia_nab_pcapita','N. Non-Primary Care Outpatient Procedures (per capita)'), # precisa criar
  cbind('sia_nprod_amb_lc_mun_pcapita','N. Low & Mid Complexity Outpatient Procedures (per capita)'),
  cbind('sia_nprod_amb_hc_mun_pcapita','N. High Complexity Outpatient Procedures (per capita)'),
  cbind('birth_prenat_ig','Proportion of births with unknown prenatal care coverage'),
  cbind('birth_prenat_0','Proportion of births with 0 prenatal visits'),
  cbind('birth_prenat_1_6','Proportion of births with 1-6 prenatal visits'),
  cbind('birth_prenat_7_plus','Proportion of births with 7+ prenatal visits')
)




#--------------------------------------------------------------------------------
#--- (4A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,25,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% seq(1, 5, 1)) {
    list(x_min = -1, x_max = 1.5, x_inc = 0.5)
  } else if (i %in% c(6,7,8)) {
    list(x_min = -1.5, x_max = 3, x_inc = 0.5)
  } else if (i %in% seq(9,16,1)) {
    list(x_min = -0.5, x_max = 0.5, x_inc = 0.1)
  } else if (i %in% seq(17,21,1)) {
    list(x_min = -7, x_max = 14, x_inc = 1)
  } else if (i %in% seq(22,25,1)) {
    list(x_min = -0.3, x_max = 0.3, x_inc = 0.1)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,3,1998,
                              vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (4B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,25,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% c(1, 2)) {
    list(x_min = -1, x_max = 1.4, x_inc = 0.2)
  } else if (i %in% c(3, 4, 5)) {
    list(x_min = -1.75, x_max = 1.5, x_inc = 0.25)
  } else if (i %in% c(6, 7, 8)) {
    list(x_min = -4.5, x_max = 4.5, x_inc = 0.5)
  } else if (i %in% seq(9, 16, 1)) {
    list(x_min = -0.8, x_max = 0.4, x_inc = 0.2)
  } else if (i %in% seq(17, 21, 1)) {
    list(x_min = -20, x_max = 30, x_inc = 5)
  } else if (i %in% seq(22, 25, 1)) {
    list(x_min = -0.5, x_max = 0.5, x_inc = 0.1)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                  paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}



#--------------------------------------------------------------------------------
#--- (5) Run analysis for inputs
#--------------------------------------------------------------------------------
yearly_folder <- "inputs/"

var_map <- rbind(
  cbind('ams_hospital_mun_pcapita','N. of Municipal Hospitals (per capita*1000)'),
  cbind('ams_hospital_nmun_pcapita','N. of Federal and State Hospitals (per capita*1000)'),
  cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),  
  cbind('ams_hr_all_pcapita',"N. of Health Professionals (per capita*1000)"),
  cbind('ams_hr_superior_pcapita','N. of Doctors (per capita*1000)'),
  cbind('ams_hr_technician_pcapita','N. of Nurses (per capita*1000)'),
  cbind('ams_hr_elementary_pcapita','N. of Nursing Assistants (per capita*1000)'),
  cbind('ams_hr_admin_pcapita','N. of Administrative Professionals (per capita*1000)'),
  cbind('ams_hospital_mun_esp_pcapita', 'N. of Specialty Hospitals (per capita*1000)'),
  cbind('ams_unity_mun_pcapita','N. of Health Facilities (per capita*1000)'),
  cbind('ams_therapy_mun_pcapita','N. of Therapy Units (per capita*1000)')
)

#--------------------------------------------------------------------------------
#--- (5A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,11,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% c(1, 2, 3)) {
    list(x_min = -0.1, x_max = 0.125, x_inc = 0.025)
  } else if (i %in% c(4)) {
    list(x_min = -15, x_max = 30, x_inc = 5)
  } else if (i %in% seq(5,11,1)) {
    list(x_min = -10, x_max = 15, x_inc = 5)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                            3,1998,-1000,1000,10,paste0("1_cont_level_",i),
                            weight = "reweightPop",year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                              3,1998,vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (5B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,11,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% c(1, 2, 3)) {
    list(x_min = -0.1, x_max = 0.125, x_inc = 0.025)
  } else if (i %in% c(4)) {
    list(x_min = -40, x_max = 80, x_inc = 20)
  } else if (i %in% seq(5, 11, 1)) {
    list(x_min = -15, x_max = 35, x_inc = 5)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,
                                 df %>% mutate(above_pre_99_dist_ec29_baseline=0,below_pre_99_dist_ec29_baseline=0)
                                 ,3,1998,vals$x_min,vals$x_max,vals$x_inc,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}



#--------------------------------------------------------------------------------
#--- (6) Run analysis for hospitalization
#--------------------------------------------------------------------------------
yearly_folder <- "hosp/"

var_map <- rbind(
  cbind('tx_sih_infant','Infant Hospitalization Rate (pop 0-1y * 1000)'),
  cbind('tx_sih_infant_icsap','Infant Hospitalization Rate - APC (pop 0-1y * 1000)'),
  cbind('tx_sih_infant_nicsap','Infant Hospitalization Rate - non-APC (pop 0-1y * 1000)'),
  cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
  cbind('tx_sih_maternal','Maternal Hospitalization Rate (women 10-49y * 1000)'),
  cbind('sih_infant','Infant Hospitalization - Total (log)'),
  cbind('sih_infant_icsap','Infant Hospitalization - APC (log)'),
  cbind('sih_infant_nicsap','Infant Hospitalization - non-APC (log)'),
  cbind('sih_maternal','Maternal Hospitalization - Total (log)')
)




#--------------------------------------------------------------------------------
#--- (6A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,9,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% seq(1, 4, 1)) {
    list(x_min = -600, x_max = 1000, x_inc = 200)
  } else if (i %in% seq(5,9,1)) {
    list(x_min = -50, x_max = 50, x_inc = 10)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  res <- reduced_yearly_imr(var,var_name,df,
                            3,1998,-1000,1000,10,paste0("1_cont_level_",i),
                            weight = "reweightPop",year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,
                              3,1998,vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}

#--------------------------------------------------------------------------------
#--- (6B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% seq(1, 4, 1)) {
    list(x_min = -1500, x_max = 3500, x_inc = 500)
  } else if (i %in% seq(5, 9, 1)) {
    list(x_min = -50, x_max = 50, x_inc = 10)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                  paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}



#--------------------------------------------------------------------------------
#--- (6C) Implement Rambachan & Roth
#--------------------------------------------------------------------------------
for (i in seq(1,9,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-1,2.5,0.25,"remove",
                     weight = "peso_pop",year_cap = 2010,cont = 1,ramb_roth=T) 
}


#--------------------------------------------------------------------------------
#--- (7) Fertility and Birth
#--------------------------------------------------------------------------------
yearly_folder <- "birth/"


var_map <- rbind(
  cbind('birth_fertility','Fertility (N. of Births per 10-49y women)'),
  cbind('birth_apgar1','Apgar 1'),
  cbind('birth_apgar5','Apgar 5'),
  cbind('birth_low_weight_2500g','Low Birth Weight (<2.5k)'),
  cbind('birth_premature','Premature Birth'),
  cbind('birth_sexratio',"Sex Ratio at Birth")
)




#--------------------------------------------------------------------------------
#--- (7A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,6,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% c(1)) {
    list(x_min = -1, x_max = 1, x_inc = 0.25)
  } else if (i %in% seq(2,3,1)) {
    list(x_min = -1, x_max = 1.5, x_inc = 0.5)
  } else if (i %in% seq(4,6,1)) {
    list(x_min = -0.1, x_max = 0.15, x_inc = 0.05)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  res <- reduced_yearly_imr(var,var_name,df,
                            3,1998,-1000,1000,10,paste0("1_cont_level_",i),
                            weight = "reweightPop",year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,
                              3,1998,vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (7B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,6,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% c(1)) {
    list(x_min = -1, x_max = 1, x_inc = 0.25)
  } else if (i %in% seq(2,3,1)) {
    list(x_min = -1.5, x_max = 3, x_inc = 0.5)
  } else if (i %in% seq(4,6,1)) {
    list(x_min = -0.2, x_max = 0.25, x_inc = 0.05)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                  paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}




#--------------------------------------------------------------------------------
#--- (7C) Implement Rambachan & Roth
#--------------------------------------------------------------------------------
for (i in seq(1,6,1)){
  var <- var_map1[i,1]
  var_name <- var_map1[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,1,1998,-1,2.5,0.25,"remove",
                     weight = "peso_pop",year_cap = 2010,cont = 1,ramb_roth=T) 
}



#--------------------------------------------------------------------------------
#--- (8) IMR
#--------------------------------------------------------------------------------
yearly_folder <- "imr/"

var_map <-  rbind(
  cbind('tx_mi','Infant Mortality Rate'),
  cbind('tx_mi_icsap','Infant Mortality Rate - APC'),
  cbind('tx_mi_nicsap','Infant Mortality Rate - non-APC'),
  cbind('tx_mi_infec','Infant Mortality Rate - Infectious'),
  cbind('tx_mi_resp','Infant Mortality Rate - Respiratory'),
  cbind('tx_mi_perinat','Infant Mortality Rate - Perinatal'),
  cbind('tx_mi_cong','Infant Mortality Rate - Congenital'),
  cbind('tx_mi_ext','Infant Mortality Rate - External'),
  cbind('tx_mi_nut','Infant Mortality Rate - Nutritional'),
  cbind('tx_mi_out','Infant Mortality Rate - Other'),
  cbind('tx_mi_illdef','Infant Mortality Rate - Ill-Defined'),
  cbind('tx_mi_fet','Infant Mortality Rate - Fetal'),
  cbind('tx_mi_24h','Infant Mortality Rate - Within 24h'),
  cbind('tx_mi_27d','Infant Mortality Rate - 1 to 27 days'),
  cbind('tx_mi_ano','Infant Mortality Rate - 27 days to 1 year'),
  cbind('tx_mm',"Maternal Mortality Rate")
)

for (i in seq(1, 16, 1)) {
  var <- var_map[i, 1]
  df[[var]][df$birth_nasc_vivos == 0] <- NA  
}




#--------------------------------------------------------------------------------
#--- (8A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,16,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% seq(1,16,1)) {
    list(x_min = -15, x_max = 10, x_inc = 5)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,
                              3,1998,vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (8B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,16,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% seq(1,16,1)) {
    list(x_min = -20, x_max = 20, x_inc = 5)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                  paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}


#--------------------------------------------------------------------------------
#--- (8C) Rambachan & Roth
#--------------------------------------------------------------------------------
#Make Rambachan and Roth plots
for (i in c(1,13,14,15,16)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-15,10,5,"remove",
                     weight = "peso_pop",year_cap = 2010, cont = 1,ramb_roth=T)
}

#--------------------------------------------------------------------------------
#--- (9) Indexes (Should generate this all natively in R)
#--------------------------------------------------------------------------------
index <- data.frame(read.dta13(paste0(DAT, "indexes.dta")))
# merge indexes to main df
all_df <- c("df")

imerge <- function(df){
  df <- df %>% 
    left_join(index, by = c("ano","cod_mun","cod_uf"))
}

for(d in all_df){
  df_merge <- get(d)
  df_merge <- df_merge %>% imerge()
  assign(d,df_merge,envir = .GlobalEnv)
}

yearly_folder <- "indexes/"

var_map <- rbind(
  cbind('access_index','Access and Production of Health Services Index','peso_pop'),
  cbind('access_pc_index','Primary Care Access and Production Index','peso_pop'),
  cbind('access_npc_index','Non-Primary Care Access and Production Index','peso_pop'),
  cbind('input_index','Health Inputs Index','peso_pop'),
  cbind('hr_index','Human Resources Index','peso_pop'),
  cbind('hospital_index','Hospitals Index','peso_pop'),
  cbind('birth_index','Birth Outcomes Index','peso_pop'),
  cbind('imr_index','Infant Mortality Index','peso_pop'),
  cbind('birth_others_index','Other Birth Outcomes Index','peso_pop')
)




#--------------------------------------------------------------------------------
#--- (9A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,9,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% c(seq(1,3,1),7,9)) {
    list(x_min = -1, x_max = 1.75, x_inc = 0.25)
  } else if (i %in% seq(4,6,1)) {
    list(x_min = -1, x_max = 2.5, x_inc = 0.5)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  if(i<4|i>6) {
    res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3, base_year=1998)
  } else {
    res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                              3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3)
  }
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    if(i<4|i>6) {
      res <- reduced_yearly_imr(var,var_name,df,3,1998,vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control, base_year=1998)
    } else {
      res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                                3,vals$x_min,vals$x_max,vals$x_inc,
                                paste0("1_cont_level_",i),weight = "peso_pop",
                                year_cap = 2010, cont = 1, spec=control)
    }
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }

  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (9B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,9,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  vals <- if (i %in% c(seq(1,3,1),7,9)) {
    list(x_min = -2, x_max = 2.5, x_inc = 0.5)
  } else if (i %in% seq(4,6,1)) {
    list(x_min = -4, x_max = 6, x_inc = 1)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    if(i<4|i>6) {
      res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,vals$x_min,vals$x_max,vals$x_inc,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    } else {
      res <- reduced_yearly_ab_imr(var,var_name,
                                   df %>% mutate(above_pre_99_dist_ec29_baseline=0, below_pre_99_dist_ec29_baseline=0),
                                   3,1998,vals$x_min,vals$x_max,vals$x_inc,
                                   paste0("2_ab_level_",i),weight = "peso_pop",
                                   year_cap = 2010, cont = 1, spec=control)
    }
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }

  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}


#--------------------------------------------------------------------------------
#--- (9C) Rambachan & Roth
#--------------------------------------------------------------------------------
#Make Rambachan and Roth plots
for (i in c(1,2,3)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-15,10,5,"remove",
                     weight = "peso_pop",year_cap = 2010, cont = 1,ramb_roth=T)
}


#--------------------------------------------------------------------------------
#--- (10) System
#--------------------------------------------------------------------------------
yearly_folder <- "system/"
  
var_map <- rbind(
  cbind('ams_hospital_pvt_pcapita','N. of Private Hospitals (per capita*1000)'),
  cbind('cobertura_plano','Private Insurance Coverage'),
  cbind('tx_sih_in_hosp_total','Hospitalization Inflow rate (pop * 1000)'),
  cbind('tx_sih_in_hosp_icsap','Hospitalization Inflow rate - APC (pop * 1000)'),
  cbind('tx_sih_in_hosp_nicsap','Hospitalization Inflow rate - non-APC (pop * 1000)'),
  cbind('tx_sih_out_hosp_total','Hospitalization Outflow rate (pop * 1000)'),
  cbind('tx_sih_out_hosp_icsap','Hospitalization Outflow rate - APC (pop * 1000)'),
  cbind('tx_sih_out_hosp_nicsap','Hospitalization Outflow rate - non-APC (pop * 1000)')
)

  
#--------------------------------------------------------------------------------
#--- (10A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,8,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% c(1)) {
    list(x_min = -0.06, x_max = 0.06, x_inc = 0.02)
  } else if (i %in% c(2)) {
    list(x_min = -0.15, x_max = 0.15, x_inc = 0.05)
  } else if (i %in% seq(3,8,1)) {
    list(x_min = -10, x_max = 20, x_inc = 2)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  if(i>1) {
    res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3, base_year=1998)
  } else {
    res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                              3,1998,-1000,1000,10,
                              paste0("1_cont_level_",i),weight = "reweightPop",
                              year_cap = 2010, cont = 1, spec=3)
  }
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    if(i>1) {
      res <- reduced_yearly_imr(var,var_name,df,3,1998,vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control, base_year=1998)
    } else {
      res <- reduced_yearly_imr(var,var_name,df %>% mutate(pre_99_dist_ec29_baseline=0),
                                3,vals$x_min,vals$x_max,vals$x_inc,
                                paste0("1_cont_level_",i),weight = "peso_pop",
                                year_cap = 2010, cont = 1, spec=control)
    }
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }

  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (10B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  vals <- if (i %in% c(1)) {
    list(x_min = -0.04, x_max = 0.1, x_inc = 0.02)
  } else if (i %in% c(2)) {
    list(x_min = -0.30, x_max = 0.20, x_inc = 0.05)
  } else if (i %in% seq(3,8,1)) {
    list(x_min = -40, x_max = 30, x_inc = 5)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    if(i>1) {
      res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,vals$x_min,vals$x_max,vals$x_inc,
                                 paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    } else {
      res <- reduced_yearly_ab_imr(var,var_name,
                                   df %>% mutate(above_pre_99_dist_ec29_baseline=0, below_pre_99_dist_ec29_baseline=0),
                                   3,1998,vals$x_min,vals$x_max,vals$x_inc,
                                   paste0("2_ab_level_",i),weight = "peso_pop",
                                   year_cap = 2010, cont = 1, spec=control)
    }
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }

  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}


#--------------------------------------------------------------------------------
#--- (10C) Rambachan & Roth
#--------------------------------------------------------------------------------
#Make Rambachan and Roth plots
for (i in seq(3,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-15,10,5,"remove",
                     weight = "peso_pop",year_cap = 2010, cont = 1,ramb_roth=T)
}

#--------------------------------------------------------------------------------
#--- (11) Adult
#--------------------------------------------------------------------------------
yearly_folder <- "adult/"

var_map <- rbind(
  cbind('tx_sih_maternal2','Maternal Hospitalization Rate (pop 0-1y * 1000)'),
  cbind('tx_sih_adult','Adult Hospitalization Rate (pop 40+y * 1000)'),
  cbind('tx_sih_adult_icsap','Adult Hospitalization Rate - APC (pop 40+y * 1000)'),
  cbind('tx_sih_adult_nicsap','Adult Hospitalization Rate - non-APC (pop 40+y * 1000)'),
  cbind('tx_mm',"Maternal Mortality Rate"),
  cbind('tx_ma5','Adult Mortality Rate (40+ y)'),
  cbind('tx_ma5_icsap','Adult Mortality Rate (40+ y) - APC'),
  cbind('tx_ma5_nicsap','Adult Mortality Rate (40+ y) - non-APC')
)




#--------------------------------------------------------------------------------
#--- (11A) Estimate single spending shock
#--------------------------------------------------------------------------------
for (i in seq(1,8,1)) {
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()
  
  ##Set axis
  vals <- if (i %in% seq(1,4,1)) {
    list(x_min = -100, x_max = 100, x_inc = 20)
  } else if (i %in% seq(5,8,1)) {
    list(x_min = -8, x_max = 4, x_inc = 2)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  res <- reduced_yearly_imr(var,var_name,df,3,1998,-1000,1000,10,
                            paste0("1_cont_level_",i),weight = "reweightPop",
                            year_cap = 2010, cont = 1, spec=3)
  print(res)
  res$con <-5
  output_list[[1]] <- res
  iter <- 2
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_imr(var,var_name,df,
                              3,1998,vals$x_min,vals$x_max,vals$x_inc,
                              paste0("1_cont_level_",i),weight = "peso_pop",
                              year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  table_main <- table_main %>% cbind(table_final)
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10
  ## Robustness plot
  robustPlot(var,combined_df)
}


#--------------------------------------------------------------------------------
#--- (11B) Estimate with above and below
#--------------------------------------------------------------------------------
for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% c(1)) {
    list(x_min = -2500, x_max = 3500, x_inc = 500)
  } else if (i %in% seq(2,4,1)) {
    list(x_min = -300, x_max = 300, x_inc = 50)
  } else if (i %in% seq(5,8,1)) {
    list(x_min = -20, x_max = 20, x_inc = 5)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                  paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}

#--------------------------------------------------------------------------------
#--- (11C) Rambachan & Roth
#--------------------------------------------------------------------------------
#Make Rambachan and Roth plots
for (i in seq(1,8,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-15,10,5,"remove",
                     weight = "peso_pop",year_cap = 2010, cont = 1,ramb_roth=T)
}



#--------------------------------------------------------------------------------
#--- (12A) Estimate with child and other mortality (above and below only)
#--------------------------------------------------------------------------------
yearly_folder <- "cmr/"
var_map <- rbind(
  cbind('tx_mc','1-4y Mortality Rate'),
  cbind('tx_mc_icsap','1-4y Mortality Rate - APC'),
  cbind('tx_mc_nicsap','1-4y Mortality Rate - non-APC'),
  cbind('tx_mc_infec','1-4y Mortality Rate - Infectious'),
  cbind('tx_mc_resp','1-4y Mortality Rate - Respiratory'),
  cbind('tx_mc_cong','1-4y Mortality Rate - Congenital'),
  cbind('tx_mc_ext','1-4y Mortality Rate - External'),
  cbind('tx_mc_nerv', '1-4y Mortality Rate - Nervous'),
  cbind('tx_mc_neop', '1-4y Mortality Rate - Neoplasm'),
  cbind('tx_mc_out','1-4y Mortality Rate - Other'),
  cbind('tx_mc_illdef','1-4y Mortality Rate - Ill-Defined'),
  cbind('tx_mc2','Under 5 Mortality Rate'),
  cbind('tx_mc2_icsap','Under 5 Mortality Rate - APC'),
  cbind('tx_mc2_nicsap','Under 5 Mortality Rate - non-APC'),
  cbind('tx_mc2_infec','Under 5 Mortality Rate - Infectious'),
  cbind('tx_mc2_resp','Under 5 Mortality Rate - Respiratory'),
  cbind('tx_mc2_cong','Under 5 Mortality Rate - Congenital'),
  cbind('tx_mc2_ext','Under 5 Mortality Rate - External'),
  cbind('tx_mc2_out','Under 5 Mortality Rate - Other'),
  cbind('tx_mc2_illdef','Under 5 Mortality Rate - Ill-Defined'),  
  cbind('tx_mf',"Female Mortality Rate (10-49y)"),
  cbind('tx_mm',"Maternal Mortality Rate")
)

for (i in seq(1, 22, 1)) {
  var <- var_map[i, 1]
  df[[var]][df$birth_nasc_vivos == 0] <- NA  
}


for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% seq(1,22,1)) {
    list(x_min = -3, x_max = 2, x_inc = 1)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                  paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}


#--------------------------------------------------------------------------------
#--- (12B) Rambachan & Roth
#--------------------------------------------------------------------------------
#Make Rambachan and Roth plots
for (i in seq(1,22,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  reduced_yearly_imr(var,var_name,df,3,1998,-15,10,5,"remove",
                     weight = "peso_pop",year_cap = 2010, cont = 1,ramb_roth=T)
}

#--------------------------------------------------------------------------------
#--- (13) Estimate with other outcomes (above and below only)
#--------------------------------------------------------------------------------
yearly_folder <- "robust_other/"
var_map <- rbind(cbind('gdp_mun_pcapita','GDP per capita'))

for (i in seq(1,1,1)){
  var <- var_map[i,1]
  var_name <- var_map[i,2]
  print(var_name)
  output_list <- list()

  #set axis values
  vals <- if (i %in% c(1)) {
    list(x_min = -50, x_max = 50, x_inc = 10)
  } else {
    list(x_min = -1000, x_max = 1000, x_inc = 100)
  }

  iter <- 1
  for (control in c(1,2,4,3)) {
    print(control)
    res <- reduced_yearly_ab_imr(var,var_name,df,3,1998,
                                 vals$x_min,vals$x_max,vals$x_inc,
                                  paste0("2_ab_level_",i),weight = "peso_pop",
                                 year_cap = 2010, cont = 1, spec=control)
    print(res)
    res$con <-control 
    output_list[[iter]] <- res
    iter <- iter + 1
  }
  combined_df <- do.call(rbind, output_list)
  combined_df <- combined_df %>%
    mutate(controls = case_when(
      con == 1 ~ "(1) Baseline",
      con == 2 ~ "(2) + Municipal char.",
      con == 3 ~ "(3) + Economic",
      con == 4 ~ "(4) + Spending",
      con == 5 ~ "(5) Reweight",
    ))
  
  table_ab<- table_ab %>% cbind(table_final)
  combined_df$con2<-as.character(combined_df$con)
  combined_df$year<- combined_df$year+combined_df$con/10-0.4
  combined_df$year[combined_df$target=="Below"]<- combined_df$year[combined_df$target=="Below"]+0.4
  ## Robustness plot
  df_nona <- combined_df[!is.na(combined_df$estimates),]
  robustPlotAB(var,df_nona)
}

#--------------------------------------------------------------------------------
#--- (14) Tables output
#--------------------------------------------------------------------------------
output_file <- "regression_tables_raw.xlsx"

write.xlsx2(table_main, file = paste0(TAB,output_file),
            sheetName = "event_study"   ,row.names = F,append = T)
write.xlsx2(table_ab,   file = paste0(TAB,output_file),
            sheetName = "event_study_ab",row.names = F,append = T)