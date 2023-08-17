reduced_yearly_ab <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_ab),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,dist_ec29_baseline) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  df_reg_a <- df_reg %>% filter(dist_ec29_baseline<=0)
  df_reg_b <- df_reg %>% filter(dist_ec29_baseline>0)
  
  
  # Regressions
  # ------------------------------------
  
  spec <- 3
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary"))
    
  }
  weight_vector <- df_reg_a[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg_a, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(1:13) %>%
    select(term,estimate,std.error) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  
  weight_vector <- df_reg_b[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg_b, weights = weight_vector,exactDOF = T)
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(1:13) %>%
    select(term,estimate,std.error) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  }else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom",
            legend.title = element_blank())
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}


reduced_yearly_ab_imr <- function(outcome,var_name,df,transform,year_filter,y0,yf,ys,name,weight,year_cap,label_size,cont){
  
  
  if(missing(label_size)){
    ylabel <- 8
  }else{
    ylabel <-  label_size
  }
  
  
  df_reg <- df
  
  # outcome variable transformation
  
  if(transform==1){
    # log
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- sapply(df_reg[outcome], function(x) ifelse(x==0,NA,x))
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,log)
    
    
    
  } else if(transform==2){
    # inverse hyperbolic sign
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
    df_reg <- df_reg %>% 
      mutate_at(ln_outcome,asinh)
  } else {
    # level
    ln_outcome <- paste0("ln_",outcome)
    df_reg[ln_outcome] <- df_reg[outcome]
  }
  
  # filtering regression variables
  df_reg <- df_reg %>% 
    select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,iv_a,iv_b,iv_binary,all_of(controls),pop,
           all_of(yeartreat_dummies),all_of(yeartreat_dummies_ab),
           peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_a3,peso_r,peso_m,peso_ha,peso_ha1,peso_ha2,peso_pop,
           finbra_desp_saude_san_pcapita_neighbor,lrf,dist_ec29_baseline) %>% 
    filter(ano>=year_filter)
  
  df_reg <- df_reg[complete.cases(df_reg),]
  df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]
  
  
  df_reg_a <- df_reg %>% filter(dist_ec29_baseline<=0)
  df_reg_b <- df_reg %>% filter(dist_ec29_baseline>0)
  
  
  # Regressions
  # ------------------------------------
  
  spec <- 3
  if (cont == 1){
    spec_reduced<- get(paste0("spec",spec,"_post_y_imr"))
  } else{
    spec_reduced<- get(paste0("spec",spec,"_post_y_binary"))
    
  }
  weight_vector <- df_reg_a[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg_a, weights = weight_vector,exactDOF = T)
  
  table1 <- fit %>% 
    broom::tidy() %>%
    slice(1:13) %>%
    select(term,estimate,std.error) %>%
    mutate(target = "Above",
           year = seq.int(year_filter,2010))
  
  
  
  weight_vector <- df_reg_b[weight] %>% unlist() %>% as.numeric()
  regformula <- as.formula(paste(ln_outcome,spec_reduced))
  fit <- felm(regformula, data = df_reg_b, weights = weight_vector,exactDOF = T)
  
  table2 <- fit %>% 
    broom::tidy() %>%
    slice(1:13) %>%
    select(term,estimate,std.error) %>%
    mutate(target = "Below",
           year = seq.int(year_filter,2010))
  
  table <- rbind(table1,table2)
  
  table <- table %>%
    mutate(estimate = ifelse(substr(term,1,7)=="post_00",0,estimate)) %>% 
    mutate(lb = estimate - 1.96 * std.error,
           ub = estimate + 1.96 * std.error,
           lb2 = estimate - 1.645 * std.error,
           ub2 = estimate + 1.645 * std.error,
           spec = as.character(spec)) %>% 
    mutate(lb_adj = NA,
           ub_adj = NA) %>% 
    mutate(lb_adj = ifelse(lb<y0,y0,lb_adj),
           ub_adj = ifelse(ub>yf,yf,ub_adj)) %>% 
    mutate(lb = ifelse(lb<y0,y0,lb),
           ub = ifelse(ub>yf,yf,ub),
           lb2 = ifelse(lb2<y0,y0,lb2),
           ub2 = ifelse(ub2>yf,yf,ub2))
  
  
  # graphs variation
  
  # if all NA for lb_adj
  if(table %>% filter(!is.na(lb_adj)) %>% nrow() == 0){
    lb_na <- 1
  }else{
    lb_na <- 0
  }
  
  # if all NA for ub_adj
  if(table %>% filter(!is.na(ub_adj)) %>% nrow() == 0){
    ub_na <- 1
  }else{
    ub_na <- 0
  }
  
  
  # graph out of bounds, defines arrow size
  arrowsize <-  (yf - y0)*0.03
  
  # graphs color
  colors <-  c("#ef8a62","#67a9cf")
  
  
  
  if(lb_na==1 & ub_na==1){
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom",
            legend.title = element_blank())
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==0 & ub_na ==1) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
    
  } else if (lb_na==1 & ub_na ==0) {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  } else {
    
    graph <- table %>%
      ggplot(aes(x = year, y = estimate, color = target, group = target))+
      geom_hline(yintercept = 0, color = "red", size = 0.3, alpha = 1, linetype = "dashed") +
      geom_vline(xintercept = 2000, color = "#9e9d9d", size = 0.5, alpha = 1, linetype = "solid") +
      geom_point(size = 1, alpha = 1,shape=0,stroke = 1, position = position_dodge(width=0.1)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = target),color = NA, alpha = 0.1) +
      geom_ribbon(aes(ymin = lb2, ymax = ub2, fill = target),color = NA, alpha = 0.2) +
      scale_x_continuous(breaks = seq(1998,year_cap,1), limits = c(1997.5,year_cap+0.5)) +
      scale_y_continuous(breaks = seq(y0,yf,ys), limits = c(y0,yf), labels = comma) +
      scale_color_manual(values = colors) +
      geom_segment(aes(y = ub_adj - arrowsize, x = year, yend = yf, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      geom_segment(aes(y = lb_adj + arrowsize , x = year, yend = y0, xend = year),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      theme_light() +
      labs(y = var_name,
           x = "Year",
           shape = "Specification") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=ylabel),
            axis.text = element_text(size = 10),
            legend.position="bottom")
    
    
    
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".png"),
           plot = graph,
           device = "png",
           width = 7, height = 5,
           units = "in")
    ggsave(paste0(dir,main_folder,yearly_folder,name,"_",outcome,".pdf"),
           plot = graph,
           device = "pdf",
           width = 7, height = 5,
           units = "in")
    
  }
  
  
  
}
