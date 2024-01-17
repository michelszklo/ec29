transform <- 3
year_filter <- 1998
i <- 6
var <- var_map[i,1]
outcome <- var


df_reg <- df %>% 
  mutate(mandate1 = ifelse(ano>=2000 & ano<=2004,1,0),
         term1 = ifelse(second_term==0,1,0)) 

# 
# df_reg <- df %>% 
#   mutate(dummy_term=0) %>% 
#   mutate(dummy_term = ifelse(ano>2000 & ano<=2004 & second_term==0,1,dummy_term)) %>% 
#   mutate(dummy_term2=0) %>% 
#   mutate(dummy_term2 = ifelse(ano>2004 & second_term==0,1,dummy_term2))

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
  select(ano, cod_mun,mun_name,cod_uf,uf_y_fe,all_of(ln_outcome),iv,all_of(controls),pop,
         peso_eq,peso_b,peso_a,peso_a1,peso_a2,peso_r,peso_m,
         finbra_desp_saude_san_pcapita_neighbor,lrf,mandate1,term1) %>% 
  filter(ano>=year_filter)

df_reg <- df_reg[complete.cases(df_reg),]
df_reg <- df_reg[complete.cases(df_reg[,ln_outcome]),]


# df_reg <- df_reg %>% 
#   mutate(iv_term = iv * dummy_term) %>% 
#   mutate(iv_term2 = iv * dummy_term2)

df_reg <- df_reg %>% 
  mutate(iv_term = iv * term1,
         iv_term_mandate = iv * term1 * mandate1,
         iv_mandate = iv * mandate1,
         term_mandate = term1 * mandate1)

spec3_alt <- paste(" ~ ","iv + iv_term + iv_mandate + iv_term_mandate + term_mandate + term1 + mandate1"," + ", paste(c(baseline_controls,tvarying_controls), collapse = " + ")," | cod_mun + uf_y_fe | 0 | cod_mun")


regformula <- as.formula(paste(ln_outcome,spec3_alt))


fit <- felm(regformula, data = df_reg,exactDOF = T)
table <- fit %>% broom::tidy() %>% 
  mutate(estimate = round(estimate,3))


