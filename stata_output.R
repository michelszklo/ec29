



var_10 <- var_map
var_11 <- var_map 
var_12 <- var_map 
var_13 <- var_map 
var_14 <- var_map 
var_15 <- var_map 
var_18 <- var_map 
var_19 <- var_map 
var_25 <- var_map 
var_27 <- var_map 

# df_stata <- df %>% select(ano, cod_mun, cod_uf,dist_ec29_baseline, all_of(var_10),all_of(var_11),all_of(var_12),all_of(var_13),
#                           all_of(var_14),all_of(var_18),all_of(var_19),all_of(controls))
# 

write.dta(df,"data.dta")

var_map <- bind_rows(as.data.frame(var_10),
                 as.data.frame(var_11),
                 as.data.frame(var_12),
                 as.data.frame(var_13),
                 as.data.frame(var_14),
                 as.data.frame(var_15),
                 as.data.frame(var_18),
                 as.data.frame(var_19),
                 as.data.frame(var_25),
                 as.data.frame(var_27)) %>% 
  select(-V3)

write.table(var_map,"labels.csv",sep = ",")