



var_10 <- var_map[,1] 
var_11 <- var_map[,1] 
var_12 <- var_map[,1] 
var_13 <- var_map[,1] 
var_14 <- var_map[,1] 
var_18 <- var_map[,1] 
var_19 <- var_map[,1] 

df_stata <- df %>% select(ano, cod_mun, cod_uf,dist_ec29_baseline, all_of(var_10),all_of(var_11),all_of(var_12),all_of(var_13),
                          all_of(var_14),all_of(var_18),all_of(var_19),all_of(controls))


write.dta(df_stata,"data.dta")

write.table(var_map,"lables.csv")