

muncap <- read.csv(paste0(raw,"lista_mun/municipios.csv")) %>% 
  filter(capital==1) %>%
  mutate(cod_mun = as.numeric(substr(as.character(codigo_ibge),1,6))) %>% 
  select(cod_mun) %>% 
  pull()

df_density <- df %>% 
  select(cod_mun, mun_name,ano, pop,tx_sih_in_hosp_total, tx_sih_in_hosp_icsap, tx_sih_in_hosp_nicsap) %>% 
  filter(ano>=1998)


plot <- ggplot(data = df_density %>% filter(ano==2000), aes(x = tx_sih_in_hosp_total)) +
  geom_histogram() + 
  geom_histogram(data = df_density %>% filter(ano==2000) %>% filter(!(cod_mun %in% muncap)),
               aes(x = tx_sih_in_hosp_total))+
  geom_vline(xintercept = 0.15, linetype = "dashed", size = 0.6, color = "grey43") +
  scale_x_continuous(breaks = seq(-100,200,50), limits = c(-5,200)) +

  # scale_color_manual(values = color_graph) +
  # scale_fill_manual(values = color_graph) +
  labs(x = 'Hospitalization Inflow rate (pop * 1000)',
       y = "Density") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))




plot <- df_density %>% filter(ano==2000) %>% filter(!(cod_mun %in% muncap)) %>% 
  # ggplot(aes(x = tx_sih_in_hosp_total, color = ano, fill = ano)) +
  ggplot(aes(x = tx_sih_in_hosp_total)) +
  geom_density(binwidth = 2, alpha = 0.1) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6, color = "grey43") +
  scale_x_continuous(breaks = seq(0,800,100), limits = c(0,800)) +
  # scale_color_manual(values = color_graph) +
  # scale_fill_manual(values = color_graph) +
  labs(x = 'Hospitalization Inflow rate (pop * 1000)',
       y = "Density") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        legend.position="bottom", legend.box = "horizontal") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))



teste <- df_density %>% 
  filter(ano==2000) %>% 
  filter(tx_sih_in_hosp_total>100)

