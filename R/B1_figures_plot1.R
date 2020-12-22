### B: Plots data, figures used in the final article
## B1 - Plot 1: Usership rate and composition of users by income for all types of transport

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()

# Gráfico 1. Taxa de utilização por quintil de renda (A) e proporção dos usuários de por decil de renda (B) por modo de transporte.

# Top (A)
plot1a <- pof[,.(RH = sum(RH,na.rm = TRUE)/.N,
                 TP = sum(TP,na.rm = TRUE)/.N,
                 TX = sum(TX,na.rm = TRUE)/.N,
                 PR = sum(PR,na.rm = TRUE)/.N),
              by = .(QUINTIL)] %>% 
  na.omit() %>%  
  tidyr::pivot_longer(names_to = 'MODO',
                      values_to = 'TAXA',
                      cols = c('RH','TP','TX','PR'))

plot1a <- data.table::setDT(plot1a)[, MODO := data.table::fcase(
  MODO == 'RH', 'Ride-hailing',
  MODO == 'TP', 'Transporte Público',
  MODO == 'TX', 'Táxi',
  MODO == 'PR', 'Transporte Privado')]

plot1a$Modo <- factor(
  plot1a$Modo, 
  levels = c('Transporte Público', 'Táxi','Transporte Privado', 'Ride-hailing'))

p1<-
ggplot(plot1a) +
  geom_point(aes(TAXA,MODO,fill=factor(QUINTIL)), shape=21,size=4.5) +
  scale_fill_aop(palette = 'blue_red',reverse = FALSE) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Taxa de Utilização',y='', fill = 'Quintil de Renda') +
  theme(legend.position = 'top')

# Bottom (B)

df <- pof %>% 
  dplyr::filter(RH == 1) %>% 
  dplyr::mutate(total = n_distinct(ID_MORADOR)) %>% 
  dplyr::group_by(DECIL) %>% 
  dplyr::summarise(SHARE = mean(n()/total)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(MODO = 'Ride-hailing')

df2 <- pof %>% 
  dplyr::filter(TP == 1) %>% 
  dplyr::mutate(total = n_distinct(ID_MORADOR)) %>% 
  dplyr::group_by(DECIL) %>% 
  dplyr::summarise(SHARE = mean(n()/total)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(MODO = 'Transporte Público')

df3 <- pof %>% 
  dplyr::filter(TX == 1) %>% 
  dplyr::mutate(total = n_distinct(ID_MORADOR)) %>% 
  dplyr::group_by(DECIL) %>% 
  dplyr::summarise(SHARE = mean(n()/total)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(MODO = 'Táxi')

df4 <- pof %>% 
  dplyr::filter(PR == 1) %>% 
  dplyr::mutate(total = n_distinct(ID_MORADOR)) %>% 
  dplyr::group_by(DECIL) %>% 
  dplyr::summarise(SHARE = mean(n()/total)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(MODO = 'Transporte Privado')

plot1b <- dplyr::bind_rows(df,df2,df3,df4)
rm(df,df2,df3,df4)

plot1b$DECIL <- factor(plot1b$DECIL,
                       levels = c('10','9','8',
                                  '7','6','5',
                                  '4','3','2','1'))

plot1b$MODO <- factor(plot1b$MODO, levels = c('Transporte Público', 'Táxi','Transporte Privado', 'Ride-hailing'))

p2<-
  plot1b %>% na.omit() %>% 
  ggplot() +
  geom_col(aes(x=SHARE, y=MODO,fill=factor(DECIL)), color='white') +
  scale_fill_aop(palette = 'blue_red', reverse = FALSE) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(x = '% dos Usuários',y='', fill = 'Decil de Renda') +
  theme(legend.position = 'top')

# Plot Composition and save

library(patchwork)

p<-p1/p2
p + plot_annotation(tag_levels = 'A')
p

ggsave(here::here('figures','plot1.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot1.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot1.svg'), dpi = 300, height = 16,width = 16,units = 'cm')

rm(list = ls())
