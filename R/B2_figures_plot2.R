### B: Plots data, figures used in the final article
## B2 - Plot 2: RH usership and user composition by individual characteristics

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###

pof_svy <- readr::read_rds(here::here('data','pof_svy.rds'))
pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()

# Gráfico 2. Taxa de utilização de ride-hailing por idade (A), sexo (B) e cor (C); 
# e  proporção de usuários de ride-hailing segundo idade (D), sexo (E) e cor (F).
 
## Left panel: usership rate

# Left (A) - Idade
se_idade <- function(x){
  
  df <- 
    survey::svymean(~RH,subset(pof_svy,FAIXA_ETARIA==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(FAIXA_ETARIA = as.character(x))
  return(df)
}

plot2a <- purrr::map(.x=c("35-44","45-54","25-34","15-24","55-64","65+"),.f = se_idade) %>% data.table::rbindlist()

p2a<-
  ggplot(plot2a) +
  geom_linerange(aes(mean,FAIXA_ETARIA,xmin = mean - RH,xmax = mean + RH),
                 color = '#00324a',) +
  geom_point(aes(mean,FAIXA_ETARIA),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(0.01,0.041)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (B) - Sexo
se_sexo <- function(x){
  
  df <- 
    survey::svymean(~RH,subset(pof_svy,SEXO==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(SEXO = as.character(x))
  return(df)
}

plot2b <- purrr::map(.x=c('Homem','Mulher'),.f = se_sexo) %>% data.table::rbindlist()

p2b<-
  ggplot(plot2b) +
  geom_linerange(aes(mean,SEXO,xmin = mean - RH,xmax = mean + RH),
                 color = '#00324a',) +
  geom_point(aes(mean,SEXO),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(0.01,0.041)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (C) - Cor
se_cor <- function(x){
  
  df <- 
    survey::svymean(~RH,subset(pof_svy,COR==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(COR = as.character(x))
  return(df)
}

plot2c <- purrr::map(.x=c("Branca","Preta","Parda"),.f = se_cor) %>% data.table::rbindlist()

p2c<-
  ggplot(plot2c) +
  geom_linerange(aes(mean,COR,xmin = mean - RH,xmax = mean + RH),
                 color = '#00324a',) +
  geom_point(aes(mean,COR),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(0.01,0.041), labels = scales::percent) +
  labs(x = 'Taxa de Utilização',y='') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())

## Right panel: users composition

# Left (D) - Idade

plot2d <- pof %>% 
  janitor::tabyl(FAIXA_ETARIA) %>% 
  dplyr::mutate(Modo = 'População') %>% 
  dplyr::bind_rows(janitor::tabyl(pof_rh,FAIXA_ETARIA) %>% 
                     dplyr::as_tibble() %>% 
                     dplyr::mutate(Modo = 'Ride-hailing')) %>% 
  dplyr::filter(FAIXA_ETARIA != '0-14')

p2d <-  
  plot2d %>% 
  ggplot(aes(FAIXA_ETARIA, percent)) +
  geom_col(aes(fill = Modo)) +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_fill_aop() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários") +
  theme(legend.position = 'none',
        axis.title = element_text(size=8),
        axis.text = element_text(size=8)) +
  facet_wrap(~Modo, nrow = 1)

# Left (E) - SEXO

plot2e <- pof %>% 
  janitor::tabyl(SEXO) %>% 
  dplyr::mutate(Modo = 'População') %>% 
  dplyr::bind_rows(janitor::tabyl(pof_rh,SEXO) %>% 
                     dplyr::as_tibble() %>% 
                     dplyr::mutate(Modo = 'Ride-hailing'))

p2e <-  
  plot2e %>% 
  ggplot(aes(SEXO, percent)) +
  geom_col(aes(fill = Modo)) +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_fill_aop() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários") +
  theme(legend.position = 'none',
        axis.title = element_text(size=8),
        axis.text = element_text(size=8)) +
  facet_wrap(~Modo, nrow = 1)

# Left (F) - COR

plot2f <- pof %>% 
  janitor::tabyl(COR) %>% 
  dplyr::mutate(Modo = 'População') %>% 
  dplyr::bind_rows(janitor::tabyl(pof_rh,COR) %>% 
                     dplyr::as_tibble() %>% 
                     dplyr::mutate(Modo = 'Ride-hailing')) %>% 
  dplyr::filter(COR %in% c('Branca','Preta','Parda'))

p2f <-  
  plot2f %>% 
  ggplot(aes(COR, percent)) +
  geom_col(aes(fill = Modo)) +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_fill_aop() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários") +
  theme(legend.position = 'none',
        axis.title = element_text(size=8),
        axis.text = element_text(size=8)) +
  facet_wrap(~Modo, nrow = 1)

# Plot composition and save

library(patchwork)

p <- (p2a/p2b/p2c)|(p2d/p2e/p2f)
p + plot_annotation(tag_levels = 'A')


ggsave(here::here('figures','plot2.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot2.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot2.svg'), dpi = 300, height = 16,width = 16,units = 'cm')

rm(list = ls())
