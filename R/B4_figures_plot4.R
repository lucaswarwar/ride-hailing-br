### B: Plots data, figures used in the final article
## B4 - Plot 4: RH usership and user composition by location

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()

# Gráfico 4. Taxa de utilização ride-hailing por estrato geográfico e tipo de habitação (A), 
# e proporção de usuários de ride-hailing segundo estrato geográfico (B) e tipo de habitação (C).

## Top panel: usership rate

# Top (A) - Estrato e tipo de casa
plot4a <- pof[,.(RH = mean(RH,na.rm = TRUE)),
              by = .(TIPO_DOM, ESTRATO)] %>% 
  na.omit() %>%  
  tidyr::pivot_longer(names_to = 'MODO',
                      values_to = 'TAXA',
                      cols = 'RH') %>% 
  dplyr::filter(TIPO_DOM != 'Habitação Irregular') %>% 
  dplyr::filter(ESTRATO != 'Interior Rural')

p4a <-
  ggplot(plot4a) +
  geom_path(aes(TAXA,reorder(ESTRATO,TAXA),group=ESTRATO),linetype='dotted') +
  geom_point(aes(TAXA,reorder(ESTRATO,TAXA),fill=TIPO_DOM), shape=21,size=3.5) +
  scale_fill_aop() +
  theme_minimal() +
  scale_x_continuous(limits = c(0,.125),labels=scales::percent) +
  labs(x = 'Taxa de Utilização',y='', fill='') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=8),
        axis.text = element_text(size=8))

## Bottom panel: users composition

# Bottom Left (B) - Estrato

plot4b <- pof %>% 
  janitor::tabyl(ESTRATO) %>% 
  dplyr::mutate(Modo = 'População') %>% 
  dplyr::bind_rows(janitor::tabyl(pof_rh,ESTRATO) %>% 
                     dplyr::as_tibble() %>% 
                     dplyr::mutate(Modo = 'Ride-hailing')) %>% 
  dplyr::filter(ESTRATO != 'Interior Rural')

p4b <-  
  plot4b %>% 
  ggplot(aes(ESTRATO, percent)) +
  geom_col(aes(fill = Modo)) +
  guides(x = guide_axis(n.dodge = 3)) +
  scale_fill_aop() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários") +
  theme(legend.position = 'none',
        axis.title = element_text(size=8),
        axis.text = element_text(size=8)) +
  facet_wrap(~Modo, nrow = 1)

# Bottom Right (C) - Location

plot4c <- pof %>% 
  janitor::tabyl(TIPO_DOM) %>% 
  dplyr::mutate(Modo = 'População') %>% 
  dplyr::bind_rows(janitor::tabyl(pof_rh,TIPO_DOM) %>% 
                     dplyr::as_tibble() %>% 
                     dplyr::mutate(Modo = 'Ride-hailing')) %>% 
  dplyr::filter(TIPO_DOM != 'Habitação Irregular')
  
p4c <-  
  plot4c %>% 
  ggplot(aes(TIPO_DOM, percent)) +
  geom_col(aes(fill = Modo)) +
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

p<-p4a/(p4b|p4c)
p + plot_annotation(tag_levels = 'A')


ggsave(here::here('figures','plot4.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot4.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot4.svg'), dpi = 300, height = 16,width = 16,units = 'cm')

rm(list = ls())