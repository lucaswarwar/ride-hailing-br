### B: Plots data, figures used in the final article
## B3 - Plot 3: RH usership and user composition by metro area

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()

# Gráfico 3. Taxa de utilização de ride-hailing por estrato geográfico (A) 
# e distribuição dos usuários de ride-hailing entre as principais regiões metropolitanas (B).

## Left panel: usership rate

plot3a <- pof[,.(RH = mean(RH,na.rm = TRUE)),
              by = .(RM,ESTRATO)] %>% 
  na.omit() %>%  
  tidyr::pivot_longer(names_to = 'MODO',
                      values_to = 'TAXA',
                      cols = 'RH') %>% 
  dplyr::filter(ESTRATO %in% c('Capital', 'RM da Capital')) %>% 
  dplyr::filter(RM != 'Brasil Urbano')

p3a<- 
  plot3a %>% 
  ggplot(aes(TAXA,reorder(RM,TAXA),group = RM)) +
  geom_path(linetype = 'dotted') +
  geom_point(aes(fill=ESTRATO), shape=21,size=3.5) +
  scale_fill_aop() +
  theme_minimal() +
  scale_x_continuous(limits = c(0,.175),labels = scales::percent) +
  labs(y = '',x='Taxa de Utilização', fill='') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

## Right Panel: users by city

plot3b <- pof_rh %>% 
  janitor::tabyl(RM) %>% 
  dplyr::as_tibble() %>% 
  dplyr::filter(RM %nin% c('Brasil Urbano','Zona Rural'))

p3b<- 
  plot3b %>%                
  ggplot(aes(reorder(RM,percent),percent)) +
  geom_col(fill = '#c88300') +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="% dos usuários") +
  theme(legend.position = 'none',
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()) +
  coord_flip()

# Plot composition and save

library(patchwork)

p<-p3a|p3b
p + plot_annotation(tag_levels = 'A')


ggsave(here::here('figures','plot3.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot3.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot3.svg'), dpi = 300, height = 16,width = 16,units = 'cm')

rm(list = ls())