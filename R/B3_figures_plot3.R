### B: Plots data, figures used in the final article
## B3 - Plot 3: RH usership and user composition by metro area

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###
### 
pof_svy <- readr::read_rds(here::here('data','pof_svy.rds'))
pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()

# Gráfico 3. Taxa de utilização de ride-hailing por estrato geográfico (A) 
# e distribuição dos usuários de ride-hailing entre as principais regiões metropolitanas (B).

## Left panel: usership rate

se_rm <- function(x){
  
  df <- 
    survey::svymean(~RH,subset(pof_svy,RM==x & ESTRATO == 'Capital'),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(RM = as.character(x), ESTRATO = 'Capital') %>% 
    dplyr::bind_rows(survey::svymean(~RH,subset(pof_svy,RM==x & ESTRATO == 'RM da Capital'),na.rm=T) %>% 
                       dplyr::as_tibble() %>% 
                       dplyr::mutate(RM = as.character(x), ESTRATO = 'RM da Capital'))
  return(df)
}
cities<-unique(pof$RM)[-1][-1]

plot3a <- purrr::map(.x=cities,.f = se_rm) %>% data.table::rbindlist()

plot3a$RM <- factor(plot3a$RM ,
                    levels= c("Belém","Curitiba","Maceió","São Luís","Manaus",'Fortaleza',"Brasília",
                              'Florianópolis',"Rio de Janeiro","São Paulo","Vitória","Campo Grande",
                              "Salvador","Recife","Belo Horizonte","Goiânia",   "Cuiabá","Porto Alegre"))

p3<-
ggplot(plot3a) +
  geom_linerange(aes(mean,RM,xmin = mean - RH,xmax = mean + RH, 
                     color = ESTRATO), position = position_dodge(width = 0.5)) +
  geom_point(aes(mean,RM, fill = ESTRATO),shape=21,size=3, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  scale_fill_aop() +
  scale_colour_aop() +
  scale_x_continuous(limits = c(-0.01,0.2),labels = scales::percent) +
  labs(x = 'Taxa de Utilização',y='',fill = '',color = '') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())

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

p<-p3|p3b
p + plot_annotation(tag_levels = 'A')


ggsave(here::here('figures','plot3.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot3.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot3.svg'), dpi = 300, height = 16,width = 16,units = 'cm')

rm(list = ls())