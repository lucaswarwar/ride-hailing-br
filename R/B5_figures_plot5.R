### B: Plots data, figures used in the final article
## B5 - Plot 5: RH usership by income and user age by metro area

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()

# Gráfico 5: Proporção dos usuários de ride-hailing por quintil de renda (A) e 
# idade média dos dos usuários de ride-hailing (B) das dez regiões metropolitanas com mais usuários.

cidades <- c('São Paulo','Brasília' ,'Rio de Janeiro' , 'Fortaleza' ,
             'Belo Horizonte','Porto Alegre', 'Salvador' ,'Recife'   ,
             'Campo Grande','Manaus')

# Left panel (A): usership by income

plot5a <- pof_rh %>% 
  dplyr::group_by(RM) %>% 
  dplyr::mutate(total = n_distinct(ID_MORADOR)) %>% 
  dplyr::group_by(QUINTIL,RM) %>% 
  dplyr::summarise(SHARE = mean(n()/total)) %>% 
  dplyr::filter(RM %in% cidades)

plot5a$RM<-factor(plot5a$RM,
                  levels = c('São Paulo','Brasília' ,'Rio de Janeiro' , 'Fortaleza' ,
                             'Belo Horizonte','Porto Alegre','Salvador' ,'Recife',
                              'Campo Grande', 'Manaus'))
p5a<-
  plot5a %>%  
  ggplot()+
  geom_col(aes(SHARE,reorder(RM,interaction(SHARE,QUINTIL)),fill=as.factor(QUINTIL))) +
  scale_fill_aop(palette = 'blue_red') +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(x = '% dos Usuários',y='', fill = 'Quintil de Renda') +
  theme(legend.position = 'top')

# Right panel (B): users age by metro area

plot5b <- pof_rh[,.(IDADE = mean(IDADE,na.rm = TRUE)),
              by = .(RM,SEXO)] %>% 
  dplyr::filter(RM %in% cidades)

p5b<-
  plot5b %>% 
  ggplot(group=RM) +
  geom_path(aes(IDADE,reorder(RM,IDADE)))+
  geom_point(aes(IDADE,reorder(RM,IDADE),fill=SEXO),
             shape=21,size=3.5) +
  scale_fill_aop() +
  labs(x='Idade média dos usuários',y='',fill='') +
  theme_minimal() +
  theme(legend.position = 'top')

# Plot composition and save

library(patchwork)

p<-p5a|p5b
p + plot_annotation(tag_levels = 'A')


ggsave(here::here('figures','plot5.png'), dpi = 300, height = 6,width = 7.5,units = 'in')
ggsave(here::here('figures','plot5.pdf'), dpi = 300, height = 6,width = 7.5,units = 'in')
ggsave(here::here('figures','plot5.svg'), dpi = 300, height = 6,width = 7.5,units = 'in')

rm(list = ls())
svymean(~ANOS_ESTUDO,pof_rh_svy,na.rm=T)
