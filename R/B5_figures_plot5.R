### B: Plots data, figures used in the final article
## B5 - Plot 5: RH usership by income and user age by metro area

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()
pof_svy <- readr::read_rds(here::here('data','pof_rh_svy.rds'))

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

se_rm <- function(x){
  
  df <- 
    survey::svymean(~IDADE,subset(pof_svy,RM==x & SEXO == 'Homem'),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(RM = as.character(x), SEXO = 'Homem') %>% 
    dplyr::bind_rows(survey::svymean(~IDADE,subset(pof_svy,RM==x & SEXO == 'Mulher'),na.rm=T) %>% 
                       dplyr::as_tibble() %>% 
                       dplyr::mutate(RM = as.character(x), SEXO = 'Mulher'))
  return(df)
}

plot5b <- purrr::map(.x=cidades,.f = se_rm) %>% data.table::rbindlist()

plot5b$RM <- factor(plot5b$RM ,
                    levels= c("Manaus","Campo Grande",'Fortaleza',"São Paulo",
                              "Recife","Belo Horizonte","Brasília", "Porto Alegre",
                              
                              "Salvador", "Rio de Janeiro"))
p5b<-
  ggplot(plot5b) +
  geom_linerange(aes(mean,RM,xmin = mean - IDADE,xmax = mean + IDADE, 
                     color = SEXO), position = position_dodge(width = 0.5)) +
  geom_point(aes(mean,RM, fill = SEXO),shape=21,size=3, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  scale_fill_aop() +
  scale_colour_aop() +
  #scale_x_continuous(limits = c(-0.01,0.2),labels = scales::percent) +
  labs(x = 'Idade média dos usuários',y='',fill = '',color = '') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())

# Plot composition and save

library(patchwork)

p<-p5a|p5b
p + plot_annotation(tag_levels = 'A')


ggsave(here::here('figures','plot5.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot5.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot5.svg'), dpi = 300, height = 16,width = 16,units = 'cm')

rm(list = ls())
svymean(~ANOS_ESTUDO,pof_rh_svy,na.rm=T)
