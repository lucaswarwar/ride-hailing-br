## B7 - Plot 5: frequency of use and cost by metro area

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

# Left panel (A): user frequency

plot7 <- pof_rh[,.(VALOR=mean(VALOR_RH,na.rm=TRUE),
                    FREQ = mean(FREQ_RH,na.rm=TRUE)),
                 by = .(RM,ESTRATO)] %>%  
  dplyr::filter(RM %in% cidades)

p7a<-
  plot7 %>%  
  ggplot(group=RM) +
  geom_path(aes(FREQ,reorder(RM,FREQ)))+
  geom_point(aes(FREQ,reorder(RM,FREQ),fill=ESTRATO),
             shape=21,size=3.5) +
  scale_fill_aop() +
  labs(x='Nº Médio de viagens por mês',y='',fill='') +
  theme_minimal() +
  theme(legend.position = 'top')

# Right panel (B): users age by metro area

p7b<-
  plot7 %>% dplyr::filter(VALOR<50) %>% 
  ggplot(group=RM) +
  geom_path(aes(VALOR,reorder(RM,VALOR)))+
  geom_point(aes(VALOR,reorder(RM,VALOR),fill=ESTRATO),
             shape=21,size=3.5) +
  scale_x_continuous(limits = c(12.5,32.5))+
  scale_fill_aop() +
  labs(x='Custo Médio da viagem (R$)',y='',fill='') +
  theme_minimal() +
  theme(legend.position = 'top')

# Plot composition and save

library(patchwork)

p<-p7a|p7b
p + plot_annotation(tag_levels = 'A')

ggsave(here::here('figures','plot7.png'), dpi = 300, height = 6,width = 7.5,units = 'in')
ggsave(here::here('figures','plot7.pdf'), dpi = 300, height = 6,width = 7.5,units = 'in')
ggsave(here::here('figures','plot7.svg'), dpi = 300, height = 6,width = 7.5,units = 'in')

rm(list = ls())
