## B7 - Plot 5: frequency of use and cost by metro area

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()
pof_svy <- readr::read_rds(here::here('data','pof_svy.rds'))

# Gráfico 5: Proporção dos usuários de ride-hailing por quintil de renda (A) e 
# idade média dos dos usuários de ride-hailing (B) das dez regiões metropolitanas com mais usuários.

cidades <- c('São Paulo','Brasília' ,'Rio de Janeiro' , 'Fortaleza' ,
             'Belo Horizonte','Porto Alegre', 'Salvador' ,'Recife'   ,
             'Campo Grande','Manaus')

# Left panel (A): user frequency

se_rm <- function(x){
  
  df <- 
    survey::svymean(~FREQ_RH,subset(pof_svy,RM==x & ESTRATO == 'Capital'),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(RM = as.character(x), ESTRATO = 'Capital') %>% 
    dplyr::bind_rows(survey::svymean(~FREQ_RH,subset(pof_svy,RM==x & ESTRATO == 'RM da Capital'),na.rm=T) %>% 
                       dplyr::as_tibble() %>% 
                       dplyr::mutate(RM = as.character(x), ESTRATO = 'RM da Capital'))
  return(df)
}

plot7a <- purrr::map(.x=cidades,.f = se_rm) %>% data.table::rbindlist()

plot7a$RM <- factor(plot7a$RM ,
                    levels= c("Recife","Salvador","Manaus",'Fortaleza',
                              "Rio de Janeiro","Campo Grande",
                              "Belo Horizonte","Brasília","Porto Alegre","São Paulo"))
p7a<-
  ggplot(plot7a) +
  geom_linerange(aes(mean,RM,xmin = mean - FREQ_RH,xmax = mean + FREQ_RH, 
                     color = ESTRATO), position = position_dodge(width = 0.5)) +
  geom_point(aes(mean,RM, fill = ESTRATO),shape=21,size=3, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  scale_fill_aop() +
  scale_colour_aop() +
  #scale_x_continuous(limits = c(-0.01,0.2),labels = scales::percent) +
  labs(x = 'Nº Médio de viagens por mês',y='',fill = '',color = '') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())

# Right panel (B): users age by metro area

se_rm2 <- function(x){
  
  df <- 
    survey::svymean(~VALOR_RH,subset(pof_svy,RM==x & ESTRATO == 'Capital'),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(RM = as.character(x), ESTRATO = 'Capital') %>% 
    dplyr::bind_rows(survey::svymean(~VALOR_RH,subset(pof_svy,RM==x & ESTRATO == 'RM da Capital'),na.rm=T) %>% 
                       dplyr::as_tibble() %>% 
                       dplyr::mutate(RM = as.character(x), ESTRATO = 'RM da Capital'))
  return(df)
}

plot7b <- purrr::map(.x=cidades,.f = se_rm2) %>% data.table::rbindlist()

plot7b$RM <- factor(plot7b$RM ,
                    levels= c('Fortaleza',"Manaus","Campo Grande",
                              "Porto Alegre","Belo Horizonte","Recife","Salvador",
                              "Brasília","São Paulo","Rio de Janeiro"))
p7b<-
  ggplot(plot7b) +
  geom_linerange(aes(mean,RM,xmin = mean - VALOR_RH,xmax = mean + VALOR_RH, 
                     color = ESTRATO), position = position_dodge(width = 0.5)) +
  geom_point(aes(mean,RM, fill = ESTRATO),shape=21,size=3, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  scale_fill_aop() +
  scale_colour_aop() +
  scale_x_continuous(limits = c(10,40)) +
  labs(x = 'Custo Médio da viagem (R$)',y='',fill = '',color = '') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())

# Plot composition and save

library(patchwork)

p<-p7a|p7b
p + plot_annotation(tag_levels = 'A')

ggsave(here::here('figures','plot7.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot7.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot7.svg'), dpi = 300, height = 16,width = 16,units = 'cm')

rm(list = ls())
