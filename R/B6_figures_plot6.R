### B: Plots data, figures used in the final article
## B6 - Plot 6: RH frequency of use and avg cost by individual characteristics

### Setup

# Load packages and some useful functions
source("setup.R")
source("colours.R")

### Recover dataset and survey design ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()
pof_svy <- readr::read_rds(here::here('data','pof_svy.rds'))

survey::svymean(~IDADE,pof_svy,na.rm=T)
# Gráfico 6: Frequência de uso de serviços de ride-hailing por faixa de renda e idade (A), sexo (B) e cor (C) 
# e distribuição do custo da viagem por faixa de renda (D), sexo (E) e cor (F).

# Left (A) - Frequência - Renda

se_quintil_freq <- function(x){
  
  df <- 
  survey::svymean(~FREQ_RH,subset(pof_svy,QUINTIL==x),na.rm=T) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(QUINTIL = as.character(x))
  return(df)
}

plot6a <- purrr::map(.x=seq(1,5,1),.f = se_quintil_freq) %>% data.table::rbindlist()

p6a<-
  ggplot(plot6a) +
  geom_linerange(aes(mean,QUINTIL,xmin = mean - FREQ_RH,xmax = mean + FREQ_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,QUINTIL),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(4.5,10)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (B) - Frequência - Sexo

se_sexo_freq <- function(x){
  
  df <- 
    survey::svymean(~FREQ_RH,subset(pof_svy,SEXO==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(SEXO = as.character(x))
  return(df)
}

plot6b <- purrr::map(.x=c('Homem','Mulher'),.f = se_sexo_freq) %>% data.table::rbindlist()


p6b<-
  ggplot(plot6b) +
  geom_linerange(aes(mean,SEXO,xmin = mean - FREQ_RH,xmax = mean + FREQ_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,SEXO),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(4.5,10)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (C) - Frequência - Idade

se_idade_freq <- function(x){
  
  df <- 
    survey::svymean(~FREQ_RH,subset(pof_svy,FAIXA_ETARIA==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(FAIXA_ETARIA = as.character(x))
  return(df)
}

plot6c <- purrr::map(.x=c("35-44","45-54","25-34","15-24","55-64","65+"),.f = se_idade_freq) %>% data.table::rbindlist()

p6c<-
  ggplot(plot6c) +
  geom_linerange(aes(mean,FAIXA_ETARIA,xmin = mean - FREQ_RH,xmax = mean + FREQ_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,FAIXA_ETARIA),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(4.5,10)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (D) - Frequência - Cor

se_cor_freq <- function(x){
  
  df <- 
    survey::svymean(~FREQ_RH,subset(pof_svy,COR==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(COR = as.character(x))
  return(df)
}

plot6d <- purrr::map(.x=c("Branca","Preta","Parda"),.f = se_cor_freq) %>% data.table::rbindlist()

p6d<-
  ggplot(plot6d) +
  geom_linerange(aes(mean,COR,xmin = mean - FREQ_RH,xmax = mean + FREQ_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,COR),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(4.5,10)) +
  labs(x = 'Nº médio de viagens por mês',y='') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())

# Right (E) - Cost - Renda x Idade

se_quintil_valor <- function(x){
  
  df <- 
    survey::svymean(~VALOR_RH,subset(pof_svy,QUINTIL==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(QUINTIL = as.character(x))
  return(df)
}

plot6e <- purrr::map(.x=seq(1,5,1),.f = se_quintil_valor) %>% data.table::rbindlist()

p6e<-
  ggplot(plot6e) +
  geom_linerange(aes(mean,QUINTIL,xmin = mean - VALOR_RH,xmax = mean + VALOR_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,QUINTIL),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(15,47.5)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (B) - valoruência - Sexo

se_sexo_valor <- function(x){
  
  df <- 
    survey::svymean(~VALOR_RH,subset(pof_svy,SEXO==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(SEXO = as.character(x))
  return(df)
}

plot6f <- purrr::map(.x=c('Homem','Mulher'),.f = se_sexo_valor) %>% data.table::rbindlist()


p6f<-
  ggplot(plot6f) +
  geom_linerange(aes(mean,SEXO,xmin = mean - VALOR_RH,xmax = mean + VALOR_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,SEXO),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(15,47.5)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (C) - valoruência - Idade

se_idade_valor <- function(x){
  
  df <- 
    survey::svymean(~VALOR_RH,subset(pof_svy,FAIXA_ETARIA==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(FAIXA_ETARIA = as.character(x))
  return(df)
}

plot6g <- purrr::map(.x=c("35-44","45-54","25-34","15-24","55-64","65+"),.f = se_idade_valor) %>% data.table::rbindlist()

p6g<-
  ggplot(plot6g) +
  geom_linerange(aes(mean,FAIXA_ETARIA,xmin = mean - VALOR_RH,xmax = mean + VALOR_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,FAIXA_ETARIA),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(15,47.5)) +
  labs(x = '',y='') +
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank())

# Left (D) - valoruência - Cor

se_cor_valor <- function(x){
  
  df <- 
    survey::svymean(~VALOR_RH,subset(pof_svy,COR==x),na.rm=T) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(COR = as.character(x))
  return(df)
}

plot6h <- purrr::map(.x=c("Branca","Preta","Parda"),.f = se_cor_valor) %>% data.table::rbindlist()

p6h<-
  ggplot(plot6h) +
  geom_linerange(aes(mean,COR,xmin = mean - VALOR_RH,xmax = mean + VALOR_RH),
                 color = '#00324a',) +
  geom_point(aes(mean,COR),shape=21,size=3,fill = '#00324a') +
  theme_minimal() +
  scale_x_continuous(limits = c(15,47.5)) +
  labs(x = 'Custo médio da viagem (R$)',y='') +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())


library(patchwork)

p <- (p6a/p6b/p6c/p6d)|(p6e/p6f/p6g/p6h)
p+plot_annotation(tag_levels = 'A')


ggsave(here::here('figures','plot6.png'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot6.pdf'), dpi = 300, height = 16,width = 16,units = 'cm')
ggsave(here::here('figures','plot6.svg'), dpi = 300, height = 16,width = 16,units = 'cm')
rm(list = ls())
