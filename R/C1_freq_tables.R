## C1 - COntingency tables

### Setup

# Load packages and some useful functions
source("setup.R")

### Recover dataset ###

pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()
pof_rh <- readr::read_rds(here::here('data','pof_rh.rds')) %>% data.table::setDT()

# Table 1
tabyl(pof,AUTO)
tabyl(pof_rh,AUTO)
tabyl(pof,AUTO,SEXO)
tabyl(pof_rh,AUTO,SEXO)

modelsummary::datasummary_skim(pof,output = 'markdown')
modelsummary::datasummary_skim(pof_rh,output = 'markdown')


pof[,.(FREQ_RH = mean(FREQ_RH,na.rm = T),
       FREQ_TX = mean(FREQ_TX,na.rm = T),
       VALOR_RH = mean(VALOR_RH,na.rm = T),
       VALOR_TX = mean(VALOR_TX,na.rm = T))]
