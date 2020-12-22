### A2: Creates survey design object

### Setup

# Load packages and some useful functions
source("setup.R")

# Survey Design
options(survey.lonely.psu = "adjust")  

# Load cleaned data
pof <- readr::read_rds(here::here('data','pof_total.rds')) %>% data.table::setDT()

pof <- pof[, COD_UPA := as.integer(stringr::str_sub(ID_MORADOR,1,9))]

# Recover post-estratification and join with df ---------

post_pop <- data.table::fread(here::here('data-raw','POF 2017 - 2018','post_strat.csv'))

pof <- data.table::merge.data.table(pof,
                                    post_pop[,.(pos_estrato, tot_pop, COD_UPA)],
                                    all.x = TRUE,
                                    by = 'COD_UPA')

# Full sample

# Create survey design ----------------------

pof_design <- survey::svydesign(
  data = pof,
  id = ~COD_UPA,
  weights = ~PESO_FINAL,
  strata = ~ESTRATO_POF,
  nest = TRUE)

# Recover post-estratification ---------

post_pop <- data.frame(
  pos_estrato = unique(pof$pos_estrato), 
  Freq = unique(pof$tot_pop))

pof_design_pos <- survey::postStratify(
  pof_design, ~pos_estrato, post_pop)

# Only RH

# Create survey design ----------------------

pof_rh <- pof[RH==1]

pof_rh_design <- survey::svydesign(
  data = pof_rh,
  id = ~COD_UPA,
  weights = ~PESO_FINAL,
  strata = ~ESTRATO_POF,
  nest = TRUE)

# Recover post-estratification ---------

post_pop_rh <- data.frame(
  pos_estrato = unique(pof_rh$pos_estrato), 
  Freq = unique(pof_rh$tot_pop))

pof_rh_design_pos <- survey::postStratify(
  pof_rh_design, ~pos_estrato, post_pop_rh)

# Save survey design and overwrite main dataset with population totals
readr::write_rds(pof_design_pos,here::here('data','pof_svy.rds'))
readr::write_rds(pof_rh_design_pos,here::here('data','pof_rh_svy.rds'))
readr::write_rds(pof[,!c('COD_UPA','pos_estrato')],here::here('data','pof_total.rds'))

###
pof_design <- readr::read_rds(here::here('data','pof_svy.rds'))

pof_rh %>% group_by(SEXO) %>% summarise(X=svymean(VALOR_RH,pof_rh_design_pos,na.rm=T))

mean(pof$RH)
weighted.mean(pof$RH,pof$PESO_FINAL,na.rm=T)
group_by(SEXO) %>% summarise()
# coefficients of variation
svyratio(~SEXO,~RH,pof_rh_design_pos)
cv(svyratio(~api.stu, ~enroll, jkstrat))

# extracting information from the results
coef(svytotal(~VALOR_RH,pof_design))
vcov(svymean(~api00+api99,jkstrat))
confint(svymean(RM~RH, pof_design))
confint(svymean(~VALOR_RH+RM, pof_rh_design_pos))
confint(svymean(~api00+api00, dclus1), df=degf(dclus1))