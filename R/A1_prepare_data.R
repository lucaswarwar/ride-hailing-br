### A1: load raw POF data and cleans it, assembles datasets used in the project

### Setup

# Load packages and some useful functions
source("setup.R")

######## 1. Dowload and Clean data ----------------------------------------------------------

#####################
### POF 2017-2018 ###
#####################


# Despesa Individual ----------------

# Consumption data for every product and person in POF 

despesa_individual <- readr::read_rds(here::here('data-raw','POF 2017 - 2018','DESPESA_INDIVIDUAL.rds'))

# Select main columns
despesa_individual <- despesa_individual %>% dplyr::select(UF,
                                                           ESTRATO_POF,
                                                           COD_UPA,
                                                           NUM_DOM,
                                                           NUM_UC,
                                                           COD_INFORMANTE,
                                                           QUADRO,
                                                           COD_ITEM = V9001,
                                                           VALOR = V8000_DEFLA,
                                                           FATOR_ANUALIZACAO,
                                                           PESO_FINAL,
                                                           RENDA_TOTAL)

# Creates unique id's for household, family and person
despesa_individual <- data.table::setDT(despesa_individual)[,ID_DOM := paste(COD_UPA, NUM_DOM, sep = "")]
despesa_individual <- despesa_individual[,ID_FAMILIA := paste(COD_UPA, NUM_DOM, NUM_UC, sep = "")]
despesa_individual <- despesa_individual[,ID_MORADOR := paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")]

# Filter consumption with transport and select columns
despesa_individual <- despesa_individual[QUADRO == "23",.(ID_MORADOR,ID_FAMILIA,ID_DOM,UF,ESTRATO_POF,RENDA_TOTAL,
                                                          COD_ITEM, VALOR,FATOR_ANUALIZACAO, PESO_FINAL)]

# Set key for merge with other data
data.table::setkey(despesa_individual, ID_MORADOR)

# Moradores ------------------------------

# Personal characteristcs from individuals

moradores <- readr::read_rds(here::here('data-raw','POF 2017 - 2018','MORADOR.rds'))

moradores <- moradores %>% dplyr::select(COD_UPA, 
                                         NUM_DOM, 
                                         NUM_UC,
                                         COD_INFORMANTE, 
                                         IDADE = V0403,
                                         SEXO = V0404, 
                                         COR = V0405,
                                         TRABALHO = V0407,
                                         ANOS_ESTUDO) 

# Creates unique id, select columns and set key
moradores <- data.table::setDT(moradores)[,ID_MORADOR := paste(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "")]
moradores <- moradores[,.(ID_MORADOR,IDADE,SEXO,COR, TRABALHO, ANOS_ESTUDO)]
data.table::setkey(moradores,key = 'ID_MORADOR')

# Merge consumption records with individual characteristcs
pof <- data.table::merge.data.table(despesa_individual,
                                    moradores,
                                    all.x = TRUE,
                                    by = 'ID_MORADOR')

# Domicílio ------------------------------

# Info on the type of household

domicilio <- readr::read_rds(here::here('data-raw','POF 2017 - 2018','DOMICILIO.rds'))

domicilio <- domicilio %>% dplyr::select(COD_UPA,
                                         NUM_DOM,
                                         TIPO_DOM = V0201)

domicilio <- data.table::setDT(domicilio)[,ID_DOM := paste(COD_UPA, NUM_DOM, sep = "")]

pof <- data.table::merge.data.table(pof,
                                    domicilio[,.(ID_DOM,TIPO_DOM)],
                                    all.x = TRUE,
                                    by = 'ID_DOM')

data.table::setkey(pof, ID_MORADOR)

# Inventario ----------------------

# Durable goods owned by the household 

inventario <- readr::read_rds(here::here('data-raw','POF 2017 - 2018','INVENTARIO.rds'))

inventario <- inventario %>% dplyr::select(COD_UPA,
                                           NUM_DOM,
                                           NUM_UC,
                                           QUADRO,
                                           COD_ITEM = V9001,
                                           QTDE = V9005)

inventario <- data.table::setDT(inventario)[,ID_FAMILIA := paste(COD_UPA, NUM_DOM, NUM_UC, sep = "")]

# Filter ownership of cars, motorcycles and bikes
inventario <- inventario[COD_ITEM %in% c(1403001,1403101,1403201)]

# Recode products
inventario <- inventario[, COD_ITEM := data.table::fcase(
  COD_ITEM == 1403001, 'CARRO',
  COD_ITEM == 1403101, 'MOTO',
  COD_ITEM == 1403201, 'BICICLETA')]

# Widens data, rows of ownership to columns of goods
inventario <- inventario %>% 
  tidyr::pivot_wider(names_from = 'COD_ITEM',
                     values_from = 'QTDE',
                     values_fill = NULL) %>% 
  data.table::setDT()

# Final merge
pof <- data.table::merge.data.table(pof,
                                    inventario[,.(ID_FAMILIA,CARRO,MOTO,BICICLETA)],
                                    all.x = TRUE,
                                    by = 'ID_FAMILIA')

data.table::setkey(pof, ID_MORADOR)

######## 2. Recode data -------------------------------------------------------------------

pof$UF <- dplyr::recode(pof$UF,
                        "11" =  "RO", "12" =  "AC", "13" =  "AM",
                        "14" =  "RR", "15" =  "PA", "16" =  "AP",
                        "17" =  "TO", "21" =  "MA", "22" =  "PI",
                        "23" =  "CE", "24" =  "RN", "25" =  "PB",
                        "26" =  "PE", "27" =  "AL", "28" =  "SE",
                        "29" =  "BA", "31" =  "MG", "32" =  "ES",
                        "33" =  "RJ", "35" =  "SP", "41" =  "PR",
                        "42" =  "SC", "43" =  "RS", "50" =  "MS",
                        "51" =  "MT", "52" =  "GO", "53" =  "DF"
)

# Identifies the classification of the city
pof <- pof[, ESTRATO := data.table::fcase(
  ESTRATO_POF == 1101 | ESTRATO_POF == 1102 | ESTRATO_POF == 1201 |
    ESTRATO_POF >= 1301 & ESTRATO_POF <= 1306 | ESTRATO_POF == 1401 & ESTRATO_POF == 1402 |
    ESTRATO_POF >= 1501 & ESTRATO_POF <= 1503 | ESTRATO_POF >= 1601 & ESTRATO_POF <= 1602 |
    ESTRATO_POF == 1701 | ESTRATO_POF >= 2101 & ESTRATO_POF <= 2103 |
    ESTRATO_POF >= 2201 & ESTRATO_POF <= 2203 | ESTRATO_POF >= 2301 & ESTRATO_POF <= 2306 |
    ESTRATO_POF >= 2401 & ESTRATO_POF <= 2402 | ESTRATO_POF >= 2501 & ESTRATO_POF <= 2503 |
    ESTRATO_POF >= 2601 & ESTRATO_POF <= 2603 | ESTRATO_POF >= 2701 & ESTRATO_POF <= 2703 |
    ESTRATO_POF >= 2801 & ESTRATO_POF <= 2802 | ESTRATO_POF >= 2901 & ESTRATO_POF <= 2906 |
    ESTRATO_POF >= 3101 & ESTRATO_POF <= 3106 | ESTRATO_POF >= 3201 & ESTRATO_POF <= 3202 |
    ESTRATO_POF >= 3301 & ESTRATO_POF <= 3309 | ESTRATO_POF >= 3501 & ESTRATO_POF <= 3509 |
    ESTRATO_POF >= 4101 & ESTRATO_POF <= 4105 | ESTRATO_POF >= 4201 & ESTRATO_POF <= 4202 |
    ESTRATO_POF >= 4301 & ESTRATO_POF <= 4306 | ESTRATO_POF >= 5001 & ESTRATO_POF <= 5003 |
    ESTRATO_POF >= 5101 & ESTRATO_POF <= 5102 | ESTRATO_POF >= 5201 & ESTRATO_POF <= 5203 |
    ESTRATO_POF >= 5301 & ESTRATO_POF <= 5306 , "Capital",
  ESTRATO_POF == 1307 | ESTRATO_POF >= 1504 & ESTRATO_POF <= 1505 | ESTRATO_POF == 1603 |
    ESTRATO_POF == 2104 | ESTRATO_POF >= 2307 & ESTRATO_POF <= 2309 | ESTRATO_POF == 2403 |
    ESTRATO_POF >= 2504 & ESTRATO_POF <= 2505 | ESTRATO_POF >= 2604 & ESTRATO_POF <= 2606 |
    ESTRATO_POF == 2704 | ESTRATO_POF == 2803 |
    ESTRATO_POF >= 2907 & ESTRATO_POF <= 2909 | ESTRATO_POF >= 3107 & ESTRATO_POF <= 3109 |
    ESTRATO_POF >= 3203 & ESTRATO_POF <= 3205 | ESTRATO_POF >= 3310 & ESTRATO_POF <= 3318 |
    ESTRATO_POF >= 3510 & ESTRATO_POF <= 3515 | ESTRATO_POF >= 4106 & ESTRATO_POF <= 4108 |
    ESTRATO_POF >= 4203 & ESTRATO_POF <= 4204 | ESTRATO_POF >= 4307 & ESTRATO_POF <= 4309 |
    ESTRATO_POF == 5103 | ESTRATO_POF >= 5204 & ESTRATO_POF <= 5206 , 'RM da Capital',
  ESTRATO_POF == 1103 | ESTRATO_POF == 1107 | ESTRATO_POF == 1202 |
    ESTRATO_POF >= 1308 & ESTRATO_POF <= 1310 | ESTRATO_POF == 1403 |
    ESTRATO_POF >= 1506 & ESTRATO_POF <= 1511 | ESTRATO_POF == 1604 |
    ESTRATO_POF >= 1702 & ESTRATO_POF <= 1705 | ESTRATO_POF >= 2105 & ESTRATO_POF <= 2113 |
    ESTRATO_POF >= 2204 & ESTRATO_POF <= 2209 | ESTRATO_POF >= 2310 & ESTRATO_POF <= 2320 |
    ESTRATO_POF >= 2404 & ESTRATO_POF <= 2408 | ESTRATO_POF >= 2506 & ESTRATO_POF <= 2511 |
    ESTRATO_POF >= 2607 & ESTRATO_POF <= 2615 | ESTRATO_POF >= 2705 & ESTRATO_POF <= 2708 |
    ESTRATO_POF >= 2804 & ESTRATO_POF <= 2806 | ESTRATO_POF >= 2910 & ESTRATO_POF <= 2925 |
    ESTRATO_POF >= 3110 & ESTRATO_POF <= 3130 | ESTRATO_POF >= 3206 & ESTRATO_POF <= 3211 |
    ESTRATO_POF >= 3319 & ESTRATO_POF <= 3330 | ESTRATO_POF >= 3516 & ESTRATO_POF <= 3536 |
    ESTRATO_POF >= 4109 & ESTRATO_POF <= 4124 | ESTRATO_POF >= 4205 & ESTRATO_POF <= 4217 |
    ESTRATO_POF >= 4310 & ESTRATO_POF <= 4324 | ESTRATO_POF >= 5004 & ESTRATO_POF <= 5009 |
    ESTRATO_POF >= 5104 & ESTRATO_POF <= 5112 | 
    ESTRATO_POF >= 5207 & ESTRATO_POF <= 5217 , "Interior Urbano",
  default = "Interior Rural")]

# Identifies City (when Metro Area)
pof <- pof[, RM := data.table::fcase( 
  ESTRATO == "Interior Rural" , "Zona Rural",
  ESTRATO != "Interior Urbano" & UF == "SP" , "São Paulo",
  ESTRATO != "Interior Urbano" & UF == "RJ" , "Rio de Janeiro",
  ESTRATO != "Interior Urbano" & UF == "PR" , "Curitiba",
  ESTRATO != "Interior Urbano" & UF == "RS" , "Porto Alegre",
  ESTRATO != "Interior Urbano" & UF == "BA" , "Salvador",
  ESTRATO != "Interior Urbano" & UF == "PE" , "Recife",
  ESTRATO != "Interior Urbano" & UF == "CE" , "Fortaleza",
  ESTRATO != "Interior Urbano" & UF == "PA" , "Belém",
  ESTRATO != "Interior Urbano" & UF == "MG" , "Belo Horizonte",
  ESTRATO != "Interior Urbano" & UF == "DF" , "Brasília",
  ESTRATO != "Interior Urbano" & UF == "SC" , "Florianópolis",
  ESTRATO != "Interior Urbano" & UF == "ES" , "Vitória",
  ESTRATO != "Interior Urbano" & UF == "MT" , "Cuiabá",
  ESTRATO != "Interior Urbano" & UF == "MS" , "Campo Grande",
  ESTRATO != "Interior Urbano" & UF == "GO" , "Goiânia",
  ESTRATO != "Interior Urbano" & UF == "AM" , "Manaus",
  ESTRATO != "Interior Urbano" & UF == "MA" , "São Luís",
  ESTRATO != "Interior Urbano" & UF == "AL" , "Maceió",
  default =  "Brasil Urbano")]

# Age intervals
pof <- pof[, FAIXA_ETARIA := data.table::fcase(
  IDADE < 15       , "0-14",
  IDADE %in% 15:24 , "15-24",
  IDADE %in% 25:34 , "25-34", 
  IDADE %in% 35:44 , "35-44",
  IDADE %in% 45:54 , "45-54",
  IDADE %in% 55:64 , "55-64", 
  IDADE > 64 , "65+")]

# Sex, WORK and race
pof <- pof[, SEXO := ifelse(SEXO == 1, "Homem", "Mulher")]
pof <- pof[, TRABALHO := ifelse(TRABALHO == 1, "Sim", "Não")]

pof <- pof[, COR := data.table::fcase(
  COR == 1 , "Branca",
  COR == 2 , "Preta",
  COR == 3 , 'Amarela',
  COR == 4 , "Parda",
  COR == 5 , 'Indígena',
  default = "Outra")]

#Type of household
pof <- pof[, TIPO_DOM := data.table::fcase(
  TIPO_DOM == 1 , 'Casa',
  TIPO_DOM == 2 , 'Apartamento',
  default = 'Habitação Irregular')]

# Identifies Mode of transport
pof <- pof[, MODO := data.table::fcase(
  COD_ITEM <= 2300401 | COD_ITEM >= 2300701 & COD_ITEM <= 2300901 |
    COD_ITEM >= 2301101 & COD_ITEM <= 2301301 | 
    COD_ITEM >= 2302301 & COD_ITEM <= 2303001 | 
    COD_ITEM >= 4100201 & COD_ITEM <= 4100501 | 
    COD_ITEM >= 4100901 & COD_ITEM <= 4101001 , "Transporte Público",
  COD_ITEM >= 2300401 & COD_ITEM <= 2300404 |
    COD_ITEM >= 4100601 & COD_ITEM <= 4100605 , 'Transporte Alternativo',
  COD_ITEM >= 2300501 & COD_ITEM <= 2300602 |
    COD_ITEM >= 4100701 & COD_ITEM <= 4100801 , "Táxi",
  COD_ITEM == 2303101 | COD_ITEM == 2303102 |
    COD_ITEM == 4106401 , "Ride-hailing", 
  default = "Transporte Privado")]

# Measures per capita income by household
pof <- pof %>%  
  dplyr::group_by(ID_FAMILIA) %>% 
  dplyr::mutate(RENDA_PC = RENDA_TOTAL / dplyr::n_distinct(ID_MORADOR)) %>% 
  dplyr::ungroup() %>% 
  data.table::setDT()

# Calculates income deciles and quintiles
pof[, 
    DECIL_RENDA := cut(x = RENDA_PC, breaks = Hmisc::wtd.quantile(
      x = RENDA_PC, weights = PESO_FINAL, probs = 0:10/10,
      type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
      normwt = F, na.rm = T), labels = F, include.lowest = T),
    by = RM]

pof[, 
    QUINTIL_RENDA := cut( x = RENDA_PC, breaks = Hmisc::wtd.quantile(
      x = RENDA_PC, weights = PESO_FINAL, probs = 0:5/5,
      type = c('quantile','(i-1)/(n-1)', 'i/(n+1)','i/n'),
      normwt = F, na.rm = T), labels = F, include.lowest = T),
    by = RM]


pof <- pof[,.(GASTO_AVG = mean(VALOR, na.rm=TRUE),
              GASTO_MENSAL = sum(VALOR*FATOR_ANUALIZACAO, na.rm = TRUE)/12,
              FREQUENCIA = sum(FATOR_ANUALIZACAO)/12),
           by = .(ID_MORADOR,ID_FAMILIA,PESO_FINAL,ESTRATO_POF,UF,ESTRATO,RM,
                  RENDA_TOTAL,RENDA_PC,QUINTIL_RENDA,DECIL_RENDA,
                  IDADE,FAIXA_ETARIA,SEXO,COR,TRABALHO, ANOS_ESTUDO,
                  CARRO,MOTO,BICICLETA,TIPO_DOM, MODO)]

# Widens data, mode of transport rows to column by individual
pof <- pof %>% 
  tidyr::pivot_wider(names_from = 'MODO',
                     values_from = c('GASTO_AVG','GASTO_MENSAL', 'FREQUENCIA')) %>% 
  data.table::setDT()

# Create dummy variables for type of transport and car ownership
pof <- pof[, RH := ifelse(is.na(`GASTO_AVG_Ride-hailing`),0,1)]
pof <- pof[, TP := ifelse(is.na(`GASTO_AVG_Transporte Público`),0,1)]
pof <- pof[, TX := ifelse(is.na(`GASTO_AVG_Táxi`),0,1)]
pof <- pof[, PR := ifelse(is.na(`GASTO_AVG_Transporte Privado`),0,1)]
pof <- pof[, AUTO := ifelse(is.na(CARRO),0,1)]

# Selects and renames columns
pof <- pof %>% dplyr::select(ID_MORADOR, ID_FAMILIA, UF, ESTRATO_POF,ESTRATO, 
                             RM, RENDA_TOTAL, RENDA_PC, PESO_FINAL,
                             QUINTIL = QUINTIL_RENDA, DECIL = DECIL_RENDA, TIPO_DOM,
                             IDADE, FAIXA_ETARIA, SEXO, COR, TRABALHO, ANOS_ESTUDO, 
                             CARRO, MOTO, BICICLETA,AUTO,RH,TP,TX,PR,
                             VALOR_RH = `GASTO_AVG_Ride-hailing`,
                             VALOR_TP = `GASTO_AVG_Transporte Público`,
                             VALOR_TX = `GASTO_AVG_Táxi`,
                             VALOR_PR = `GASTO_AVG_Transporte Privado`,
                             GASTO_RH = `GASTO_MENSAL_Ride-hailing`,
                             GASTO_TP = `GASTO_MENSAL_Transporte Público`,
                             GASTO_TX = `GASTO_MENSAL_Táxi`,
                             GASTO_PR = `GASTO_MENSAL_Transporte Privado`,
                             FREQ_RH = `FREQUENCIA_Ride-hailing`,
                             FREQ_TP = `FREQUENCIA_Transporte Público`,
                             FREQ_TX = `FREQUENCIA_Táxi`,
                             FREQ_PR = `FREQUENCIA_Transporte Privado`) %>% 
  data.table::setDT(key = 'ID_MORADOR')

# Smaller dataset, only RH users
pof_rh <- pof[RH == 1]

# Save files
readr::write_rds(pof,here::here('data', 'pof_total.rds'))
readr::write_rds(pof_rh,here::here('data', 'pof_rh.rds'))