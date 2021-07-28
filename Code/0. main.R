      ###################################################################
      ## The causal effect of road concessions on road safety outcomes ##
      ###################################################################
                                               ## Pedro Jorge Alves    ##
                                               ## Lucas Emanuel        ##
                                               ## Rafael H. M. Pereira ##
                                               ##########################

#### 0) Read packages---------------------------------------------------------

library(dplyr)        ; library(data.table) ; library(readxl)   ; library(foreign)
library(readr)        ; library(bit64)      ; library(magrittr) ; library(furrr)
library(haven)        ; library(foreign)    ; library(stargazer);
library(lfe)          ; library(sf)         ; library(geobr)    ; library(ggplot2)
library(lwgeom)       ; library(tidyr)      ; library(statar)   ;
library(rgeos)        ; library(sp)         ; library(plyr)     ; library(lubridate)
library(rnaturalearth); library(viridis)    ; library(ggspatial); library(sf)
library(spdep)        ; library(spatialreg)

#### 1) Read Database ------------------------------------------------

rm(list=ls()) 

#### Reading and initial filtering of the original database of accidents

source('./R/1. leitura.R', encoding = 'UTF-8')

#### Concessions Roads database

# Read database of roads
      
rodovias <- readxl::read_excel(path = "./Database/Rodovias_completo.xlsx") %>%  # col_types='text'
  mutate(br = as.numeric(br),km_inicial = as.numeric(km_inicial),
         extensao = as.numeric(extensao),km_final = as.numeric(km_final))

# Add volume traffic data
volume <- readxl::read_excel("L:/# DIRUR #/ASMEQ/bosistas/Lucas/ANTT/data/fluxo/Modelagem 2019_SNV_202001A.xlsx", sheet  = 2)

volume <- dplyr::rename(volume, br = vl_br, uf  = sg_uf,
                 km_inicial = vl_km_inic, km_final = vl_km_fina)
volume <- mutate(volume,
                 VMDa_C = as.numeric(VMDa_C),
                 VMDa_D = as.numeric(VMDa_D),
                 br = as.numeric(br),
                 VMDa =  VMDa_C + VMDa_D)
volume <- select(volume,br,uf,km_inicial,km_final,VMDa,VMDa_C,VMDa_D)

rodovias <- rodovias %>% left_join(volume)

# Concessions database

empresas <- readxl::read_excel(path = "./Database/Empresas_concessao.xlsx")


# Include only Federation unity that has concession
temp <- empresas %>% distinct(uf, Ano_Inicio, .keep_all = T) %>%
  mutate(base = 1) %>% 
  mutate(yr_start = as.numeric(substr(Ano_Inicio,-4,4))) %>% 
  filter(yr_start >= 2007) %>% 
  filter(yr_start <= 2017) %>% select(c(uf,base))

rodovias <- rodovias %>% left_join(temp) %>% 
  filter(base==1) %>% select(-c(base))

rm(temp)

# Applying Concession base on highways
rodovias <- rodovias %>% left_join(empresas) %>%  
  mutate(yr_start = as.numeric(substr(Ano_Inicio,-4,4))) %>% 
           filter(is.na(yr_start) | yr_start <= 2018)


# Remove repeated lines and add sections by extension
rodovias <- rodovias %>%
  distinct(br,uf,km_inicial,km_final, .keep_all = TRUE) %>%
  mutate(concessao = ifelse(is.na(Empresa),0,1),
         rod = ifelse(superficie=="PLA" & !is.na(superfic_1),
                      superfic_1,superficie),
         via_duplicada = ifelse(rod=="DUP",1,0),
         via_pavimentada = ifelse(rod=="PAV",1,0))

# Summarise roads
rodovias <- rodovias %>%
  group_by(br,uf,concessao,yr_start) %>% 
  dplyr::summarise(Ano_Inicio = mean(Ano_Inicio, na.rm = T),
                   Ano_Final = mean(Ano_Final, na.rm = T),
                   Inicio_pedagio = mean(Inicio_pedagio, na.rm = T),
                   min_km = min(km_inicial, na.rm = T),
                   max_km = max(km_final, na.rm = T),
                   extensao = sum(extensao, na.rm = T),
                   via_duplicada = sum(via_duplicada,na.rm = T),
                   via_pavimentada = sum(via_pavimentada,na.r=T),
                   VMDa_C = sum(VMDa_C,na.r=T),
                   VMDa_D = sum(VMDa_D,na.r=T),
                   VMDa =  sum(VMDa,na.r=T)) %>% ungroup() %>% 
  select(c(Ano_Inicio,Inicio_pedagio,yr_start,br,uf,min_km,
           max_km,extensao,via_pavimentada,via_duplicada))


# Manual adjustments
rodovias <- rodovias %>%
  # MT e MG didn't have correct extension
  mutate(min_km = ifelse(br==163 & uf == "MT" & is.na(Inicio_pedagio),855,min_km),
         max_km = ifelse(br==381 & uf == "MG" & is.na(Inicio_pedagio),476.1,max_km)) %>% 
  mutate(primeira_fase = ifelse(yr_start < 2007,1,0),
         extensao = max_km - min_km)






########## 2)  Find if accidents have occurred on concession roads -----------

acidentes_concessoes <- acidentes %>% 
  inner_join(rodovias, by = c("uf"="uf","br"="br")) %>% 
  mutate(km = as.numeric(km)) %>% 
  # Manual alteration of some highways that the command did not solve
  mutate(max_km = ifelse(br==393 & uf == "RJ" & 
                         km<=105.3,105.3,max_km),
       min_km = ifelse(br==393 & uf == "RJ" & 
                         km>= 291.1,291.1,min_km))

# Generate concession variable
setDT(acidentes_concessoes)
acidentes_concessoes[,concessao := ifelse(data.table::between(km,min_km,max_km) &
                                            !is.na(Ano_Inicio),1,0)]

# Generate if roads was concession in first phase
acidentes_concessoes[,primeira_fase := ifelse(data.table::between(km,min_km,max_km) &
                                            !is.na(primeira_fase),1,0)]
acidentes_concessoes[,primeira_fase := ifelse(yr_start >= 2007,0,primeira_fase)]

# Remove these roads
temp <- acidentes_concessoes %>% 
  filter(primeira_fase==1)

acidentes_concessoes <- subset(acidentes_concessoes, !(id %in% temp$id))


acidentes_concessoes <- acidentes_concessoes %>% 
  # Generate a new date variable
  mutate(data_inversa = paste0(ano_ocorrencia,"-",mes_ocorrencia,"-",dia_ocorrencia)) %>% 
  # Rename some variables
  dplyr::rename(grupo_tratado = concessao, ano_acidente = ano_ocorrencia,
                inicio_pedagio = Inicio_pedagio,
                ano_inicio = Ano_Inicio) %>% ungroup()


## Others variables

# Accident by day
acidentes_concessoes[,acidente_pelodia := ifelse(fase_dia=="Amanhecer" |
                                                 fase_dia=="Pleno dia",1,0)] 

# Accident by night
acidentes_concessoes[,acidente_pelanoite := ifelse(fase_dia=="Plena Noite" |
                                                   fase_dia=="Plena noite" |
                                                   fase_dia=="Anoitecer",1,0)] 

# Joining day vs night accidents
acidentes_concessoes[, area_acidente_dia := ifelse(acidente_pelodia  ==1,1,NA)]
acidentes_concessoes[, area_acidente_dia := ifelse(acidente_pelanoite==1,0,area_acidente_dia)]


# Acident by urban area
acidentes_concessoes[, acidente_urb := ifelse(uso_solo=="Urbano"  | uso_solo=="Sim",1,0)] 

# Acident by rural area
acidentes_concessoes[,acidente_rur := ifelse(uso_solo=="Rural"  | uso_solo=="Não",1,0)] 

# Joining urban vs rural accidents
acidentes_concessoes[, area_acidente_urb := ifelse(acidente_urb==1,1,NA)]
acidentes_concessoes[, area_acidente_urb := ifelse(acidente_rur==1,0,area_acidente_urb)]



# Single lane accident
acidentes_concessoes[,acidente_pistasimp := ifelse(tipo_pista=="Simples",1,NA),] 

# Double or multiple lane accident
acidentes_concessoes[,acidente_pistamult := ifelse(tipo_pista=="Dupla" |
                                                   tipo_pista=="Múltipla" ,1,NA)] 

# Accident in metropolitan area
acidentes_concessoes[,acidente_dentrm := ifelse(metro==1,1,NA)] 

# Accident out metropolitan area
acidentes_concessoes[,acidente_forarm := ifelse(metro!=1,1,NA)] 

# Good weather when the accident happened
acidentes_concessoes[,acidente_tempo_bom := ifelse(condicao_metereologica=="Ceu Claro" |
                                                  condicao_metereologica=="Céu Claro"  |
                                                  condicao_metereologica=="Sol",1,NA)] 

# Bad weather when the accident happened
acidentes_concessoes[,acidente_tempo_ruim := ifelse(condicao_metereologica=="Nublado"         |
                                                   condicao_metereologica=="Chuva"            |
                                                   condicao_metereologica=="Nevoeiro/neblina" | 
                                                   condicao_metereologica=="Vento"            |
                                                   condicao_metereologica=="Garoa/Chuvisco"   |
                                                   condicao_metereologica=="Nevoeiro/Neblina" | 
                                                   condicao_metereologica=="Granizo"          | 
                                                   condicao_metereologica=="Neve",1,NA)] 



# Death by vehicle
acidentes_concessoes[,morte_automovel   := ifelse(mortos==1 & automovel   == 1,1,0)]
acidentes_concessoes[,morte_motocicleta := ifelse(mortos==1 & motocicleta == 1,1,0)]
acidentes_concessoes[,morte_caminhao_onibus := ifelse(mortos==1 & (caminhao == 1 | onibus == 1),1,0)]


# Accidents
acidentes_concessoes[,acidente := 1] 


#### 3) Base cleaning ------------------------------------------------------

acidentes_concessoes <- acidentes_concessoes %>%
  # Remove accidents that occurred outside the limits of the highways
  filter(km >= min_km & km <= max_km) %>% 
  # Remove other problems from the base
  filter((grupo_tratado == 0 & km > min_km & km < max_km) | 
           grupo_tratado == 1) %>% 
  # These highways have some problems
  filter(!(uf=="RJ" & br == 393 & grupo_tratado == 0 & km >=105 & km <= 291.1)) %>% 
  filter(!(uf=="SP" & br == 153 & (km>230.3 & km<256)))



# Our analysis is 2007-2017, so we removed 2018 from the sample
acidentes_concessoes <- filter(acidentes_concessoes,ano_acidente <2018) 



haven::write_dta(acidentes_concessoes,
                 "base acidentes (teste) - todas UFs.dta")





### Generate id for id_br_uf

a <- acidentes_concessoes %>% 
  mutate(br_uf = paste0(br,"_",uf,"_",grupo_tratado),
         ano_inicio     = as.Date(ano_inicio),
         data_inversa   = as.Date(data_inversa)) %>%
  distinct(br_uf) %>% group_by(br_uf) %>% 
  mutate(id_br_uf = row_number(br_uf))

# Join this id back to our database

acidentes_concessoes <- acidentes_concessoes %>% 
  mutate(br_uf          = paste0(br,"_",uf,"_",grupo_tratado),
         ano_inicio     = as.Date(ano_inicio),
         data_inversa   = as.Date(data_inversa),
         inicio_pedagio = as.Date(inicio_pedagio),
         data_concessao = data_inversa,
         ano_ocorrencia = ano_acidente) %>%
  left_join(a) 
rm(a)


# Drop duplicates
acidentes_concessoes <- acidentes_concessoes %>% 
  distinct(id,data_inversa,dia_semana,horario,uf,br,km,municipio,.keep_all = T)





#### 4) Grouping of roads ------------------------------------------------

pre_temp <- acidentes_concessoes

acidentes_concessoes <- cbind(setDT(acidentes_concessoes)[, lapply(.SD, mean, na.rm=TRUE),
                                    by=list(id_br_uf,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                    .SDcols=c("inicio_pedagio","yr_start","data_concessao",
                                              "min_km","max_km",
                                              "ano_inicio","ano_acidente","extensao")],
    setDT(acidentes_concessoes)[, lapply(.SD, sum, na.rm=TRUE),
                                    by=list(id_br_uf,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                    .SDcols=c("acidente","pessoas","mortos",
                                              "area_acidente_urb","area_acidente_dia",
                                              "feridos_leves","feridos_graves",
                                              "ilesos","masculino","feminino","motocicleta",
                                              "morte_automovel","morte_motocicleta","morte_caminhao_onibus", 
                                              "automovel","caminhao","onibus","outros",
                                              "idade_10","idade_20","idade_30","idade_40",
                                              "idade_50","idade_60","idade_120","via_pavimentada",
                                              "via_duplicada","acidente_pelodia","acidente_pelanoite",
                                              "acidente_urb","acidente_rur","acidente_pistasimp",
                                              "acidente_pistamult","acidente_dentrm","acidente_forarm",
                                              "acidente_tempo_bom","acidente_tempo_ruim")])
    

## Organizing variables after summarise

# Standardizing date variable
acidentes_concessoes <-  acidentes_concessoes %>%  
  mutate(dt = mdy(paste0(mes_ocorrencia,"/","01/",ano_ocorrencia)),
         data_inversa = as.monthly(dt),
         ym_concessao = as.monthly(ano_inicio),
         ym_pedagio = as.monthly(inicio_pedagio)) %>% 
  dplyr::rename(data = data_inversa)

# Separating year and month of accident, concession and toll
acidentes_concessoes <-  acidentes_concessoes %>% 
  mutate(data_acidente = dt,
         ano_pedagio   = as.numeric(substr(inicio_pedagio,1,4)),
         mes_pedagio   = as.numeric(substr(inicio_pedagio,6,7)),
         ano_concessao = as.numeric(substr(data_concessao,1,4)),
         mes_concessao = as.numeric(substr(data_concessao,6,7)),
         ano_acidente  = as.numeric(substr(data_acidente,1,4)),
         mes_acidente  = as.numeric(substr(data_acidente,6,7)),
         dia_acidente  = as.numeric(substr(data_acidente,9,10)),
         yr_start      = as.numeric(substr(ano_inicio,1,4)))


#### 5) Balancing --------------------------------------------------------

# Make sure that every month they have information for the highways
acidentes_concessoes_geral <- acidentes_concessoes %>% 
  dplyr::group_by(id_br_uf) %>% statar::fill_gap(data, full = TRUE)

# Fixed variable and drop repeated variable
acidentes_concessoes_geral <- acidentes_concessoes_geral %>%
  select(-c(br.1,uf.1,id_br_uf.1,grupo_tratado.1,
                           ano_ocorrencia.1, mes_ocorrencia.1))


# Turn NA to zero
acidentes_concessoes_geral[,15:48] <- lapply(acidentes_concessoes_geral[,15:48],
                                      function(n) ifelse(is.na(n),0,n))


# Organizing the post balanced base
acidentes_concessoes_geral <- acidentes_concessoes_geral %>%
  group_by(id_br_uf) %>% 
  fill(br,uf,min_km,max_km,ano_inicio, mes_ocorrencia,  ano_ocorrencia,
       grupo_tratado, data_concessao, extensao, yr_start,ano_acidente,
       inicio_pedagio,ano_pedagio,mes_pedagio,ano_concessao,
       mes_concessao,mes_acidente,dia_acidente,ym_concessao,
       .direction = "downup")



#### 6) Generating variables for treatment ------------------------------------

## Setting the date variable
acidentes_concessoes_geral <-  acidentes_concessoes_geral %>% 
  mutate(ano_acidente = ifelse(ano_acidente==0,ano_ocorrencia,ano_acidente))

## Generating exposure time
acidentes_concessoes_geral <- acidentes_concessoes_geral %>% 
  mutate(tempo_exposicao = ((ano_acidente-yr_start)+1))

acidentes_concessoes_geral <- acidentes_concessoes_geral %>% 
  mutate(treat_1yr =  ifelse(tempo_exposicao==1,1,0),
         treat_2yr =  ifelse(tempo_exposicao==2,1,0),
         treat_3yr =  ifelse(tempo_exposicao==3,1,0),
         treat_4yr =  ifelse(tempo_exposicao==4,1,0),
         treat_5yr =  ifelse(tempo_exposicao==5,1,0),
         treat_6yr =  ifelse(tempo_exposicao==6,1,0),
         treat_7yr =  ifelse(tempo_exposicao==7,1,0),
         treat_8yr =  ifelse(tempo_exposicao==8,1,0),
         treat_9yr =  ifelse(tempo_exposicao==9,1,0),
         treat_10yr = ifelse(tempo_exposicao==10,1,0),
         treat_1yr_10yrs = ifelse(tempo_exposicao>0,tempo_exposicao,0))

acidentes_concessoes_geral <- acidentes_concessoes_geral %>% 
  mutate(treat_1yr = ifelse(is.na(treat_1yr),0,treat_1yr),
         treat_2yr = ifelse(is.na(treat_2yr),0,treat_2yr),
         treat_3yr = ifelse(is.na(treat_3yr),0,treat_3yr),
         treat_4yr = ifelse(is.na(treat_4yr),0,treat_4yr),
         treat_5yr = ifelse(is.na(treat_5yr),0,treat_5yr),
         treat_6yr = ifelse(is.na(treat_6yr),0,treat_6yr),
         treat_7yr = ifelse(is.na(treat_7yr),0,treat_7yr),
         treat_8yr = ifelse(is.na(treat_8yr),0,treat_8yr),
         treat_9yr = ifelse(is.na(treat_9yr),0,treat_9yr),
         treat_10yr = ifelse(is.na(treat_10yr),0,treat_10yr),
         treat_1yr_10yrs = ifelse(is.na(treat_1yr_10yrs),0,treat_1yr_10yrs))



# Leads and lags
acidentes_concessoes_geral <- acidentes_concessoes_geral %>% 
  mutate(treat_1yrbef =  ifelse(tempo_exposicao==-1,1,0),
         treat_2yrbef =  ifelse(tempo_exposicao==-2,1,0),
         treat_3yrbef =  ifelse(tempo_exposicao==-3,1,0),
         treat_4yrbef =  ifelse(tempo_exposicao==-4,1,0),
         treat_5yrbef =  ifelse(tempo_exposicao==-5,1,0),
         treat_6yrbef =  ifelse(tempo_exposicao==-6,1,0))



# Correction of any problems in date variables
setDT(acidentes_concessoes_geral)
acidentes_concessoes_geral[, ano_inicio := as.Date(ano_inicio,  origin = "1899-12-30")]
acidentes_concessoes_geral[, data_acidente := as.Date(data_acidente,  origin = "1899-12-30")]
acidentes_concessoes_geral[, data_concessao := as.Date(data_concessao,  origin = "1899-12-30")]



# Highways that have had a concession must be controlled before the concession starts
acidentes_concessoes_geral[ , treat := ifelse(data >= ym_concessao & grupo_tratado ==1, 1, 0) ]
acidentes_concessoes_geral[ , treat := ifelse(is.na(treat), 0, treat) ]

# Same proceed for tools
acidentes_concessoes_geral[ , treat_pedag := ifelse(data >= ym_pedagio & grupo_tratado == 1, 1, 0) ]
acidentes_concessoes_geral[ , treat_pedag := ifelse(is.na(treat_pedag), 0, treat_pedag) ]


# Gen day of the week
acidentes_concessoes_geral <- mutate(acidentes_concessoes_geral,
                        day_of_week = weekdays(as.Date(data)),
                        month_pedagio = month.abb[mes_pedagio],
                        month_concessao = month.abb[mes_concessao],
                        ym_acidente = zoo::as.yearmon(
                        paste(ano_acidente, mes_acidente), "%Y %m"))

#### 7) Read covariates databases ---------------------------------------------------

source('./R/2. covs.R', encoding = 'UTF-8')

## Include Covariates


# Anuals covariates
base <- acidentes_concessoes_geral %>% left_join(cov_anual)

# Monthly covariates
base <- base %>% left_join(cov_mensal)

# Situation Extension
base <- base %>% mutate(extensao_situacao = max_km - min_km)

# Base organization: transformation into logarithm or weighting
base <- base %>% 
  mutate(acidente_km = acidente/extensao_situacao,
         pessoas_km = pessoas/extensao_situacao,
         mortos_km = mortos/extensao_situacao,
         veiculos_km = automovel/extensao_situacao,
         feridos_leves_km = feridos_leves/extensao_situacao,
         feridos_graves_km = feridos_graves/extensao_situacao,
         ilesos_km = ilesos/extensao_situacao,
         feridos_leves_acidente = feridos_leves/acidente,
         feridos_graves_acidente= feridos_graves/acidente,
         morte_acidente = mortos/acidente,
         morte_automovel_acidente = morte_automovel/acidente,
         morte_motocicleta_acidente = morte_motocicleta/acidente,
         morte_caminhao_onibus_acidente = morte_caminhao_onibus/acidente,
         morte_pessoas  = mortos/pessoas,
         feridos = feridos_leves + feridos_graves,
         feridos_km = feridos/extensao_situacao,
         acidente_automovel_km = automovel/extensao_situacao,
         acidente_motocicleta_km = motocicleta/extensao_situacao,
         caminhao_onibus = caminhao + onibus,
         acidente_caminhao_onibus_km = caminhao_onibus/extensao_situacao,
         feridos_acidente = feridos/acidente,
         feridos_pessoas  = feridos/pessoas,
         ln_pib_extension = log(pib_extension),
         ln_pop_extension = log(pop_extension),
         ln_emp_extension = log(emp_extension),
         popmais70 = pop70_a_74 + pop75_a_79 + pop80_a_84 + pop85_a_89 + pop90_a_94 + pop95_a_99,
         pop20_29 = pop20_a_24 + pop25_a_29,
         ln_popmais90           = log(popmais70),
         ln_pop20_29            = log(pop20_29),
         ln_temp                = log(v_temp),
         ln_rain                = log(v_rain),
         ln_v_rain_extension    = log(v_rain_extension),
         ln_v_temp_extension    = log(v_temp_extension),
         ln_v_rain_extension_m  = log(v_rain_extension_mensal),
         ln_v_temp_extension_m  = log(v_temp_extension_mensal),
         ln_emp_extension       = log(emp_extension),
         ln_pib_extension       = log(pib_extension), 
         ln_pop_extension       = log(pop_extension))

base <- base %>%
  mutate(acidente_km = ifelse(is.na(acidente_km),0,acidente_km),
         pessoas_km = ifelse(is.na(pessoas_km),0,pessoas_km),
         mortos_km = ifelse(is.na(mortos_km),0,mortos_km),
         feridos_km = ifelse(is.na(acidente_km),0,acidente_km),
         morte_automovel_km = ifelse(is.na(morte_automovel_km),0,morte_automovel_km),
         morte_motocicleta_km = ifelse(is.na(morte_motocicleta_km),0,morte_motocicleta_km),
         veiculos_km = ifelse(is.na(veiculos_km),0,veiculos_km),
         feridos_leves_km = ifelse(is.na(feridos_leves_km),0,feridos_leves_km),
         feridos_graves_km = ifelse(is.na(feridos_graves_km),0,feridos_graves_km),
         ilesos_km = ifelse(is.na(ilesos_km),0,ilesos_km),
         feridos_km = ifelse(is.na(feridos_km),0,feridos_km))

# Turning into as character
base <- base %>% 
  mutate(ano_acidente    = as.character(ano_acidente),
         dia_acidente    = as.character(dia_acidente),
         mes_acidente    = as.character(mes_acidente),
         id_br_uf        = as.character(id_br_uf),
         treat_1yr_10yrs = as.character(treat_1yr_10yrs))

# ID State

base <- base %>% left_join(read_state() %>% as.data.frame() %>% 
                             select(-c(geom)),
                           by = c('uf'="abbrev_state"))



# Total of accidents
base <- base %>% 
  group_by(id_br_uf) %>% 
  mutate(acidente_tot = sum(acidente, na.rm=T))

temp <- base %>% mutate(teste = as.numeric(substr(data,1,4)))  %>%  filter(teste == 2007) %>% 
  group_by(id_br_uf) %>% 
  dplyr::summarise(mortos_tot_t0    = sum(mortos, na.rm=T)) %>% ungroup() %>% 
  mutate(#mortos_tot_t0    = sum(mortos, na.rm=T),
         median_mortos_t0 = median(mortos_tot_t0,na.rm = T),
         diff = ifelse(mortos_tot_t0 >= median_mortos_t0,1,0)) %>% 
  select(c(id_br_uf,mortos_tot_t0,median_mortos_t0,diff))

base <- left_join(base,temp)


# 1º Concession Phase of our database (2º Phase in history)
setDT(base)
base[,fase1 := ifelse(yr_start==2008 | yr_start==2009,1,NA)]
base[,grupo_fase1 := mean(fase1, na.rm=T), by = uf]
base[,fase1 := ifelse(yr_start==2008 | yr_start==2009,1,0)]

# 2º Concession Phase of our database (3º Phase in history)
base[,fase2 := ifelse(yr_start==2013 | yr_start==2014,1,NA)] 
base[,grupo_fase2 := mean(fase2, na.rm=T), by = uf]
base[,fase2 := ifelse(yr_start==2013 | yr_start==2014,1,0)] 

base[fase2==1 ,grupo_fase1 := NA]
base[fase1==1 ,grupo_fase2 := NA]

summary(base$extensao)

## Balancing day/night and urban/rural data

source('./R/3. heterogeneidade_base.R')



# Remove all previous bases
rm(acidentes,acidentes_concessoes,cov_anual,cov_mensal)




#### 8) Data Descriptive statistics ------------------------------------------
## Back-of-the-envelope

tabela1 <- base %>% 
  group_by(ano_ocorrencia,treat) %>% 
  #ungroup() %>% 
  dplyr::summarise(acidente= sum(acidente,na.rm = TRUE),
                   pessoas = sum(pessoas,na.rm = TRUE),
                   mortos = sum(mortos,na.rm = TRUE),
                   feridos_leves = sum(feridos_leves,na.rm = TRUE),
                   feridos_graves = sum(feridos_graves,na.rm = TRUE),
                   ilesos = sum(ilesos,na.rm = TRUE),
                   min_km  = mean(min_km, na.rm = TRUE),
                   max_km  = mean(max_km, na.rm = TRUE),
                   ano_acidente = mean(ano_acidente, na.rm = T),
                   extensao = mean(extensao, na.rm = T))

sum(tabela1$acidente)*0.016

tabela2 <- tabela1 %>% 
  select(c(ano_ocorrencia,treat,mortos,acidente)) %>% 
  mutate(mortos_calc = mortos/(0.984)) %>% 
  filter(treat==1 | (treat==0 & ano_ocorrencia==2007))






## Gráfico DD

base <- base %>% group_by(id_br_uf) %>% 
  dplyr::mutate(mes = 1,mes_acumulado = cumsum(mes),
                primeira_concessao = ifelse(mes_acumulado>=15 & mes_acumulado<100,1,
                                            ifelse(mes_acumulado>=100,2,0)))

temp <- base %>% ungroup() %>% 
  distinct(br,uf,treat, .keep_all = T) %>% 
  filter(treat==1) %>% select(c(br,uf,treat,mes_acumulado)) %>% 
  dplyr::rename(cond = mes_acumulado) %>% 
  mutate(mean_cond = mean(cond))

temp <- temp %>% mutate(mean_cond = round(mean_cond,digits=0))

a <- base %>% left_join(temp) %>% 
  group_by(br,uf) %>% 
  fill(cond,
       .direction = "up") %>% 
  mutate(cond = ifelse(is.na(cond),57,cond),
         diff = mes_acumulado - cond) %>% 
  filter(!is.na(cond))

a <- base %>% ungroup() %>% 
  select(c(ano_acidente,mes_acumulado,mes_acidente,
           data,grupo_tratado,morte_acidente,primeira_concessao,
           mes_acumulado,acidente_km)) %>% 
  mutate(trimestre = ifelse(mes_acidente<=3,1,
                            ifelse(mes_acidente>3 & mes_acidente<=6,2,
                                   ifelse(mes_acidente>6 & mes_acidente<=9,3,
                                          ifelse(mes_acidente>9 & mes_acidente<=12,4,NA))))) %>% 
  group_by(data,grupo_tratado,primeira_concessao,mes_acumulado) %>% 
  dplyr::summarise(morte_acidente = mean(morte_acidente,na.rm=T),
                   acidente_km = mean(acidente_km,na.rm=T))

## Mortes por acidente

ggplot() + geom_point(data=a,aes(x = mes_acumulado,
                                 y = morte_acidente,
                                 col=factor(grupo_tratado))) +
  geom_smooth(data=subset(a,primeira_concessao==0),
              method = "lm",se=F,aes(x = mes_acumulado,
                                     y = morte_acidente,
                                     col=factor(grupo_tratado))) +
  geom_smooth(data=subset(a,primeira_concessao==1),
              method = "lm",se=F,aes(x = mes_acumulado,
                                     y = morte_acidente,
                                     col=factor(grupo_tratado))) +
  geom_smooth(data=subset(a,primeira_concessao==2),
              method = "lm",se=F,aes(x = mes_acumulado,
                                     y = morte_acidente,
                                     col=factor(grupo_tratado))) +
  geom_vline(xintercept=15, linetype="solid") +
  geom_vline(xintercept=100, linetype="solid")

## Acidente

ggplot() + geom_point(data=a,aes(x = mes_acumulado,
                                 y = acidente_km,
                                 col=factor(grupo_tratado))) +
  geom_smooth(data=subset(a,primeira_concessao==0),
              method = "lm",se=F,aes(x = mes_acumulado,
                                     y = acidente_km,
                                     col=factor(grupo_tratado))) +
  geom_smooth(data=subset(a,primeira_concessao==1),
              method = "lm",se=F,aes(x = mes_acumulado,
                                     y = acidente_km,
                                     col=factor(grupo_tratado))) +
  geom_smooth(data=subset(a,primeira_concessao==2),
              method = "lm",se=F,aes(x = mes_acumulado,
                                     y = morte_acidente,
                                     col=factor(grupo_tratado))) +
  geom_vline(xintercept=15, linetype="solid") +
  geom_vline(xintercept=100, linetype="solid")

## Table  1 ---------

temp <- read_sf(dsn="./Shapefiles/ST_DNIT_Rodovias_SNV2015_03.shp") %>%
  as.data.frame() %>% select(-c(geometry)) %>% 
  mutate(br= as.numeric(br)) %>% 
  left_join(empresas) %>%
  mutate(yr_start = as.numeric(substr(Ano_Inicio,-4,4)),
         concessao = ifelse(is.na(Empresa),0,1)) %>% 
  distinct(br,uf,km_inicial,km_final, .keep_all = TRUE) %>%
  group_by(br,uf,concessao) %>% 
  dplyr::summarise(Ano_Inicio = mean(Ano_Inicio, na.rm = T),
                   min_km = min(km_inicial, na.rm = T),
                   max_km = max(km_final, na.rm = T),
                   yr_start = mean(yr_start, na.rm = T)) %>% 
  mutate(Fase_Concessao = ifelse(is.na(yr_start),"Não Concedida",
                          ifelse(yr_start>=1995 & yr_start<=2001,"1º Fase",
                          ifelse(yr_start>=2008 & yr_start<=2012,"2º Fase",
                          ifelse(yr_start>=2013 & yr_start<=2014,"3º Fase",
                          ifelse(yr_start>=2018,"4º Fase",yr_start))))),
         extensao = max_km - min_km,
         Primeira = ifelse(Fase_Concessao=="1º Fase",1,0),
         Segunda  = ifelse(Fase_Concessao=="2º Fase",1,0),
         Terceira = ifelse(Fase_Concessao=="3º Fase",1,0),
         Quarta   = ifelse(Fase_Concessao=="4º Fase",1,0),
         NC       = ifelse(Fase_Concessao=="Não Concedida",1,0)) %>%
  select(-c(yr_start,concessao)) %>% 
  group_by(Fase_Concessao) %>% 
  dplyr::summarise(extensao = sum(extensao),
                   Primeira = sum(Primeira),
                   Segunda  = sum(Segunda),
                   Terceira = sum(Terceira),
                   Quarta   = sum(Quarta),
                   NC       = sum(NC)) %>% 
  mutate(rodovias = Primeira + Segunda + Terceira + Quarta + NC) %>% 
  select(-c(Primeira,Segunda,Terceira,Quarta,NC))

write.csv2(temp,"tabela1.csv",row.names = F)

rm(temp)

## Mapa -----

# Base com as empresas

empresas <- readxl::read_excel(path = "./data/Empresas_concessao.xlsx") %>% 
  mutate(yr_start = as.numeric(substr(Ano_Inicio,-4,4))) %>% 
  filter(yr_start>=2007 & yr_start <= 2017)

# Base rodovias

dnit <- read_sf(dsn="./Shapefiles/ST_DNIT_Rodovias_SNV2015_03.shp") %>%
  mutate(br= as.numeric(br)) %>% 
  left_join(empresas) %>%
  mutate(concessao = ifelse(!is.na(Empresa),"Treated","Non treated"),
         yr_start = as.numeric(substr(Ano_Inicio,-4,4)),
         Fase_Concessao = ifelse(is.na(yr_start),
                                 "Não Concedida",
                                 ifelse(yr_start>=1995 & yr_start<=1998,"1º Fase",
                                 ifelse(yr_start>=2008 & yr_start<=2009,"2º Fase",
                                 ifelse(yr_start>=2013 & yr_start<=2014,"3º Fase",
                                 ifelse(yr_start>=2018,"4º Fase",yr_start)))))) %>% 
  distinct(id,br,uf, .keep_all = T)

dnit <- subset(dnit, uf %in% distinct(empresas,uf)$uf)

# Arquivo mapa dos estados

estado <- read_state(code_state = 'all')

# Mapa final

mapa <- ggplot() + 
  geom_sf(data=estado, color = "black", fill ="white") + # Mapa dos estados
  geom_sf(data=dnit, aes(colour=concessao)) + theme_bw() + # Mapa das rodovias
  scale_color_manual(values=c("gray80","#20908c")) + # Escolhadas cores
  labs(
    color = "Road Concession" ) +
  annotation_scale(location = "bl", width_hint = 0.25,
                   pad_x = unit(0.6, "in"), pad_y = unit(0.4, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         width = unit(1.5, "cm"),
                         pad_x = unit(0.9, "in"), pad_y = unit(0.6, "in")) +
  theme_minimal() +
  theme_void()+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  theme(legend.position = c(0.8, 0.2))

mapa

ggsave(mapa, file="mapa.png", dpi = 300,
       height = 17, width = 17, units = "cm") 

#### 9) Estimations -------------------------------------------------------------

source('./R/4. reg_padrao.R')

#### 10) Spatial Diff in Diff  -------------------------------------------------

source('./R/5. reg_spatial.R')

##################################### END SCRIPT ######################################

# Alves, P. J., Emanuel, L., & Pereira, R. H. M. (2020, July 15). The causal effect of road concessions on road safety.
# https://doi.org/10.31235/osf.io/rqew3
 