#### 0) Read packages---------------------------------------------------------

library(dplyr)        ; library(data.table) ; library(readxl)   ; library(foreign)
library(readr)        ; library(bit64)      ; library(magrittr) ; library(furrr)
library(haven)        ; library(foreign)    ; library(genvar)   ; library(stargazer)
library(lfe)          ; library(sf)         ; library(geobr)    ; library(ggplot2)
library(lwgeom)       ; library(tidyr)      ; library(statar)   ; library(rdrobust)
library(rgeos)        ; library(sp)         ; library(plyr)     ; library(lubridate)
library(rnaturalearth); library(viridis)    ; library(ggspatial); library(sf)
library(spdep)        ; library(spatialreg)

#### 1) Ler bases de dados ------------------------------------------------

#### Leitura e filtro inicial da base de dados original

source('./R/leitura.R')

#### BASE RODOVIAS

# ler base de rodovias concedidas

rodovias <- readxl::read_excel(path = "./Database/Rodovias_completo.xlsx") %>%  # col_types='text'
  mutate(br = as.numeric(br),km_inicial = as.numeric(km_inicial),
         extensao = as.numeric(extensao),km_final = as.numeric(km_final))

# Base das concessoes

empresas <- readxl::read_excel(path = "./Database/Empresas_concessao.xlsx")


# Incluir apenas uf que teve concessao
temp <- empresas %>% distinct(uf, Ano_Inicio, .keep_all = T) %>%
  mutate(base = 1) %>% 
  mutate(yr_start = as.numeric(substr(Ano_Inicio,-4,4))) %>% 
  filter(yr_start >= 2007) %>% 
  filter(yr_start <= 2017) %>% select(c(uf,base))

rodovias <- rodovias %>% left_join(temp) %>% 
  filter(base==1) %>% select(-c(base))

rm(temp)

#temp <- empresas %>%  
#  mutate(yr_start = as.numeric(substr(Ano_Inicio,-4,4))) %>% 
#  filter(yr_start < 2007)

#rodovias <- subset(rodovias, !(id %in% temp$id))
#rodovias <- subset(rodovias, !(br %in% temp$br & uf %in% temp$uf))
#rm(temp)

# Aplicando base de empresas nas rodovias
rodovias <- rodovias %>% left_join(empresas) %>%  
  mutate(yr_start = as.numeric(substr(Ano_Inicio,-4,4))) %>% 
  filter(is.na(yr_start) | yr_start <= 2018)


# retira linhas reptidas e agrega trechos pela extensao
rodovias <- rodovias %>%
  distinct(br,uf,km_inicial,km_final, .keep_all = TRUE) %>%
  mutate(concessao = ifelse(is.na(Empresa),0,1),
         rod = ifelse(superficie=="PLA" & !is.na(superfic_1),
                      superfic_1,superficie),
         via_duplicada = ifelse(rod=="DUP",1,0),
         via_pavimentada = ifelse(rod=="PAV",1,0))

# Agrupar rodovias
rodovias <- rodovias %>%
  group_by(br,uf,concessao,yr_start) %>% 
  dplyr::summarise(Ano_Inicio = mean(Ano_Inicio, na.rm = T),
                   Ano_Final = mean(Ano_Final, na.rm = T),
                   Inicio_pedagio = mean(Inicio_pedagio, na.rm = T),
                   min_km = min(km_inicial, na.rm = T),
                   max_km = max(km_final, na.rm = T),
                   extensao = sum(extensao, na.rm = T),
                   via_duplicada = sum(via_duplicada,na.rm = T),
                   via_pavimentada = sum(via_pavimentada,na.r=T)) %>% ungroup() %>% 
  select(c(Ano_Inicio,Inicio_pedagio,yr_start,br,uf,min_km,
           max_km,extensao,via_pavimentada,via_duplicada))


# Ajustes manuais
rodovias <- rodovias %>%
  # MT e MG não tiveram extensao correta
  mutate(min_km = ifelse(br==163 & uf == "MT" & is.na(Inicio_pedagio),855,min_km),
         max_km = ifelse(br==381 & uf == "MG" & is.na(Inicio_pedagio),476.1,max_km)) %>% 
  mutate(primeira_fase = ifelse(yr_start < 2007,1,0),
         extensao = max_km - min_km)






########## 2)  Localizar se acidentes ocorreram em trechos concedidos -----------

acidentes_concessoes <- acidentes %>% 
  inner_join(rodovias, by = c("uf"="uf","br"="br")) %>% 
  mutate(km = as.numeric(km)) %>% 
  # Alteração manual de algumas rodovias que o comando não resolveu
  mutate(max_km = ifelse(br==393 & uf == "RJ" & 
                           km<=105.3,105.3,max_km),
         min_km = ifelse(br==393 & uf == "RJ" & 
                           km>= 291.1,291.1,min_km))

# Gerar variável de concessão
setDT(acidentes_concessoes)
acidentes_concessoes[,concessao := ifelse(data.table::between(km,min_km,max_km) &
                                            !is.na(Ano_Inicio),1,0)]

acidentes_concessoes[,primeira_fase := ifelse(data.table::between(km,min_km,max_km) &
                                                !is.na(primeira_fase),1,0)]

acidentes_concessoes[,primeira_fase := ifelse(yr_start >= 2007,0,primeira_fase)]

# Retirar rodovias concedidas na primeira fase
temp <- acidentes_concessoes %>% 
  filter(primeira_fase==1)

acidentes_concessoes <- subset(acidentes_concessoes, !(id %in% temp$id))


acidentes_concessoes <- acidentes_concessoes %>% 
  # Gerar uma nova variavel de data
  mutate(data_inversa = paste0(ano_ocorrencia,"-",mes_ocorrencia,"-",dia_ocorrencia)) %>% 
  # Renomear algumas variáveis
  dplyr::rename(grupo_tratado = concessao, ano_acidente = ano_ocorrencia,
                inicio_pedagio = Inicio_pedagio,
                ano_inicio = Ano_Inicio) %>% ungroup()


## Outras variáveis

# Acidente pelo dia
acidentes_concessoes[,acidente_pelodia := ifelse(fase_dia=="Amanhecer" |
                                                        fase_dia=="Pleno dia",1,NA)] 

# Acidente pela noite
acidentes_concessoes[,acidente_pelanoite := ifelse(fase_dia=="Plena Noite" |
                                                         fase_dia=="Plena noite" |
                                                         fase_dia=="Anoitecer",1,NA)] 

# Acidente em area urbana
acidentes_concessoes[, acidente_urb := ifelse(uso_solo=="Urbano",1,NA)] 

# Acidente em area rural
acidentes_concessoes[,acidente_rur := ifelse(uso_solo=="Rural",1,NA)] 

# Acidente em pista simples
acidentes_concessoes[,acidente_pistasimp := ifelse(tipo_pista=="Simples",1,NA),] 

# Acidente em pista dupla ou multipla
acidentes_concessoes[,acidente_pistamult := ifelse(tipo_pista=="Dupla" |
                                                         tipo_pista=="Múltipla" ,1,NA)] 

# Acidente em região metrpolitana
acidentes_concessoes[,acidente_dentrm := ifelse(metro==1,1,NA)] 

# Acidente fora da reigão metropolitana
acidentes_concessoes[,acidente_forarm := ifelse(metro!=1,1,NA)] 

# Tempo bom em que aconteceu o acidente
acidentes_concessoes[,acidente_tempo_bom := ifelse(condicao_metereologica=="Ceu Claro" |
                                                  condicao_metereologica=="Céu Claro"  |
                                                  condicao_metereologica=="Sol",1,NA)] 

# Tempo ruim em que aconteceu o acidente
acidentes_concessoes[,acidente_tempo_ruim := ifelse(condicao_metereologica=="Nublado"         |
                                                   condicao_metereologica=="Chuva"            |
                                                   condicao_metereologica=="Nevoeiro/neblina" | 
                                                   condicao_metereologica=="Vento"            |
                                                   condicao_metereologica=="Garoa/Chuvisco"   |
                                                   condicao_metereologica=="Nevoeiro/Neblina" | 
                                                   condicao_metereologica=="Granizo"          | 
                                                   condicao_metereologica=="Neve",1,NA)] 

# 1º Fase da concessão em que aconteceu o acidente
acidentes_concessoes[,fase1 := ifelse(yr_start==2008 | yr_start==2009,1,NA)] 

# 2º Fase da concessão em que aconteceu o acidente
acidentes_concessoes[,fase2 := ifelse(yr_start==2013 | yr_start==2014,1,NA)] 

# Observações de acidentes
acidentes_concessoes[,acidente := 1] 


#### 3) Limpeza da base ------------------------------------------------------

acidentes_concessoes <- acidentes_concessoes %>%
  # Retirar acidentes que ocorreram fora dos limites das rodovias
  filter(km >= min_km & km <= max_km) %>% 
  # Filtro para retirar outros problemas da base
  filter((grupo_tratado == 0 & km > min_km & km < max_km) | 
           grupo_tratado == 1) %>% 
  # Filtro manual: essas rodovias possuem pequenos problemas
  filter(!(uf=="RJ" & br == 393 & grupo_tratado == 0 & km >=105 & km <= 291.1)) %>% 
  filter(!(uf=="SP" & br == 153 & (km>230.3 & km<256)))

# Como nossa análise é 2007-2017, retiramos 2017 da amostra
acidentes_concessoes <- filter(acidentes_concessoes,ano_acidente <2018) 

# Retirar duplicatas
acidentes_concessoes <- acidentes_concessoes %>% 
  distinct(id,data_inversa,dia_semana,horario,uf,br,km,municipio,.keep_all = T)

#haven::write_dta(acidentes_concessoes,
#                 "base acidentes (teste) - Com todas as UFs.dta")





### Gerando indentificador id_br_uf

a <- acidentes_concessoes %>% 
  mutate(br_uf = paste0(br,"_",uf,"_",grupo_tratado),
         ano_inicio     = as.Date(ano_inicio),
         data_inversa   = as.Date(data_inversa)) %>%
  distinct(br_uf) %>% group_by(br_uf) %>% 
  mutate(id_br_uf = row_number(br_uf))

# Agregação

acidentes_concessoes <- acidentes_concessoes %>% 
  mutate(br_uf          = paste0(br,"_",uf,"_",grupo_tratado),
         ano_inicio     = as.Date(ano_inicio),
         data_inversa   = as.Date(data_inversa),
         inicio_pedagio = as.Date(inicio_pedagio),
         data_concessao = data_inversa,
         ano_ocorrencia = ano_acidente) %>%
  left_join(a) 
rm(a)



#### 4) Agrupamento de trechos ------------------------------------------------

acidentes_concessoes <- cbind(setDT(acidentes_concessoes)[, lapply(.SD, mean, na.rm=TRUE),
                                    by=list(id_br_uf,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                    .SDcols=c("inicio_pedagio","yr_start","data_concessao",
                                              "min_km","max_km","fase1","fase2",
                                              "ano_inicio","ano_acidente","extensao")],
    setDT(acidentes_concessoes)[, lapply(.SD, sum, na.rm=TRUE),
                                    by=list(id_br_uf,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                    .SDcols=c("acidente","pessoas","mortos",
                                              "feridos_leves","feridos_graves",
                                              "ilesos","masculino","feminino","motocicleta",
                                              "automovel","caminhao","onibus","outros",
                                              "idade_10","idade_20","idade_30","idade_40",
                                              "idade_50","idade_60","idade_120","via_pavimentada",
                                              "via_duplicada","acidente_pelodia","acidente_pelanoite",
                                              "acidente_urb","acidente_rur","acidente_pistasimp",
                                              "acidente_pistamult","acidente_dentrm","acidente_forarm",
                                              "acidente_tempo_bom","acidente_tempo_ruim")])
    

## Organizando variaveis após agregação

# Padronizando variável de data
acidentes_concessoes <-  acidentes_concessoes %>%  
  mutate(dt = mdy(paste0(mes_ocorrencia,"/","01/",ano_ocorrencia)),
         data_inversa = as.monthly(dt),
         ym_concessao = as.monthly(ano_inicio),
         ym_pedagio = as.monthly(inicio_pedagio)) %>% 
  dplyr::rename(data = data_inversa)

# Seperando ano e mes de acidente, concessao e pedágio
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


#### 5) Balanceamento --------------------------------------------------------

# Fazer com que todos os meses tenham informações para as rodovias
acidentes_concessoes <- acidentes_concessoes %>% 
  dplyr::group_by(id_br_uf) %>% statar::fill_gap(data, full = TRUE)

# Fix variable and drop repeated variable
acidentes_concessoes <- acidentes_concessoes %>%
  select(-c(br.1,uf.1,id_br_uf.1,grupo_tratado.1,
                           ano_ocorrencia.1, mes_ocorrencia.1))
#area_acidente_urb

# Transformar NA em zero
acidentes_concessoes[,15:48] <- lapply(acidentes_concessoes[,15:48],
                                      function(n) ifelse(is.na(n),0,n))


# Organizando a base pós balanceada
acidentes_concessoes <- acidentes_concessoes %>%
  group_by(id_br_uf) %>% 
  fill(br,uf,min_km,max_km,ano_inicio, mes_ocorrencia,  ano_ocorrencia,
       grupo_tratado, data_concessao, extensao, yr_start,ano_acidente,
       inicio_pedagio,ano_pedagio,mes_pedagio,ano_concessao,
       mes_concessao,mes_acidente,dia_acidente,ym_concessao,
       .direction = "downup")



#### 6) Gerando variaveis para tratamento ------------------------------------

## Gerando tempo de exposição
acidentes_concessoes <- acidentes_concessoes %>% 
  mutate(tempo_exposicao = ((ano_acidente-yr_start)+1))

acidentes_concessoes <- acidentes_concessoes %>% 
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

acidentes_concessoes <- acidentes_concessoes %>% 
  mutate(treat_1yr_10yrs = ifelse(is.na(treat_1yr_10yrs),0,treat_1yr_10yrs))



# Leads and lags
acidentes_concessoes <- acidentes_concessoes %>% 
  mutate(treat_1yrbef =  ifelse(tempo_exposicao==-1,1,0),
         treat_2yrbef =  ifelse(tempo_exposicao==-2,1,0),
         treat_3yrbef =  ifelse(tempo_exposicao==-3,1,0),
         treat_4yrbef =  ifelse(tempo_exposicao==-4,1,0),
         treat_5yrbef =  ifelse(tempo_exposicao==-5,1,0),
         treat_6yrbef =  ifelse(tempo_exposicao==-6,1,0))



# Correção de qualquer problema nas variáveis de data
setDT(acidentes_concessoes)
acidentes_concessoes[, ano_inicio := as.Date(ano_inicio,  origin = "1899-12-30")]
acidentes_concessoes[, data_acidente := as.Date(data_acidente,  origin = "1899-12-30")]
acidentes_concessoes[, data_concessao := as.Date(data_concessao,  origin = "1899-12-30")]



# Rodovias que tiveram concessão devem ser controle antes do inicio da concessão
acidentes_concessoes[ , treat := ifelse(data >= ym_concessao & grupo_tratado ==1, 1, 0) ]
acidentes_concessoes[ , treat := ifelse(is.na(treat), 0, treat) ]

# Mesmo procedimento para o pedágio
acidentes_concessoes[ , treat_pedag := ifelse(data >= ym_pedagio & grupo_tratado == 1, 1, 0) ]
acidentes_concessoes[ , treat_pedag := ifelse(is.na(treat_pedag), 0, treat_pedag) ]


# Gen day of the week
acidentes_concessoes <- mutate(acidentes_concessoes,
                        day_of_week = weekdays(as.Date(data)),
                        month_pedagio = month.abb[mes_pedagio],
                        month_concessao = month.abb[mes_concessao],
                        ym_acidente = zoo::as.yearmon(
                        paste(ano_acidente, mes_acidente), "%Y %m"))

#### 7) ler covariadas ---------------------------------------------------

source('./R/covs.R', encoding = 'UTF-8' )

## Incluir covariadas
# Covariadas anuais
base_completa <- acidentes_concessoes %>% left_join(cov_anual)

# Covariadas mensais
base_completa <- base_completa %>% left_join(cov_mensal)

# Extensao Situacao
base_completa <- base_completa %>% mutate(extensao_situacao = max_km - min_km)

# Organização da base: transformação em logaritmo ou ponderação
base_completa <- base_completa %>% 
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
         morte_pessoas  = mortos/pessoas,
         feridos = feridos_leves + feridos_graves,
         feridos_km = feridos/extensao_situacao,
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


# Transformando em caractere
base_completa <- base_completa %>% 
  mutate(ano_acidente    = as.character(ano_acidente),
         dia_acidente    = as.character(dia_acidente),
         mes_acidente    = as.character(mes_acidente),
         id_br_uf        = as.character(id_br_uf),
         treat_1yr_10yrs = as.character(treat_1yr_10yrs))

# ID Estado

base_completa <- base_completa %>% left_join(read_state() %>% as.data.frame() %>% 
                             select(-c(geom)),
                           by = c('uf'="abbrev_state"))

# Remover todas as bases anteriores
rm(acidentes,acidentes_concessoes,cov_anual,cov_mensal)