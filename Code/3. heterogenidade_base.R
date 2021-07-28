## Dia/noite

pre_temp <- pre_temp %>% 
  mutate(area_acidente_urb = ifelse(acidente_urb==1,1,NA),
         area_acidente_urb = ifelse(acidente_rur==1,0,area_acidente_urb),
         acidente_dia      = ifelse(acidente_pelodia==1,1,NA),
         acidente_dia      = ifelse(acidente_pelanoite==1,0,acidente_dia))

acidentes_concessoes_dia <- pre_temp %>% 
  filter(!is.na(acidente_dia)) %>% 
  ungroup() %>% 
  mutate(id_br_uf_dia = paste0(id_br_uf,acidente_dia))

acidentes_concessoes_dia <- cbind(setDT(acidentes_concessoes_dia)[, lapply(.SD, mean, na.rm=TRUE),
                                                          by=list(id_br_uf_dia,acidente_dia,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                                          .SDcols=c("inicio_pedagio","yr_start","data_concessao",
                                                                    "min_km","max_km",
                                                                    "ano_inicio","ano_acidente","extensao")],
                              setDT(acidentes_concessoes_dia)[, lapply(.SD, sum, na.rm=TRUE),
                                                          by=list(id_br_uf_dia,acidente_dia,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                                          .SDcols=c("acidente","pessoas","mortos",
                                                                    "area_acidente_urb","area_acidente_dia",
                                                                    "feridos_leves","feridos_graves",
                                                                    "ilesos","masculino","feminino","motocicleta",
                                                                    "automovel","caminhao","onibus","outros",
                                                                    "idade_10","idade_20","idade_30","idade_40",
                                                                    "idade_50","idade_60","idade_120","via_pavimentada",
                                                                    "via_duplicada","acidente_pelodia","acidente_pelanoite",
                                                                    "acidente_urb","acidente_rur","acidente_pistasimp",
                                                                    "acidente_pistamult","acidente_dentrm","acidente_forarm",
                                                                    "acidente_tempo_bom","acidente_tempo_ruim")] %>% 
                                select(-c(id_br_uf_dia,acidente_dia,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia)))




# Standardizing date variable
acidentes_concessoes_dia <-  acidentes_concessoes_dia %>%  
  mutate(dt = mdy(paste0(mes_ocorrencia,"/","01/",ano_ocorrencia)),
         data_inversa = as.monthly(dt),
         ym_concessao = as.monthly(ano_inicio),
         ym_pedagio = as.monthly(inicio_pedagio)) %>% 
  dplyr::rename(data = data_inversa)

# Separating year and month of accident, concession and toll
acidentes_concessoes_dia <-  acidentes_concessoes_dia %>% 
  mutate(data_acidente = dt,
         ano_pedagio   = as.numeric(substr(inicio_pedagio,1,4)),
         mes_pedagio   = as.numeric(substr(inicio_pedagio,6,7)),
         ano_concessao = as.numeric(substr(data_concessao,1,4)),
         mes_concessao = as.numeric(substr(data_concessao,6,7)),
         ano_acidente  = as.numeric(substr(data_acidente,1,4)),
         mes_acidente  = as.numeric(substr(data_acidente,6,7)),
         dia_acidente  = as.numeric(substr(data_acidente,9,10)),
         yr_start      = as.numeric(substr(ano_inicio,1,4)))

# Fazer com que todos os meses tenham informações para as rodovias
acidentes_concessoes_dia <- acidentes_concessoes_dia  %>% 
  dplyr::group_by(id_br_uf_dia) %>% statar::fill_gap(data, full = TRUE)

#area_acidente_urb

# Transformar NA em zero
acidentes_concessoes_dia[,15:48] <- lapply(acidentes_concessoes_dia[,15:48],
                                             function(n) ifelse(is.na(n),0,n))

#acidentes_concessoes_dia <- acidentes_concessoes_dia %>% 
#  mutate(acidente_dia = ifelse(is.na(acidente_dia),0,acidente_dia))

# Organizando a base pós balanceada
acidentes_concessoes_dia <- acidentes_concessoes_dia %>%
  group_by(id_br_uf_dia) %>% 
  fill(br,uf,min_km,max_km,ano_inicio, mes_ocorrencia,  ano_ocorrencia,
       grupo_tratado, data_concessao, extensao, yr_start,ano_acidente,
       inicio_pedagio,ano_pedagio,mes_pedagio,ano_concessao,
       mes_concessao,mes_acidente,dia_acidente,ym_concessao,
       .direction = "downup")



#### 6) Gerando variaveis para tratamento ------------------------------------

## Ajuste da variável de dada
acidentes_concessoes_dia <-  acidentes_concessoes_dia %>% 
  mutate(ano_acidente = ifelse(ano_acidente==0,ano_ocorrencia,ano_acidente))

## Gerando tempo de exposição
acidentes_concessoes_dia <- acidentes_concessoes_dia %>% 
  mutate(tempo_exposicao = ((ano_acidente-yr_start)+1))

acidentes_concessoes_dia <- acidentes_concessoes_dia %>% 
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

acidentes_concessoes_dia <- acidentes_concessoes_dia %>% 
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
acidentes_concessoes_dia <- acidentes_concessoes_dia %>% 
  mutate(treat_1yrbef =  ifelse(tempo_exposicao==-1,1,0),
         treat_2yrbef =  ifelse(tempo_exposicao==-2,1,0),
         treat_3yrbef =  ifelse(tempo_exposicao==-3,1,0),
         treat_4yrbef =  ifelse(tempo_exposicao==-4,1,0),
         treat_5yrbef =  ifelse(tempo_exposicao==-5,1,0),
         treat_6yrbef =  ifelse(tempo_exposicao==-6,1,0))



# Correção de qualquer problema nas variáveis de data
setDT(acidentes_concessoes_dia)
acidentes_concessoes_dia[, ano_inicio := as.Date(ano_inicio,  origin = "1899-12-30")]
acidentes_concessoes_dia[, data_acidente := as.Date(data_acidente,  origin = "1899-12-30")]
acidentes_concessoes_dia[, data_concessao := as.Date(data_concessao,  origin = "1899-12-30")]



# Rodovias que tiveram concessão devem ser controle antes do inicio da concessão
acidentes_concessoes_dia[ , treat := ifelse(data >= ym_concessao & grupo_tratado ==1, 1, 0) ]
acidentes_concessoes_dia[ , treat := ifelse(is.na(treat), 0, treat) ]

# Mesmo procedimento para o pedágio
acidentes_concessoes_dia[ , treat_pedag := ifelse(data >= ym_pedagio & grupo_tratado == 1, 1, 0) ]
acidentes_concessoes_dia[ , treat_pedag := ifelse(is.na(treat_pedag), 0, treat_pedag) ]


# Gen day of the week
acidentes_concessoes_dia <- mutate(acidentes_concessoes_dia,
                                     day_of_week = weekdays(as.Date(data)),
                                     month_pedagio = month.abb[mes_pedagio],
                                     month_concessao = month.abb[mes_concessao],
                                     ym_acidente = zoo::as.yearmon(
                                       paste(ano_acidente, mes_acidente), "%Y %m"))

#### 7) ler covariadas ---------------------------------------------------

## Incluir covariadas
# Covariadas anuais
base_dia <- acidentes_concessoes_dia %>% left_join(cov_anual)

# Covariadas mensais
base_dia <- base_dia %>% left_join(cov_mensal)

# Extensao Situacao
base_dia <- base_dia %>% mutate(extensao_situacao = max_km - min_km)

# Organização da base: transformação em logaritmo ou ponderação
base_dia <- base_dia %>% 
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

base_dia <- base_dia %>%
  mutate(acidente_km = ifelse(is.na(acidente_km),0,acidente_km),
         pessoas_km = ifelse(is.na(pessoas_km),0,pessoas_km),
         mortos_km = ifelse(is.na(mortos_km),0,mortos_km),
         veiculos_km = ifelse(is.na(veiculos_km),0,veiculos_km),
         feridos_leves_km = ifelse(is.na(feridos_leves_km),0,feridos_leves_km),
         feridos_graves_km = ifelse(is.na(feridos_graves_km),0,feridos_graves_km),
         ilesos_km = ifelse(is.na(ilesos_km),0,ilesos_km),
         feridos_km = ifelse(is.na(feridos_km),0,feridos_km))

# Transformando em caractere
base_dia <- base_dia %>% 
  mutate(ano_acidente = as.character(ano_acidente),
         dia_acidente    = as.character(dia_acidente),
         mes_acidente    = as.character(mes_acidente),
         id_br_uf_dia   = as.character(id_br_uf_dia),
         treat_1yr_10yrs = as.character(treat_1yr_10yrs))

# ID Estado

base_dia <- base_dia %>% left_join(read_state() %>% as.data.frame() %>% 
                             select(-c(geom)),
                           by = c('uf'="abbrev_state"))



# Acidente totais
base_dia <- base_dia %>% 
  group_by(id_br_uf_dia) %>% 
  mutate(acidente_tot = sum(acidente, na.rm=T))


## Urbano/rural ----------------

acidentes_concessoes_urbano <- pre_temp %>% 
  filter(!is.na(area_acidente_urb)) %>% 
  ungroup() %>% 
  mutate(id_br_uf_urb = paste0(id_br_uf,"_",area_acidente_urb))

acidentes_concessoes_urbano <- cbind(setDT(acidentes_concessoes_urbano)[, lapply(.SD, mean, na.rm=TRUE),
                                                                  by=list(id_br_uf_urb,area_acidente_urb,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                                                  .SDcols=c("inicio_pedagio","yr_start","data_concessao",
                                                                            "min_km","max_km",
                                                                            "ano_inicio","ano_acidente","extensao")],
                                  setDT(acidentes_concessoes_urbano)[, lapply(.SD, sum, na.rm=TRUE),
                                                                  by=list(id_br_uf_urb,area_acidente_urb,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia),
                                                                  .SDcols=c("acidente","pessoas","mortos",
                                                                            "area_acidente_urb","area_acidente_dia",
                                                                            "feridos_leves","feridos_graves",
                                                                            "ilesos","masculino","feminino","motocicleta",
                                                                            "automovel","caminhao","onibus","outros",
                                                                            "idade_10","idade_20","idade_30","idade_40",
                                                                            "idade_50","idade_60","idade_120","via_pavimentada",
                                                                            "via_duplicada","acidente_pelodia","acidente_pelanoite",
                                                                            "acidente_urb","acidente_rur","acidente_pistasimp",
                                                                            "acidente_pistamult","acidente_dentrm","acidente_forarm",
                                                                            "acidente_tempo_bom","acidente_tempo_ruim")] %>% 
                                    select(-c(id_br_uf_urb,area_acidente_urb,uf,br,grupo_tratado,mes_ocorrencia,ano_ocorrencia)))


#acidentes_concessoes_urbano[,c(18:24) := NULL]
#acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>% 
#  ungroup() %>% 
#  dplyr::select(-c(18:24))

# Standardizing date variable
acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>%  
  mutate(dt = mdy(paste0(mes_ocorrencia,"/","01/",ano_ocorrencia)),
         data_inversa = as.monthly(dt),
         ym_concessao = as.monthly(ano_inicio),
         ym_pedagio = as.monthly(inicio_pedagio)) %>% 
  dplyr::rename(data = data_inversa)

# Separating year and month of accident, concession and toll
acidentes_concessoes_urbano <-  acidentes_concessoes_urbano %>% 
  mutate(data_acidente = dt,
         ano_pedagio   = as.numeric(substr(inicio_pedagio,1,4)),
         mes_pedagio   = as.numeric(substr(inicio_pedagio,6,7)),
         ano_concessao = as.numeric(substr(data_concessao,1,4)),
         mes_concessao = as.numeric(substr(data_concessao,6,7)),
         ano_acidente  = as.numeric(substr(data_acidente,1,4)),
         mes_acidente  = as.numeric(substr(data_acidente,6,7)),
         dia_acidente  = as.numeric(substr(data_acidente,9,10)),
         yr_start      = as.numeric(substr(ano_inicio,1,4)))

acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>% 
  dplyr::group_by(id_br_uf_urb) %>% statar::fill_gap(data, full = TRUE) 


#area_acidente_urb

# Transformar NA em zero
acidentes_concessoes_urbano[,15:48] <- lapply(acidentes_concessoes_urbano[,15:48],
                                             function(n) ifelse(is.na(n),0,n))


# Organizando a base pós balanceada
acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>%
  group_by(id_br_uf_urb) %>% 
  fill(br,uf,min_km,max_km,ano_inicio, mes_ocorrencia,  ano_ocorrencia,
       grupo_tratado, data_concessao, extensao, yr_start,ano_acidente,
       inicio_pedagio,ano_pedagio,mes_pedagio,ano_concessao,
       mes_concessao,mes_acidente,dia_acidente,ym_concessao,
       .direction = "downup")



#### 6) Gerando variaveis para tratamento ------------------------------------

## Ajuste da variável de dada
acidentes_concessoes_urbano <-  acidentes_concessoes_urbano %>% 
  mutate(ano_acidente = ifelse(ano_acidente==0,ano_ocorrencia,ano_acidente))

## Gerando tempo de exposição
acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>% 
  mutate(tempo_exposicao = ((ano_acidente-yr_start)+1))

acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>% 
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

acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>% 
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
acidentes_concessoes_urbano <- acidentes_concessoes_urbano %>% 
  mutate(treat_1yrbef =  ifelse(tempo_exposicao==-1,1,0),
         treat_2yrbef =  ifelse(tempo_exposicao==-2,1,0),
         treat_3yrbef =  ifelse(tempo_exposicao==-3,1,0),
         treat_4yrbef =  ifelse(tempo_exposicao==-4,1,0),
         treat_5yrbef =  ifelse(tempo_exposicao==-5,1,0),
         treat_6yrbef =  ifelse(tempo_exposicao==-6,1,0))



# Correção de qualquer problema nas variáveis de data
setDT(acidentes_concessoes_urbano)
acidentes_concessoes_urbano[, ano_inicio := as.Date(ano_inicio,  origin = "1899-12-30")]
acidentes_concessoes_urbano[, data_acidente := as.Date(data_acidente,  origin = "1899-12-30")]
acidentes_concessoes_urbano[, data_concessao := as.Date(data_concessao,  origin = "1899-12-30")]



# Rodovias que tiveram concessão devem ser controle antes do inicio da concessão
acidentes_concessoes_urbano[ , treat := ifelse(data >= ym_concessao & grupo_tratado ==1, 1, 0) ]
acidentes_concessoes_urbano[ , treat := ifelse(is.na(treat), 0, treat) ]

# Mesmo procedimento para o pedágio
acidentes_concessoes_urbano[ , treat_pedag := ifelse(data >= ym_pedagio & grupo_tratado == 1, 1, 0) ]
acidentes_concessoes_urbano[ , treat_pedag := ifelse(is.na(treat_pedag), 0, treat_pedag) ]


# Gen day of the week
acidentes_concessoes_urbano <- mutate(acidentes_concessoes_urbano,
                                     day_of_week = weekdays(as.Date(data)),
                                     month_pedagio = month.abb[mes_pedagio],
                                     month_concessao = month.abb[mes_concessao],
                                     ym_acidente = zoo::as.yearmon(
                                       paste(ano_acidente, mes_acidente), "%Y %m"))

#### 7) ler covariadas ---------------------------------------------------

## Incluir covariadas
# Covariadas anuais
base_urbano <- acidentes_concessoes_urbano %>% left_join(cov_anual)

# Covariadas mensais
base_urbano <- base_urbano %>% left_join(cov_mensal)

# Extensao Situacao
base_urbano <- base_urbano %>% mutate(extensao_situacao = max_km - min_km)

# Organização da base: transformação em logaritmo ou ponderação
base_urbano <- base_urbano %>% 
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

base_urbano <- base_urbano %>%
  mutate(acidente_km = ifelse(is.na(acidente_km),0,acidente_km),
         pessoas_km = ifelse(is.na(pessoas_km),0,pessoas_km),
         mortos_km = ifelse(is.na(mortos_km),0,mortos_km),
         veiculos_km = ifelse(is.na(veiculos_km),0,veiculos_km),
         feridos_leves_km = ifelse(is.na(feridos_leves_km),0,feridos_leves_km),
         feridos_graves_km = ifelse(is.na(feridos_graves_km),0,feridos_graves_km),
         ilesos_km = ifelse(is.na(ilesos_km),0,ilesos_km),
         feridos_km = ifelse(is.na(feridos_km),0,feridos_km))

# Transformando em caractere
base_urbano <- base_urbano %>% 
  mutate(ano_acidente = as.character(ano_acidente),
         dia_acidente    = as.character(dia_acidente),
         mes_acidente    = as.character(mes_acidente),
         id_br_uf_urb   = as.character(id_br_uf_urb),
         treat_1yr_10yrs = as.character(treat_1yr_10yrs))

# ID Estado

base_urbano <- base_urbano %>% left_join(read_state() %>% as.data.frame() %>% 
                             select(-c(geom)),
                           by = c('uf'="abbrev_state"))



# Acidente totais
base_urbano <- base_urbano %>% 
  group_by(id_br_uf_urb) %>% 
  mutate(acidente_tot = sum(acidente, na.rm=T),
         area_acidente_urb = ifelse(is.na(area_acidente_urb),0,area_acidente_urb))
