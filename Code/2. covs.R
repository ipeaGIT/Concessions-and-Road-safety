## Lendo Covariadas

# Usando geobr para informações dos municípios do Brasil
mun_brasil <- geobr::read_municipality(code_muni = 'all') %>% 
  as.data.frame() %>% dplyr::rename(Codigo = code_muni) %>% 
  mutate(Codigo = as.numeric(substr(Codigo,1,6)))

# Gerando centróides
mun_brasil <-  cbind(mun_brasil,
                     st_centroid(mun_brasil$geom)) %>% 
  select(-c(geom)) %>% dplyr::rename(geom_urb = geometry)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#aqui nao tinha q considerar tambem as 
#Grande concentração urbana considerada na análise - Primeira integração do arranjo
# Mas não esta considerando

# Base de dados da REGIC 2018
regic <- read_excel("./Database/população/Arranjos_Populacionais_considerados_no_REGIC_2018.xlsx") %>% 
  filter(TipoConsidAnalise == "Grande concentração urbana considerada na análise - Arranjo populacional" |
         TipoConsidAnalise == "Grande concentração urbana considerada na análise - Segunda integração do arranjo") %>% 
  mutate(Codigo = as.numeric(substr(CodMunic,1,6))) %>% 
  left_join(mun_brasil)

rm(mun_brasil)


# População (censo)

# População municipal por faixa de idade
pop_idade <- read_excel("./Database/população/tabela200.xlsx",sheet=1) %>% 
  select(-c(Total)) %>% 
  dplyr::rename(Codigo = code_muni) %>% 
  mutate(Codigo = as.numeric(substr(Codigo,1,6))) 

# População municipal por sexo
pop_sexo <- read_excel("./Database/população/tabela200 (1).xlsx",sheet=1) %>% 
  select(-c(Total)) %>% 
  dplyr::rename(Codigo = code_muni) %>% 
  mutate(Codigo = as.numeric(substr(Codigo,1,6)))

## Censo Agro
censo_agro_valor <- read_excel("./Database/censo agro/Tabela2266.xlsx", sheet = 1) %>% 
  filter(mes!="Total") %>% group_by(code_muni) %>% filter(Total==max(Total)) %>% 
  select(c(code_muni,num_mes)) %>% 
  dplyr::rename(mes_valor = num_mes,
                Codigo = code_muni) %>% 
  mutate(Codigo = as.numeric(substr(Codigo,1,6)))

censo_agro_area <- read_excel("./Database/censo agro/Tabela2266.xlsx", sheet = 2) %>% 
  filter(mes!="Total") %>% group_by(code_muni) %>% filter(Total==max(Total)) %>% 
  select(c(code_muni,num_mes)) %>% 
  dplyr::rename(mes_area = num_mes,
                Codigo = code_muni) %>% 
  mutate(Codigo = as.numeric(substr(Codigo,1,6)))

## Covariadas

covs <- readRDS("./Database/covs.rds") %>% 
  left_join(pop_idade) %>% left_join(pop_sexo) %>% 
  left_join(censo_agro_valor) %>% left_join(censo_agro_area)


## Lendo apenas base de grupo de tratamento

# ler base de rodovias concedidas
rodovias <- read_excel("./Database/ST_DNIT_Rodovias_SNV2015_03.xlsx",sheet=2) %>% 
  mutate(br = as.numeric(br)) 


# retira empresas missing e linhas reptidas
rodovias <- filter(rodovias, !is.na(Empresa)) %>%
  distinct(br,uf,km_inicial,km_final, .keep_all = TRUE) 
setDT(rodovias)

## Lendo base de rodovias geral

temp <- rodovias %>% select(c(codigo,Empresa)) # Arquivo temp para join

dnit <- read_sf(dsn="./Database/Shapefiles/ST_DNIT_Rodovias_SNV2015_03.shp") %>%
  left_join(temp, by = c("codigo"="codigo")) %>%
  mutate(concessao = ifelse(!is.na(Empresa),1,0))

rm(temp)

mapa <- read_municipality(code_muni = "all", year = 2017) #geobr

# Arquivo temporariorio para o intersection
## Saber quais municipios tem cruzamento de rodovia
dnit <- st_transform(dnit, crs = 4326)
mapa <- st_transform(mapa, crs = 4326)

temp <- mapa %>% 
  group_by(code_muni) %>% sf::st_intersection(dnit) %>%
  st_sf

detach(package:plyr)
temp <-temp %>%
  dplyr::group_by(br,uf,Empresa,concessao,code_muni) %>% 
  st_as_sf() %>% 
  dplyr::summarize(km_inicial = min(km_inicial, na.rm = T),
                   km_final   = max(km_final, na.rm = T),
                   extensao   = sum(extensao, na.rm = T)) %>% 
  mutate(extensao = km_final - km_inicial)

# Agregacao das distancias das estradas

cumprimento <- temp %>% as.data.frame() %>% 
  mutate(dist = as.numeric(st_length(geom))) %>% 
  select(c(code_muni,dist,concessao,br,uf,Empresa,geom)) %>% 
  group_by(code_muni, concessao) %>% st_as_sf() %>% 
  dplyr::summarize(dist = sum(dist, na.rm = TRUE))

# Criar variavel de distancia para o municipio urbano mais proximo

estrada_cump <- cbind(cumprimento,st_centroid(cumprimento$geom)) %>% 
  as.data.frame() %>% select(-c(geom)) %>%
  mutate(code_muni = as.numeric(substr(code_muni,1,6))) %>%
  dplyr::rename(geom_cump = geometry)

regic <- filter(regic, !is.na(geom_urb)) %>% 
  select(Codigo, geom_urb) %>% dplyr::rename(geom = geom_urb) %>% 
  st_as_sf() %>% st_transform(crs = 3857)

estrada_cump <- filter(estrada_cump, !is.na(geom_cump)) %>% 
  select(code_muni,geom_cump) %>% dplyr::rename(geom = geom_cump) %>% 
  st_as_sf() %>% st_transform(crs = 3857)

estrada_cump$distancia_urbana <- apply(
  st_distance(estrada_cump,regic), 1, which.min)

estrada_cump <- estrada_cump %>% 
  as.data.frame() %>% select(-c(geom))

### Gerar ponderacao para as variaveis de controle municipal:

## Municipios que tem tratado e controle:
#  A ponderacao será pela porcentagem de representacao da rodovia
#  tratado vs controle para o controle

cumprimento <- as.data.table(cumprimento) %>%
  select(c(code_muni,dist,concessao)) %>% 
  group_by(code_muni) %>% 
  mutate(perc_dist = dist/sum(dist,na.rm = T))

## Municipios que tem tratado e controle:
### Solucao secundaria: dividir por dois
#### Exemplo: Mun que tem trat e controle divide o pib por dois..
####          Metade do pib sera tratado e outra metade controle

duplicadas <- cumprimento %>% group_by(code_muni) %>% 
  mutate(n = 1) %>% 
  dplyr::summarize(n = sum(n, na.rm = TRUE)) %>% filter(n>1)

cumprimento <- cumprimento %>% 
  left_join(duplicadas, by =c("code_muni"="code_muni")) %>% 
  ungroup() %>% 
  mutate(n = ifelse(is.na(n),1,n),
         code_muni = as.numeric(substr(code_muni,1,6)))

## Adicionar as covariadas

base <- covs %>% left_join(cumprimento, by = c("Codigo"="code_muni")) %>%
  left_join(estrada_cump, c("Codigo"="code_muni")) %>% 
  filter(!is.na(n)) %>% 
  mutate(pop = pop*perc_dist,
         empregados = empregados*perc_dist,
         emp = emp*perc_dist,tam = tam*perc_dist,
         pib = pib*perc_dist,ind = ind*perc_dist,
         agri= agri*perc_dist,vab = vab*perc_dist,
         serv = serv*perc_dist)

## Criando temp2 para fazer join com base n Stata

temp2 <- mapa %>% group_by(code_muni) %>% sf::st_intersection(dnit) %>%
  st_sf  %>% as.data.frame() %>% 
  group_by(br,uf,Empresa,concessao,code_muni) %>% 
  dplyr::summarise(km_inicial = min(km_inicial, na.rm = T),
                   km_final = max(km_final, na.rm = T),
                   extensao = sum(extensao, na.rm = T)) %>% 
  distinct(br,uf,km_inicial,km_final, .keep_all = TRUE) %>% 
  mutate(code_muni = as.numeric(substr(code_muni,1,6)))

cov <- base %>% 
  left_join(temp2, by = c("Codigo"="code_muni"))

# Organizacao da base

cov <- cov %>% 
        dplyr::rename(grupo_tratado = concessao.x, codigo_IBGE = Codigo) %>% 
  filter(!is.na(br)) %>% 
  select(-c(n,Empresa,km_inicial,km_final)) %>% 
  mutate(ano = as.numeric(ano),
         extensao_pop = extensao*pop)

# Leitura dos controles do clima

clima <- read_dta("./database/base_clima_br_1950_2017.dta") %>% 
  select(-c(uf))

cov <- cov %>% 
  filter(ano<=2017)

# Juncao da base

covs <- cov %>% left_join(clima) %>%
  select(c(pop0_a_4,pop5_a_9,pop10_a_14,pop15_a_19,pop20_a_24,
           pop25_a_29,pop30_a_34,pop35_a_39,pop40_a_44,pop45_a_49,
           pop50_a_54,pop55_a_59,pop60_a_64,pop65_a_69,pop70_a_74,
           pop75_a_79,pop80_a_84,pop85_a_89,pop90_a_94,pop95_a_99,
           Homem,Mulher,v_temp,v_temp_01,
           v_temp_02,v_temp_03,v_temp_04,v_temp_05,
           v_temp_06,v_temp_07,v_temp_08,v_temp_09,v_temp_10,v_temp_11,
           v_temp_12,v_rain,v_rain_01,v_rain_02,v_rain_03,v_rain_04,
           v_rain_05, v_rain_06, v_rain_07, v_rain_08, v_rain_09,
           v_rain_10, v_rain_11, v_rain_12,pop, empregados, emp,
           pib, ind, agri, vab, serv, distancia_urbana,
           uf, br, grupo_tratado, ano, mes_valor,mes_area,extensao,extensao_pop)) %>% 
  group_by(uf,br,grupo_tratado,ano) %>% 
  dplyr::summarise(v_temp_extension = weighted.mean(v_temp,w=extensao,na.rm = T), v_rain_extension = weighted.mean(v_rain,extensao,na.rm = T),
                   v_temp_extension_02 = weighted.mean(v_temp_02,w=extensao,na.rm = T), v_temp_extension_03 = weighted.mean(v_temp_03,w=extensao,na.rm = T),
                   v_temp_extension_04 = weighted.mean(v_temp_04,w=extensao,na.rm = T), v_temp_extension_05 = weighted.mean(v_temp_05,w=extensao,na.rm = T),
                   v_temp_extension_06 = weighted.mean(v_temp_06,w=extensao,na.rm = T), v_temp_extension_07 = weighted.mean(v_temp_07,w=extensao,na.rm = T),
                   v_temp_extension_08 = weighted.mean(v_temp_08,w=extensao,na.rm = T), v_temp_extension_09 = weighted.mean(v_temp_09,w=extensao,na.rm = T),
                   v_temp_extension_10 = weighted.mean(v_temp_10,w=extensao,na.rm = T), v_temp_extension_11 = weighted.mean(v_temp_11,w=extensao,na.rm = T),
                   v_temp_extension_12 = weighted.mean(v_temp_12,w=extensao,na.rm = T), v_temp_extension_01 = weighted.mean(v_temp_01,w=extensao,na.rm = T),
                   v_rain_extension_01 = weighted.mean(v_rain_01,w=extensao,na.rm = T),  v_rain_extension_02 = weighted.mean(v_rain_02,w=extensao,na.rm = T),
                   v_rain_extension_03 = weighted.mean(v_rain_03,w=extensao,na.rm = T),  v_rain_extension_04 = weighted.mean(v_rain_04,w=extensao,na.rm = T),
                   v_rain_extension_05 = weighted.mean(v_rain_05,w=extensao,na.rm = T),  v_rain_extension_06 = weighted.mean(v_rain_06,w=extensao,na.rm = T),
                   v_rain_extension_07 = weighted.mean(v_rain_07,w=extensao,na.rm = T),  v_rain_extension_08 = weighted.mean(v_rain_08,w=extensao,na.rm = T),
                   v_rain_extension_09 = weighted.mean(v_rain_09,w=extensao,na.rm = T),  v_rain_extension_10 = weighted.mean(v_rain_10,w=extensao,na.rm = T),
                   v_rain_extension_11 = weighted.mean(v_rain_11,w=extensao,na.rm = T),  v_rain_extension_12 = weighted.mean(v_rain_12,w=extensao,na.rm = T),
                   v_temp = mean(v_temp,na.rm = T), v_rain    = sum (v_rain,na.rm = T),
                   v_temp_01 = mean(v_temp_01,na.rm = T),
                   v_temp_02 = mean(v_temp_02,na.rm = T), v_temp_03 = mean(v_temp_03,na.rm = T),
                   v_temp_04 = mean(v_temp_04,na.rm = T), v_temp_05 = mean(v_temp_05,na.rm = T),
                   v_temp_06 = mean(v_temp_06,na.rm = T), v_temp_07 = mean(v_temp_07,na.rm = T),
                   v_temp_08 = mean(v_temp_08,na.rm = T), v_temp_09 = mean(v_temp_09,na.rm = T),
                   v_temp_10 = mean(v_temp_10,na.rm = T), v_temp_11 = mean(v_temp_11,na.rm = T),
                   v_temp_12 = mean(v_temp_12,na.rm = T), v_rain    = sum (v_rain,na.rm = T),
                   v_rain_01 = sum(v_rain_01,na.rm = T),  v_rain_02 = sum (v_rain_02,na.rm = T),
                   v_rain_03 = sum(v_rain_03,na.rm = T),  v_rain_04 = sum (v_rain_04,na.rm = T),
                   v_rain_05 = sum(v_rain_05,na.rm = T),  v_rain_06 = sum (v_rain_06,na.rm = T),
                   v_rain_07 = sum(v_rain_07,na.rm = T),  v_rain_08 = sum (v_rain_08,na.rm = T),
                   v_rain_09 = sum(v_rain_09,na.rm = T),  v_rain_10 = sum (v_rain_10,na.rm = T),
                   v_rain_11 = sum(v_rain_11,na.rm = T),  v_rain_12 = sum (v_rain_12,na.rm = T),
                   pop0_a_4 = sum(pop0_a_4,na.rm = T),pop5_a_9 = sum(pop5_a_9,na.rm = T),
                   pop10_a_14 = sum(pop10_a_14,na.rm = T),pop15_a_19 = sum(pop15_a_19,na.rm = T),
                   pop20_a_24 = sum(pop20_a_24,na.rm = T),pop25_a_29 = sum(pop25_a_29,na.rm = T),
                   pop30_a_34 = sum(pop30_a_34,na.rm = T),pop35_a_39 = sum(pop35_a_39,na.rm = T),
                   pop40_a_44 = sum(pop40_a_44,na.rm = T),pop45_a_49 = sum(pop45_a_49,na.rm = T),
                   pop50_a_54 = sum(pop50_a_54,na.rm = T),pop55_a_59 = sum(pop55_a_59,na.rm = T),
                   pop60_a_64 = sum(pop60_a_64,na.rm = T),pop65_a_69 = sum(pop65_a_69,na.rm = T),
                   pop70_a_74 = sum(pop70_a_74,na.rm = T),pop75_a_79 = sum(pop75_a_79,na.rm = T),
                   pop80_a_84 = sum(pop80_a_84,na.rm = T),pop85_a_89 = sum(pop85_a_89,na.rm = T),
                   pop90_a_94 = sum(pop90_a_94,na.rm = T),pop95_a_99 = sum(pop95_a_99,na.rm = T),
                   Homem = sum(Homem,na.rm = T),Mulher = sum(Mulher,na.rm = T),
                   pop_extension        = weighted.mean(pop,w=extensao,na.rm = T),
                   empregados_extension = weighted.mean(empregados,w=extensao,na.rm = T),
                   emp_extension        = weighted.mean(emp,w=extensao,na.rm = T),
                   pib_extension        = weighted.mean(pib,w=dplyr::coalesce(extensao,0),na.rm = T),
                   pib_extension_pop    = weighted.mean(pib,w=extensao_pop,na.rm = T),
                   ind_extension        = weighted.mean(ind,w=extensao,na.rm = T),
                   agri_extension       = weighted.mean(agri,w=extensao,na.rm = T),
                   vab_extension        = weighted.mean(vab,w=extensao,na.rm = T),
                   serv_extension       = weighted.mean(serv,w=extensao,na.rm = T),
                   pop = sum(pop,na.rm = T),  empregados = sum (empregados,na.rm = T),
                   emp = sum(emp,na.rm = T),  pib        = sum (pib,na.rm = T),
                   ind = sum(ind,na.rm = T),  agri       = sum (agri,na.rm = T),
                   vab = sum(vab,na.rm = T),  serv       = sum (serv,na.rm = T),
                   distancia_urbana = mean(distancia_urbana, na.rm = T),
                   mes_area = mean(mes_area,na.rm=T),
                   mes_valor = mean(mes_valor,na.rm=T)) %>% ungroup() %>% 
  mutate(ln_pop = log(pop+1),ln_empregados = log(empregados+1),
         ln_emp = log(emp+1),ln_pib = log(pib+1),ln_ind = log(ind+1),
         ln_agri= log(agri+1),ln_vab = log(vab+1),ln_serv = log(serv+1),
         ln_dist_urb = log(distancia_urbana+1),
         br = as.numeric(br),
         mes_area = round(mes_area, digits = 0),
         mes_valor = round(mes_valor, digits = 0)) %>% 
  dplyr::rename(ano_acidente = ano) %>% ungroup() %>% 
  filter(ano_acidente!=2018 | ano_acidente!=2006) 

# Cov anual

cov_anual <- covs %>% select(-c(
                               v_temp_extension_01,v_temp_extension_02,
                               v_temp_extension_03,v_temp_extension_04,
                               v_temp_extension_05,v_temp_extension_06,
                               v_temp_extension_07,v_temp_extension_08,
                               v_temp_extension_09,v_temp_extension_10,
                               v_temp_extension_11,v_temp_extension_12,
                               v_rain_extension_01,v_rain_extension_02,
                               v_rain_extension_03,v_rain_extension_04,
                               v_rain_extension_05,v_rain_extension_06,
                               v_rain_extension_07,v_rain_extension_08,
                               v_rain_extension_09,v_rain_extension_10,
                               v_rain_extension_11,v_rain_extension_12,
                               v_temp_01,v_temp_02,v_temp_03,v_temp_04,
                               v_temp_05,v_temp_06,v_temp_07,v_temp_08,
                               v_temp_09,v_temp_10,v_temp_11,v_temp_12,
                               v_rain_01,v_rain_02,v_rain_03,v_rain_04,
                               v_rain_05,v_rain_06,v_rain_07,v_rain_08,
                               v_rain_09,v_rain_10,v_rain_11,v_rain_12))

# Cov mensal

cov_mensal <- covs %>% select(c(uf,br,grupo_tratado,ano_acidente,
                               v_temp_extension_01,v_temp_extension_02,
                               v_temp_extension_03,v_temp_extension_04,
                               v_temp_extension_05,v_temp_extension_06,
                               v_temp_extension_07,v_temp_extension_08,
                               v_temp_extension_09,v_temp_extension_10,
                               v_temp_extension_11,v_temp_extension_12,
                               v_rain_extension_01,v_rain_extension_02,
                               v_rain_extension_03,v_rain_extension_04,
                               v_rain_extension_05,v_rain_extension_06,
                               v_rain_extension_07,v_rain_extension_08,
                               v_rain_extension_09,v_rain_extension_10,
                               v_rain_extension_11,v_rain_extension_12,
                               v_temp_01,v_temp_02,v_temp_03,v_temp_04,
                               v_temp_05,v_temp_06,v_temp_07,v_temp_08,
                               v_temp_09,v_temp_10,v_temp_11,v_temp_12,
                               v_rain_01,v_rain_02,v_rain_03,v_rain_04,
                               v_rain_05,v_rain_06,v_rain_07,v_rain_08,
                               v_rain_09,v_rain_10,v_rain_11,v_rain_12)) %>% 
  gather("var","valor",-uf,-br,-grupo_tratado,-ano_acidente) %>% 
  mutate(tipo = ifelse(nchar(var)==9,substr(var,1,6),substr(var,1,16)),
         mes_acidente = ifelse(nchar(var)==9,substr(var,8,9),substr(var,18,19))) %>% 
  spread(tipo,valor) %>% 
  select(-c(var)) %>% 
  group_by(uf,br,grupo_tratado,ano_acidente,mes_acidente) %>% 
  dplyr::summarise(v_rain_mensal = sum(v_rain,na.rm=T),
                   v_rain_extension_mensal = sum(v_rain_extension,na.rm=T) ,
                   v_temp_mensal = sum(v_temp,na.rm=T) ,
                   v_temp_extension_mensal = sum(v_temp_extension, na.rm=T)) %>% 
  mutate(mes_acidente = as.numeric(mes_acidente))

# Limpeza da base

rm(base,cov,cumprimento,dnit,temp,temp2,
   clima,duplicadas,mapa,rodovias,regic,
   estrada_cump,pop_idade,pop_sexo,
   censo_agro_area,censo_agro_valor,covs)

# Leitura em rds e stata

haven::write_dta(cov_anual,"./database/cov_anual.dta")
write_rds(cov_anual,"./database/cov_anual.rds")


haven::write_dta(cov_mensal,"./database/cov_mensal.dta")
write_rds(cov_mensal,"./database/cov_mensal.rds")
