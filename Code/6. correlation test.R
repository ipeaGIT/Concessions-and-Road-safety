## Lendo Covariadas

# Usando geobr para informações dos municípios do Brasil
mun_brasil <- geobr::read_municipality(code_muni = 'all') %>% 
  as.data.frame() %>% dplyr::rename(Codigo = code_muni) %>% 
  mutate(Codigo = as.numeric(substr(Codigo,1,6)))

# Gerando centróides
mun_brasil <-  cbind(mun_brasil,
                     st_centroid(mun_brasil$geom)) %>% 
  select(-c(geom))

# Base de dados da REGIC 2018
regic <- read_excel("./Database/população/Arranjos_Populacionais_considerados_no_REGIC_2018.xlsx") %>% 
  filter(TipoConsidAnalise == "Grande concentração urbana considerada na análise - Arranjo populacional" |
           TipoConsidAnalise == "Grande concentração urbana considerada na análise - Segunda integração do arranjo") %>% 
  mutate(Codigo = as.numeric(substr(CodMunic,1,6))) %>% 
  left_join(mun_brasil) %>% st_as_sf()

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

regic <- st_transform(regic, crs = 4326)

# Read database of roads
dnit <- read_sf(dsn="./Database/Shapefiles/ST_DNIT_Rodovias_SNV2015_03.shp")
dnit <- st_transform(dnit, crs = 4326)
dnit <- dnit %>% 
  group_by(br, uf) %>% st_as_sf() %>% 
  dplyr::summarize(km_inicial=  min(km_inicial, na.rm = T),
                   km_final  =  max(km_final, na.rm = T)) %>% 
  mutate(extensao = km_final - km_inicial)

mapa <- read_municipality(code_muni = "all", year = 2017) #geobr

# Arquivo temporariorio para o intersection
## Saber quais municipios tem cruzamento de rodovia
dnit <- st_transform(dnit, crs = 4326)
mapa <- st_transform(mapa, crs = 4326)

mapa <- mapa %>% 
  group_by(code_muni) %>% sf::st_intersection(dnit) %>%
  st_sf

mapa <- mapa %>% as.data.frame() %>% select(-c(geom))
mapa <- mapa %>% mutate(Codigo = as.numeric(substr(code_muni,1,6)))
mapa <- mapa %>% select(c(Codigo,br,uf))
covs <- covs %>% filter(ano == "2017") %>% mutate(Codigo = as.numeric(Codigo))

roads <- left_join(mapa,covs)

roads <- roads %>% mutate(ano = as.numeric(ano))
  
clima <- read_dta("./database/base_clima_br_1950_2017.dta") %>% 
  select(-c(uf))

clima <- clima %>% dplyr::rename(Codigo = codigo_IBGE)

roads <- roads %>% left_join(clima) 
  
roads <- roads %>% 
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
           pib, ind, agri, vab, serv, 
           uf, br)) %>% 
  group_by(uf,br) %>% 
  dplyr::summarise(v_temp = mean(v_temp,na.rm = T), v_rain    = sum (v_rain,na.rm = T),
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
                   pop = sum(pop,na.rm = T),  empregados = sum (empregados,na.rm = T),
                   emp = sum(emp,na.rm = T),  pib        = sum (pib,na.rm = T),
                   ind = sum(ind,na.rm = T),  agri       = sum (agri,na.rm = T),
                   vab = sum(vab,na.rm = T),  serv       = sum (serv,na.rm = T)) %>%
  ungroup() %>% 
  mutate(ln_pop = log(pop+1),ln_empregados = log(empregados+1),
         ln_emp = log(emp+1),ln_pib = log(pib+1),ln_ind = log(ind+1),
         ln_agri= log(agri+1),ln_vab = log(vab+1),ln_serv = log(serv+1),
         br = as.numeric(br))



 dnit$distancia_urbana <- apply(
   st_distance(dnit$geometry,regic$geometry), 1, min)

dnit <- dnit %>% as.data.frame() %>%  select(-c(geometry))
dnit <- dnit %>% mutate(br = as.numeric(br))

saveRDS(dnit,"dnit.rds", compress = T)
dnit <- readRDS("dnit.rds")

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

volume <- volume %>% group_by(uf,br) %>% 
  dplyr::summarise(VMDa = sum(VMDa, na.rm = T),
                   VMDa_C = sum(VMDa_C, na.rm = T),
                   VMDa_D = sum(VMDa_D, na.rm = T))

dni <- dnit %>% left_join(volume) %>% filter(!is.na(VMDa))

roads <- roads %>% mutate(br = as.numeric(br))

roads <- roads %>% left_join(volume) %>% left_join(dnit) %>% 
  filter(!is.na(VMDa))

a <- dni$distancia_urbana
b <- dni$VMDa

summary(lm(VMDa ~ 1 + distancia_urbana, data = dni))

a <- roads %>% select(c(VMDa,v_temp,v_rain,ln_pib,ln_pop,ln_emp,distancia_urbana)) %>% 
  mutate(v_temp = ifelse(is.na(v_temp),0,v_temp))
cor(a, method = c("pearson", "kendall", "spearman"))

cor(a, b, method = c("pearson", "kendall", "spearman"))


names(roads)
summary(lm(VMDa ~ 1 + ln_pop, data = roads))

summary(lm(VMDa ~ 1 + ln_emp, data = roads))

summary(lm(VMDa ~ 1 +ln_pib, data = roads))

summary(lm(VMDa ~ 1 + distancia_urbana , data = roads))

summary(lm(VMDa ~ 1 + v_temp, data = roads))

summary(lm(VMDa ~ 1 + v_rain, data = roads))


