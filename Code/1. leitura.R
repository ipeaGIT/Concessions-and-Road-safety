## Ler todos os anos

# Read crashes data
Ano <- 2006
ocorrencia <- NULL
while(Ano != 2019){
    Ano <- Ano + 1
    x <- fread(paste0("./Database/original/datatran",Ano,".csv")) %>% 
      mutate(ano = Ano)
    ocorrencia <- rbind.fill(ocorrencia,x)
    rm(x)
}
rm(Ano)

ocorrencia <- ocorrencia %>% distinct(uf,br,km,
                                  municipio,id,
                                  data_inversa,dia_semana,
                                  horario,.keep_all = T) %>% 
  select(-c("causa_acidente","tipo_acidente","ignorados",
            "classificacao_acidente","sentido_via",
            "latitude","longitude","regional",
            "tracado_via","delegacia","uop")) %>% 
  mutate(km = gsub(",",".",km),
         br = as.numeric(br))

ocorrencia <- ocorrencia %>% mutate(km = as.numeric(km)) %>% 
  filter(!is.na(km))

# Read crashes data
Ano <- 2006
pessoas <- NULL
while(Ano != 2019){
  Ano <- Ano + 1
  x <- fread(paste0("./Database/PRF/acidentes",Ano,".csv")) %>% 
    mutate(ano = Ano)
  pessoas <- rbind.fill(pessoas,x)
  rm(x)
}
rm(Ano)

pessoas <- mutate(pessoas,
                    km = gsub(",",".",km))

pessoas <- mutate(pessoas,km = as.numeric(km),
                              br = as.numeric(br)) %>% 
  filter(!is.na(km))

pessoas <- pessoas %>% 
  distinct(uf,br,km,municipio,id,pesid,
           data_inversa,dia_semana,horario, .keep_all = T) %>% 
  mutate(masculino = ifelse(sexo=="Masculino",1,
                     ifelse(sexo=="Feminino",0,NA)),
         feminino  = ifelse(sexo=="Feminino",1,
                     ifelse(sexo=="Masculino",0,NA)),
         idade = ifelse(idade>0  & idade<=10,1,
                 ifelse(idade>10 & idade<=20,2,
                 ifelse(idade>20 & idade<=30,3,
                 ifelse(idade>30 & idade<=40,4,
                 ifelse(idade>40 & idade<=50,5,
                 ifelse(idade>50 & idade<=60,6,
                 ifelse(idade>60 & idade<=120,7,NA))))))),
         idade_10 = ifelse(idade==1,1,
                    ifelse(is.na(idade),NA,0)),
         idade_20 = ifelse(idade==2,1,
                    ifelse(is.na(idade),NA,0)),
         idade_30 = ifelse(idade==3,1,
                    ifelse(is.na(idade),NA,0)),
         idade_40 = ifelse(idade==4,1,
                           ifelse(is.na(idade),NA,0)),
         idade_50 = ifelse(idade==5,1,
                           ifelse(is.na(idade),NA,0)),
         idade_60 = ifelse(idade==6,1,
                           ifelse(is.na(idade),NA,0)),
         idade_120 = ifelse(idade==7,1,
                           ifelse(is.na(idade),NA,0)),
         automovel= ifelse(tipo_veiculo=="Automóvel",1,
                           ifelse(tipo_veiculo=="(null)",NA,0)),
         caminhao = ifelse(tipo_veiculo=="Camioneta"  |
                           tipo_veiculo=="Caminhão"   |
                           tipo_veiculo=="Caminhonete"|
                           tipo_veiculo=="Caminhão-Tanque"|
                           tipo_veiculo=="Caminhão-Trator"|
                           tipo_veiculo=="Caminhão-trator",1,
                           ifelse(tipo_veiculo=="(null)",NA,0)),
         motocicleta = ifelse(tipo_veiculo=="Motocicletas" |
                              tipo_veiculo=="Motocicleta",1,
                              ifelse(tipo_veiculo=="(null)",NA,0)),
         onibus = ifelse(tipo_veiculo=="Ônibus" |
                         tipo_veiculo=="Micro-ônibus"| 
                         tipo_veiculo=="Microônibus",1,
                              ifelse(tipo_veiculo=="(null)",NA,0)),
         outros = ifelse(onibus == 0 & motocicleta == 0 &
                         caminhao == 0 & automovel==0,1,
                         ifelse(tipo_veiculo=="(null)",NA,0))) %>% 
  group_by(uf,br,km,municipio,id,
           data_inversa,dia_semana,horario) %>%
  dplyr::summarise(
         masculino       = sum(masculino, na.rm = T),
         feminino        = sum(feminino, na.rm = T),
         automovel       = sum(automovel, na.rm = T),
         caminhao        = sum(caminhao, na.rm = T),
         motocicleta     = sum(motocicleta, na.rm = T),
         onibus          = sum(onibus, na.rm =T),
         outros          = sum(outros, na.rm =T),
         idade_10            = sum(idade_10, na.rm = T),
         idade_20            = sum(idade_20, na.rm = T),
         idade_30            = sum(idade_30, na.rm = T),
         idade_40            = sum(idade_40, na.rm = T),
         idade_50            = sum(idade_50, na.rm = T),
         idade_60            = sum(idade_60, na.rm = T),
         idade_120           = sum(idade_120,na.rm = T))
  

acidentes <- ocorrencia %>% left_join(pessoas,
                                      by = c("uf","br","km",
                                             "municipio","id",
                                             "data_inversa",
                                             "dia_semana",
                                             "horario"))
rm(ocorrencia,pessoas)

acidentes <- mutate(acidentes,
                    ano_ocorrencia = ifelse(ano<=2011, substr(data_inversa,7,11),
                                     ifelse(ano>=2012 & ano<=2015,substr(data_inversa,1,4),
                                     ifelse(ano >= 2017,substr(data_inversa,1,4),
                                     ifelse(ano == 2016,ano,NA)))))

acidentes <- mutate(acidentes,
                    mes_ocorrencia = ifelse(ano<=2011, substr(data_inversa,4,5),
                                     ifelse(ano>=2012 & ano<=2015, substr(data_inversa,6,7),
                                     ifelse(ano >= 2017,substr(data_inversa,6,7),
                                     ifelse(ano == 2016,substr(data_inversa,4,5),NA)))))

acidentes <- mutate(acidentes,
                    dia_ocorrencia = ifelse(ano<=2011, substr(data_inversa,1,2),
                                     ifelse(ano>=2012 & ano<=2015, substr(data_inversa,9,10),
                                     ifelse(ano >= 2017,substr(data_inversa,9,10),
                                     ifelse(ano == 2016,substr(data_inversa,1,2),NA)))))


acidentes <- mutate(acidentes,
                    data_inversa = paste0(ano_ocorrencia,"-",mes_ocorrencia,"-",dia_ocorrencia),
                    data_inversa = as.Date(data_inversa))

acidentes <- mutate(acidentes,
                    pessoas        = as.numeric((as.character(pessoas))),
                    mortos         = as.numeric((as.character(mortos))),
                    feridos_leves  = as.numeric((as.character(feridos_leves))),
                    feridos_graves = as.numeric((as.character(feridos_graves))),
                    ilesos         = as.numeric((as.character(ilesos))),
                    feridos        = as.numeric((as.character(feridos))),
                    veiculos       = as.numeric((as.character(veiculos))),
                    ano_ocorrencia = as.numeric((as.character(ano_ocorrencia))),
                    mes_ocorrencia = as.numeric((as.character(mes_ocorrencia))),
                    dia_ocorrencia = as.numeric((as.character(dia_ocorrencia))))


## Adicionar dummy se trecho e metropolitana

geo_metro <- geobr::read_metro_area() %>% 
  mutate(maiores_metro = ifelse(
         name_metro == "RM São Paulo"          |
         name_metro == "RM Rio de Janeiro"     |
         name_metro == "RM Belo Horizonte"     |
         name_metro == "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno"|
         name_metro == "RM Porto Alegre"       |
         name_metro == "RM Recife"             |
         name_metro == "RM Fortaleza"          |
         name_metro == "RM Salvador"           |
         name_metro == "RM Curitiba"           |
         name_metro == "RM Campinas",1,NA),
         metro = 1) # dummy para maiores metropolitanas e outra para metropolitana

geo_metro <- geo_metro %>% as.data.frame() %>% 
  select(c(name_muni,maiores_metro,metro,abbrev_state)) %>% 
  mutate(name_muni = as.character(name_muni),
         abbrev_state = as.character(abbrev_state)) %>% 
mutate_if(is.character, ~iconv(.,from="UTF-8",to="ASCII//TRANSLIT")) %>% 
mutate_if(is.character, toupper)

acidentes <- acidentes %>% 
  left_join(geo_metro, by = c("municipio"="name_muni","uf"="abbrev_state"))

rm(geo_metro)
