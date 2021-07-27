dnit <- read_sf(dsn="./Database/Shapefiles/ST_DNIT_Rodovias_SNV2015_03.shp") %>% 
  select(c(id,br,uf,geometry)) %>% 
  mutate(br = as.numeric(br))

a <- base %>% distinct(uf,br,grupo_tratado)

roads <- a %>% left_join(dnit) %>% 
  group_by(uf,br,grupo_tratado) %>% st_as_sf %>% dplyr::summarise()
rm(a)

coords<- st_coordinates(st_centroid(roads$geometry))

kmeans <- function(k=NULL){
  
  roads.knn<-knearneigh(coords,k=as.numeric(paste0(k)))
  roads_nb<-knn2nb(roads.knn)
  KNEIGHBORS_W1<-nb2listw(roads_nb,style="W")
  W2<-as(as_dgRMatrix_listw(KNEIGHBORS_W1),"CsparseMatrix")
  
  
  #k1 <- knn2nb(knearneigh(coords,k=4))
  #all.linked <- max(unlist(nbdists(k1, coords)))
  #nb_roads <- dnearneigh(coords, 0, all.linked)
  #roads.dis<-nbdists(nb_roads,coords)
  #roads.invdis<-lapply(roads.dis, function(x) 1/x)
  #INV_W<-nb2listw(nb_roads, glist=roads.invdis)
  #W3<-as(as_dgRMatrix_listw(INV_W),"CsparseMatrix")
  setDT(base)
  ### Create spatial variables
  
  I=diag(132)
  W4=kronecker(I,W2)
  
  WD <- W4%*%treat
  WD = as.numeric(WD)
  base[, paste0("WD_",k,"K") := WD]
  mean(WD)
  
  treat_yr <- list("treat_1yr","treat_2yr","treat_3yr","treat_4yr","treat_5yr",
                   "treat_6yr","treat_7yr","treat_8yr","treat_9yr","treat_10yr")
  
  for(i in treat_yr){
    
    assign(paste0(i,"_spatial_lag"),as.numeric(W4%*%get(i)))
    assign(paste0(i,"_spatial_lag"),ifelse(is.na(get(paste0(i,"_spatial_lag"))),
                                           0,get(paste0(i,"_spatial_lag"))))
    
  }
  

  base[, paste0("treat_1yr_spatial_lag_",k,"K")   := treat_1yr_spatial_lag]
  base[, paste0("treat_2yr_spatial_lag_",k,"K")   := treat_2yr_spatial_lag]
  base[, paste0("treat_3yr_spatial_lag_",k,"K")   := treat_3yr_spatial_lag]
  base[, paste0("treat_4yr_spatial_lag_",k,"K")   := treat_4yr_spatial_lag]
  base[, paste0("treat_5yr_spatial_lag_",k,"K")   := treat_5yr_spatial_lag]
  base[, paste0("treat_6yr_spatial_lag_",k,"K")   := treat_6yr_spatial_lag]
  base[, paste0("treat_7yr_spatial_lag_",k,"K")   := treat_7yr_spatial_lag]
  base[, paste0("treat_8yr_spatial_lag_",k,"K")   := treat_8yr_spatial_lag]
  base[, paste0("treat_9yr_spatial_lag_",k,"K")   := treat_9yr_spatial_lag]
  base[, paste0("treat_10yr_spatial_lag_",k,"K")  := treat_10yr_spatial_lag]  
  
  ### Variables with spatial lag ###
  I=diag(132)
  W=kronecker(I,W2) 
  
  Wmorte_acidente = W%*%morte_acidente
  Wmorte_acidente = as.numeric(Wmorte_acidente)

    
  Wmorte_pessoas = W%*%morte_pessoas
  Wmorte_pessoas = as.numeric(Wmorte_pessoas)


  Wv_rain_extension_mensal = W%*%log(v_rain_extension_mensal + 1)
  Wv_rain_extension_mensal = as.numeric(Wv_rain_extension_mensal)

  
  Wln_emp_extension = W%*%ln_emp_extension
  Wln_emp_extension = as.numeric(Wln_emp_extension)

  
  Wln_pib_extension = W%*%ln_pib_extension
  Wln_pib_extension = as.numeric(Wln_pib_extension)


  
  Wv_temp_extension_mensal = W%*%log(v_temp_extension_mensal)
  Wv_temp_extension_mensal = as.numeric(Wv_temp_extension_mensal)


  
  Wln_pop_extension = W%*%ln_pop_extension
  Wln_pop_extension = as.numeric(Wln_pop_extension)
  
  
  Wln_dist_urb = W%*%ln_dist_urb
  Wln_dist_urb = as.numeric(Wln_dist_urb)
  
## Add Varaibles
base[, paste0("Wmorte_acidente_",k,"K") := Wmorte_acidente]
base[, paste0("Wmorte_pessoas_",k,"K") := Wmorte_pessoas]
base[, paste0("Wv_rain_extension_mensal_",k,"K") := Wv_rain_extension_mensal]
base[, paste0("Wln_emp_extension_",k,"K") := Wln_emp_extension]
base[, paste0("Wln_pib_extension_",k,"K") := Wln_pib_extension]
base[, paste0("Wv_temp_extension_mensal_",k,"K") := Wv_temp_extension_mensal]
base[, paste0("Wln_pop_extension_",k,"K") := Wln_pop_extension]
base[, paste0("Wln_dist_urb_",k,"K") := Wln_dist_urb]
  
}

options(warn = -1)
kmeans(k=1)
kmeans(k=2)
kmeans(k=3)
kmeans(k=4)
kmeans(k=6)
kmeans(k=8)


rm(list=setdiff(ls(), c("base","controls","FE"))) 

write.dta(base,"base acidentes - espacial.dta")



attach(base, warn.conflicts = F)

dep_var <- list("morte_acidente","morte_pessoas")
Wcontrols<-cbind(Wv_rain_extension_mensal_4K,Wln_emp_extension_4K,Wln_pib_extension_4K,
                 Wv_temp_extension_mensal_4K,
                 Wln_pop_extension_4K)

####################################
# Table 10 -  Spatial main results #
####################################

for(i in dep_var){
  
  assign(paste0(i,"_spatial_results1"),felm(get(paste0(i)) ~ treat + WD_4K + id_br_uf + mes_acidente + ano_acidente + controls            |0|0|id_br_uf))
  assign(paste0(i,"_spatial_results2"),felm(get(paste0(i)) ~ treat + treat_1yr_spatial_lag_4K + treat_2yr_spatial_lag_4K + treat_3yr_spatial_lag_4K +
                                      treat_4yr_spatial_lag_4K + treat_5yr_spatial_lag_4K + treat_6yr_spatial_lag_4K +
                                      treat_7yr_spatial_lag_4K + treat_8yr_spatial_lag_4K + treat_9yr_spatial_lag_4K +
                                      treat_10yr_spatial_lag_4K + id_br_uf + mes_acidente + ano_acidente + controls           |0|0|id_br_uf))
  assign(paste0(i,"_spatial_results3"),felm(get(paste0(i)) ~ treat + WD_4K + id_br_uf + mes_acidente + ano_acidente + controls + Wcontrols   |0|0|id_br_uf))
  assign(paste0(i,"_spatial_results4"),felm(get(paste0(i)) ~ treat + treat_1yr_spatial_lag_4K + treat_2yr_spatial_lag_4K + treat_3yr_spatial_lag_4K +
                                      treat_4yr_spatial_lag_4K + treat_5yr_spatial_lag_4K + treat_6yr_spatial_lag_4K +
                                      treat_7yr_spatial_lag_4K + treat_8yr_spatial_lag_4K + treat_9yr_spatial_lag_4K +
                                      treat_10yr_spatial_lag_4K + id_br_uf + mes_acidente + ano_acidente + controls + Wcontrols |0|0|id_br_uf))
  
}

stargazer::stargazer(morte_acidente_spatial_results1,
                     morte_acidente_spatial_results3,
                     morte_pessoas_spatial_results1,
                     morte_pessoas_spatial_results3,
                     se = list(coef(summary(morte_acidente_spatial_results1, cluster = c("id_br_uf")))[,2],
                               coef(summary(morte_acidente_spatial_results3, cluster = c("id_br_uf")))[,2], 
                               coef(summary(morte_pessoas_spatial_results1, cluster = c("id_br_uf")))[,2],
                               coef(summary(morte_pessoas_spatial_results3, cluster = c("id_br_uf")))[,2]), 
                     add.lines = list(c("ID FE", "Yes", "Yes", "Yes", "Yes"),
                                      c("Crash month FE", "Yes", "Yes", "Yes", "Yes"),
                                      c("Crash year FE", "Yes", "Yes", "Yes", "Yes"),
                                      c("Controls", "Yes", "Yes", "Yes", "Yes"),
                                      c("Spatial Controls", "No", "Yes", "No", "Yes")),
                     title="Results", align=TRUE , digits = 3,out="table3.tex",
                     no.space=TRUE,
                     keep = c("WD","treat","treat_1yr_spatial_lag",
                              "treat_2yr_spatial_lag","treat_3yr_spatial_lag",
                              "treat_4yr_spatial_lag","treat_5yr_spatial_lag",
                              "treat_6yr_spatial_lag","treat_7yr_spatial_lag",
                              "treat_8yr_spatial_lag","treat_9yr_spatial_lag",
                              "treat_10yr_spatial_lag"))

stargazer::stargazer(morte_acidente_spatial_results2,
                     morte_acidente_spatial_results4,
                     morte_pessoas_spatial_results2,
                     morte_pessoas_spatial_results4,
                     add.lines = list(c("ID FE", "Yes", "Yes", "Yes", "Yes"),
                                      c("Crash month FE", "Yes", "Yes", "Yes", "Yes"),
                                      c("Crash year FE", "Yes", "Yes", "Yes", "Yes"),
                                      c("Controls", "Yes", "Yes", "Yes", "Yes"),
                                      c("Spatial Controls", "No", "Yes", "No", "Yes")),
                     omit.stat=c("LL","ser","f"),
                     no.space=TRUE ,
                     keep = c("treat","treat_1yr_spatial_lag",
                              "treat_2yr_spatial_lag","treat_3yr_spatial_lag",
                              "treat_4yr_spatial_lag","treat_5yr_spatial_lag",
                              "treat_6yr_spatial_lag","treat_7yr_spatial_lag",
                              "treat_8yr_spatial_lag","treat_9yr_spatial_lag",
                              "treat_10yr_spatial_lag"))

