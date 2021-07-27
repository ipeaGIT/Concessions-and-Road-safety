attach(base, warn.conflicts = F)

temporary <- base

dep_var <- list("acidente_km", "morte_acidente", "feridos_acidente")

controls <- cbind(log(v_rain_extension_mensal+1),ln_emp_extension,ln_pib_extension,
                  log(v_temp_extension_mensal),ln_pop_extension)

###########################
# Table 2 -  Main results #
###########################

for(i in dep_var){
  
  assign(paste0(i,"_results1"),felm(get(paste0(i)) ~ treat + id_br_uf + mes_acidente + ano_acidente                      |0|0|id_br_uf))
  assign(paste0(i,"_results2"),felm(get(paste0(i)) ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente            |0|0|id_br_uf))
  assign(paste0(i,"_results3"),felm(get(paste0(i)) ~ treat + id_br_uf + mes_acidente + ano_acidente + controls           |0|0|id_br_uf))
  assign(paste0(i,"_results4"),felm(get(paste0(i)) ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls |0|0|id_br_uf))

}

stargazer::stargazer(acidente_km_results1,
                     acidente_km_results3,
                     morte_acidente_results1,
                     morte_acidente_results3,
                     feridos_acidente_results1,
                     feridos_acidente_results3,
                     keep.stat=c("n"),  
                     add.lines = list(c("ID FE","Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
                                      c("Crash month FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                      c("Crash year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                      c("Controls", "", "Yes", "", "Yes","", "Yes")),
                     title="Results", align=TRUE , digits = 3,out="table2.html",
  keep = c("treat","treat_1yr_10yrs"))

stargazer::stargazer(acidente_km_results2,
                     acidente_km_results4,
                     morte_acidente_results2,
                     morte_acidente_results4,
                     feridos_acidente_results2,
                     feridos_acidente_results4,
                     keep.stat=c("n"),  
                     add.lines = list(c("ID FE","Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
                     c("Crash month FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Crash year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Controls", "", "Yes", "", "Yes","", "Yes")),
                     title="Results", align=TRUE , digits = 3,out="table2.html", type = 'text',
                     keep = c("treat"))

# Manter apenas as bases de dados e os resultados

rm(list=setdiff(ls(), c("base","base_dia","base_urbano","controls","FE"))) 

################################
# Table 8 -  Concession Groups #
################################


results1 <- felm(morte_acidente ~ treat + id_br_uf + mes_acidente*ano_acidente + controls  |0|0|id_br_uf)

results2 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente*ano_acidente + controls  |0|0|id_br_uf)

results3 <-  felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente +  as.factor(code_state)*ym_acidente + controls  |0|0|id_br_uf)

results4 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente +  as.factor(code_state)*ym_acidente + controls  |0|0|id_br_uf)

results5 <-  felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + as.numeric(id_br_uf)*ano_acidente + controls  |0|0|id_br_uf)

results6 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + as.numeric(id_br_uf)*ano_acidente + controls  |0|0|id_br_uf)

results7 <-  felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + as.factor(mes_valor):as.factor(ano_acidente) +  controls  |0|0|id_br_uf)

results8 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + as.factor(mes_valor):as.factor(ano_acidente) +   controls  |0|0|id_br_uf)

results9 <-  felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente  + treat_pedag + controls  |0|0|id_br_uf)

results10 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente  + treat_pedag + controls  |0|0|id_br_uf)

results11 <-  felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ln_dist_urb*ano_acidente +  ano_acidente +  code_state*ym_acidente + controls  |0|0|id_br_uf)

results12 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + ln_dist_urb*ano_acidente +  code_state*ym_acidente + controls  |0|0|id_br_uf)

#source('./R/script_todos_estados.R', encoding = 'UTF-8')

results13 <-  felm(morte_acidente ~ treat + id_br_uf + mes_acidente  +  ano_acidente +  controls  |0|0|id_br_uf, data = base_completa)

results14 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente  + controls  |0|0|id_br_uf, data = base_completa)

temp <- subset(temporary, acidente_tot>=33 & acidente_tot<=21508)

controls_temp  <- cbind(log(temp$v_rain_extension_mensal+1),temp$ln_emp_extension,temp$ln_pib_extension,
                        log(temp$v_temp_extension_mensal),temp$ln_pop_extension)

results15 <-  felm(morte_acidente ~ treat + id_br_uf + mes_acidente +  ano_acidente  + controls_temp  |0|0|id_br_uf, data = temp)

results16 <-  felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls_temp  |0|0|id_br_uf, data= temp)


#rm(temp)

stargazer::stargazer(results1,
                     results3,
                     results5,
                     results7,
                     results9,
                     results11,
                     results13,
                     results15,
                     title="Results", align=TRUE , digits = 3,out="table2.html",
                     keep = c("treat","treat_1yr_10yrs"))

stargazer::stargazer(results2,
                     results4,
                     results6,
                     results8,
                     results10,
                     results12,
                     results14,
                     results16,
                     add.lines = list(c("ID FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes","Yes"),
                     c("Crash month FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes","Yes"),
                     c("Crash year FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes","Yes"),
                     c("Controls", "Yes", "Yes", "Yes", "Yes","Yes", "Yes","Yes"),
                     c("Concerning all states"          ,"Yes","","","","","","",""), 
                     c("Distance to nearest large city" ,"","Yes","","","","","",""),
                     c("Including a toll start dummy"   ,"","","Yes","","","","",""),
                     c("Harvest month"                  ,"","","","Yes","","","",""),
                     c("ID specific trend"              ,"","","","","Yes","","",""),
                     c("State linear trend"             ,"","","","","","Yes","",""),
                     c("Concerning all states"          ,"","","","","","","Yes",""),
                     c("Triming the tails of the sample","","","","","","","","Yes")),
                     title="Results", align=TRUE , digits = 3,out="table2.html", type = 'text',
                     keep = c("treat","treat_1yr_10yrs"))

#############################
# Table 9 -  Other outcomes #
#############################

dep_var <- list("pessoas_km" , 'mortos_km', "veiculos_km", "feridos_leves_acidente", "feridos_graves_acidente")

for(i in dep_var){
  
  assign(paste0(i,"_results1"),felm(get(paste0(i)) ~ treat + id_br_uf + mes_acidente + ano_acidente + controls           |0|0|id_br_uf))
  assign(paste0(i,"_results2"),felm(get(paste0(i)) ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls |0|0|id_br_uf))
  
}

stargazer::stargazer(pessoas_km_results1,
                     mortos_km_results1,
                     veiculos_km_results1,
                     feridos_leves_acidente_results1,
                     feridos_graves_acidente_results1,
                     title="Results", align=TRUE , digits = 3,out="table2.html",
                     keep = c("treat","treat_1yr_10yrs"))

stargazer::stargazer(pessoas_km_results2,
                     mortos_km_results2,
                     veiculos_km_results2,
                     feridos_leves_acidente_results2,
                     feridos_graves_acidente_results2,
                     add.lines = list(c("ID FE", "Yes", "Yes", "Yes", "Yes"),
                     c("Crash month FE", "Yes", "Yes", "Yes", "Yes"),
                     c("Crash year FE", "Yes", "Yes", "Yes", "Yes"),
                     c("Controls", "", "Yes", "", "Yes")),
                     title="Results", align=TRUE , digits = 3,out="table2.html", type = 'text',
                     keep = c("treat","treat_1yr_10yrs"))

# Manter apenas as bases de dados e os resultados

rm(list=setdiff(ls(), c("base","controls","FE"))) 

########################################################
# Table 5 -  Test Heterogeneity: Phase concession      #
########################################################

a1 = subset(base,grupo_fase1==1)
controlsa1 <- cbind(log(a1$v_rain_extension_mensal+1),a1$ln_emp_extension,a1$ln_pib_extension,
                  log(a1$v_temp_extension_mensal),a1$ln_pop_extension)
a2 = subset(base,grupo_fase2==1)
controlsa2 <- cbind(log(a2$v_rain_extension_mensal+1),a2$ln_emp_extension,a2$ln_pib_extension,
                    log(a2$v_temp_extension_mensal),a2$ln_pop_extension)
a3 = subset(base_urbano,area_acidente_urb==1)
controlsa3 <- cbind(log(a3$v_rain_extension_mensal+1),a3$ln_emp_extension,a3$ln_pib_extension,
                    log(a3$v_temp_extension_mensal),a3$ln_pop_extension)
a4 = subset(base_urbano,area_acidente_urb==0)
controlsa4 <- cbind(log(a4$v_rain_extension_mensal+1),a4$ln_emp_extension,a4$ln_pib_extension,
                    log(a4$v_temp_extension_mensal),a4$ln_pop_extension)
a5 = subset(base_dia,acidente_dia==1)
controlsa5 <- cbind(log(a5$v_rain_extension_mensal+1),a5$ln_emp_extension,a5$ln_pib_extension,
                    log(a5$v_temp_extension_mensal),a5$ln_pop_extension)
a6 = subset(base_dia,acidente_dia==0)
controlsa6 <- cbind(log(a6$v_rain_extension_mensal+1),a6$ln_emp_extension,a6$ln_pib_extension,
                    log(a6$v_temp_extension_mensal),a6$ln_pop_extension)
a7 = subset(base,diff==1)
controlsa7 <- cbind(log(a7$v_rain_extension_mensal+1),a7$ln_emp_extension,a7$ln_pib_extension,
                    log(a7$v_temp_extension_mensal),a7$ln_pop_extension)
a8 =subset(base,diff==0)
controlsa8 <- cbind(log(a8$v_rain_extension_mensal+1),a8$ln_emp_extension,a8$ln_pib_extension,
                    log(a8$v_temp_extension_mensal),a8$ln_pop_extension)

morte_acidente_results1  <- felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + controlsa1               |0|0|id_br_uf, data = a1)
morte_acidente_results2  <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controlsa1     |0|0|id_br_uf, data = a1)
morte_acidente_results3  <- felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente  + controlsa2              |0|0|id_br_uf, data = a2)
morte_acidente_results4  <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente   + controlsa2   |0|0|id_br_uf, data = a2)    
morte_acidente_results5  <- felm(morte_acidente ~ treat + id_br_uf_urb + mes_acidente + ano_acidente + controlsa3           |0|0|id_br_uf_urb, data = a3)
morte_acidente_results6  <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf_urb + mes_acidente + ano_acidente + controlsa3 |0|0|id_br_uf_urb, data = a3)    
morte_acidente_results7  <- felm(morte_acidente ~ treat + id_br_uf_urb + mes_acidente + ano_acidente  + controlsa4          |0|0|id_br_uf_urb, data = a4)
morte_acidente_results8  <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf_urb + mes_acidente + ano_acidente + controlsa4 |0|0|id_br_uf_urb, data = a4) 
morte_acidente_results9  <- felm(morte_acidente ~ treat + id_br_uf_dia + mes_acidente + ano_acidente + controlsa5           |0|0|id_br_uf_dia, data = a5)
morte_acidente_results10 <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf_dia + mes_acidente + ano_acidente + controlsa5 |0|0|id_br_uf_dia, data = a5)    
morte_acidente_results11 <- felm(morte_acidente ~ treat + id_br_uf_dia + mes_acidente + ano_acidente + controlsa6           |0|0|id_br_uf_dia, data = a6)
morte_acidente_results12 <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf_dia + mes_acidente + ano_acidente + controlsa6 |0|0|id_br_uf_dia, data = a6) 
morte_acidente_results13 <- felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + controlsa7               |0|0|id_br_uf, data = a7)
morte_acidente_results14 <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controlsa7     |0|0|id_br_uf, data = a7)    
morte_acidente_results15 <- felm(morte_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + controlsa8               |0|0|id_br_uf, data = a8)
morte_acidente_results16 <- felm(morte_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controlsa8     |0|0|id_br_uf, data = a8) 


stargazer::stargazer(morte_acidente_results1,
                     morte_acidente_results2,
                     morte_acidente_results3,
                     morte_acidente_results4,
                     morte_acidente_results5,
                     morte_acidente_results6,
                     morte_acidente_results7,
                     morte_acidente_results8,
                     morte_acidente_results9,
                     morte_acidente_results10,
                     morte_acidente_results11,
                     morte_acidente_results12,
                     morte_acidente_results13,
                     morte_acidente_results14,
                     morte_acidente_results15,
                     morte_acidente_results16,
                     add.lines = list(c("ID FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                      c("Crash month FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                      c("Crash year FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                                      c("Controls", "", "Yes", "", "Yes", "", "Yes", "", "Yes", "", "Yes", "", "Yes", "", "Yes", "", "Yes")),
                     title="Results", align=TRUE , digits = 3,out="table4.html",type = 'text',
                     keep = c("treat","treat_1yr_10yrs"))

stargazer::stargazer(morte_acidente_results2,
                     morte_acidente_results4,
                     morte_acidente_results6,
                     morte_acidente_results8,
                     morte_acidente_results10,
                     morte_acidente_results12,
                     morte_acidente_results14,
                     morte_acidente_results16,
                     add.lines = list(c("ID FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
                     c("Crash month FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Crash year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                     c("Controls", "", "Yes", "", "Yes", "", "Yes")),
                     title="Results", align=TRUE , digits = 3,out="table2.html", type = 'text',
                     keep = c("treat","treat_1yr_10yrs"))

# Manter apenas as bases de dados e os resultados

rm(list=setdiff(ls(), c("base","controls","FE"))) 


###

morte_automovel_acidente_results1 <- felm(morte_automovel_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + controls           |0|0|id_br_uf, data = base)
morte_automovel_acidente_results2 <- felm(morte_automovel_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls |0|0|id_br_uf, data = base) 
morte_motocicleta_acidente_results1 <- felm(morte_motocicleta_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + controls               |0|0|id_br_uf, data = base)
morte_motocicleta_acidente_results2 <- felm(morte_motocicleta_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls     |0|0|id_br_uf, data = base)    
morte_caminhao_onibus_acidente_results1 <- felm(morte_caminhao_onibus_acidente ~ treat + id_br_uf + mes_acidente + ano_acidente + controls               |0|0|id_br_uf, data = base)
morte_caminhao_onibus_acidente_results2 <- felm(morte_caminhao_onibus_acidente ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls     |0|0|id_br_uf, data = base) 

acidente_automovel_km_results1 <- felm(acidente_automovel_km ~ treat + id_br_uf + mes_acidente + ano_acidente + controls           |0|0|id_br_uf, data = base)
acidente_automovel_km_results2 <- felm(acidente_automovel_km ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls |0|0|id_br_uf, data = base) 
acidente_motocicleta_km_results1 <- felm(acidente_motocicleta_km ~ treat + id_br_uf + mes_acidente + ano_acidente + controls               |0|0|id_br_uf, data = base)
acidente_motocicleta_km_results2 <- felm(acidente_motocicleta_km ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls     |0|0|id_br_uf, data = base)    
acidente_caminhao_onibus_km_results1 <- felm(acidente_caminhao_onibus_km ~ treat + id_br_uf + mes_acidente + ano_acidente + controls               |0|0|id_br_uf, data = base)
acidente_caminhao_onibus_km_results2 <- felm(acidente_caminhao_onibus_km ~ treat_1yr_10yrs + id_br_uf + mes_acidente + ano_acidente + controls     |0|0|id_br_uf, data = base) 


stargazer::stargazer(morte_automovel_acidente_results1,
                     morte_automovel_acidente_results2,
                     morte_motocicleta_acidente_results1,
                     morte_motocicleta_acidente_results2,
                     morte_caminhao_onibus_acidente_results1,
                     morte_caminhao_onibus_acidente_results2,
                     add.lines = list(c("ID FE", "Yes", "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
                                      c("Crash month FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
                                      c("Crash year FE", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes", "Yes"),
                                      c("Controls", "", "Yes", "", "Yes", "", "Yes", "", "Yes", "", "Yes", "", "Yes")),
                     title="Results", align=TRUE , digits = 3,out="table5.html", type = 'text',
                     keep = c("treat","treat_1yr_10yrs"))
