library(tidyverse)
options(scipen = 999)

inocentes <- readRDS("~/Archivos Medicina/TFG/Raw Data/inocentes.rds")
library(readxl)
library(xlsx)
or_crudas <- read_excel("Resultados/or_crudas.xls", 
                        sheet = "Total")
or_crudas <- or_crudas[,-1]

### Analisis por tipo de zona

# Zona urbana: calle, travesia, autovia urbana (2,3,4) = zonacat = 0
# Zona no urbana: carretera (1) = zonacat = 1


or_zona <- inocentes %>% group_by(cat_edad, sexo) %>% count(zonacat)

hombres <- or_zona %>% filter(sexo == 1)
mujeres <- or_zona %>% filter(sexo == 2)

or_zona <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)
  
or_zona <- add_row(or_zona, cat_edad = 0, zonacat = 0, 
                  mujeres = sum(filter(or_zona, zonacat == 0) %>% select(mujeres)),
                  hombres = sum(filter(or_zona, zonacat == 0) %>% select(hombres)))
or_zona <- add_row(or_zona, cat_edad = 0, zonacat = 1, 
                  mujeres = sum(filter(or_zona, zonacat == 1) %>% select(mujeres)),
                  hombres = sum(filter(or_zona, zonacat == 1) %>% select(hombres)))

or_zona <- add_column(or_zona, odds = or_zona$mujeres/or_zona$hombres) %>% 
  arrange(cat_edad)

expuestos <- or_zona %>% filter(zonacat == 1)
noexp <- or_zona %>% filter(zonacat == 0)

expuestos <- add_column(expuestos, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)
noexp <- add_column(noexp, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)
  
or_zona <- bind_rows(expuestos, noexp) %>% 
  arrange(cat_edad, zonacat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 2)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 2))

#Creo los IC al 95%
or_zona <- add_column(or_zona, muj_INE = rep(or_crudas$muj_INE, each = 2)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 2))

or_zona <- mutate(or_zona, 
       IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_zona <- add_column(or_zona, muj_DGT = rep(or_crudas$muj_DGT, each = 2)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 2))

or_zona <- mutate(or_zona, 
                  IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_zona, file = "OR ajustadas por zona.rds")
write.xlsx(or_zona, file = "OR ajustadas por zona.xls")

### ANÁLISIS POR TIPO DE FIRME

# Firme seco y limpio → firme = 0
# Firme en cualquier otro estado → firme = 1

or_firme <- inocentes %>% 
  mutate(firmecat = ifelse(CONDICION_FIRME == 1, 0, ifelse(CONDICION_FIRME != 1, 1, NA))) %>% 
  group_by(cat_edad, sexo) %>% count(firmecat)
hombres <- or_firme %>% filter(sexo == 1)
mujeres <- or_firme %>% filter(sexo == 2)
or_firme <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)

or_firme <- add_row(or_firme, cat_edad = 0, firmecat = 0, 
                   mujeres = sum(filter(or_firme, firmecat == 0) %>% select(mujeres)),
                   hombres = sum(filter(or_firme, firmecat == 0) %>% select(hombres)))
or_firme <- add_row(or_firme, cat_edad = 0, firmecat = 1, 
                   mujeres = sum(filter(or_firme, firmecat == 1) %>% select(mujeres)),
                   hombres = sum(filter(or_firme, firmecat == 1) %>% select(hombres)))

or_firme <- add_column(or_firme, odds = or_firme$mujeres/or_firme$hombres) %>% 
  arrange(cat_edad)

expuestos <- or_firme %>% filter(firmecat == 1)
noexp <- or_firme %>% filter(firmecat == 0)

expuestos <- add_column(expuestos, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)
noexp <- add_column(noexp, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)

or_firme <- bind_rows(expuestos, noexp) %>% 
  arrange(cat_edad, firmecat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 2)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 2))

#Creo los IC al 95%
or_firme <- add_column(or_firme, muj_INE = rep(or_crudas$muj_INE, each = 2)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 2))

or_firme <- mutate(or_firme, 
                  IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_firme <- add_column(or_firme, muj_DGT = rep(or_crudas$muj_DGT, each = 2)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 2))

or_firme <- mutate(or_firme, 
                  IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_firme, file = "OR ajustadas por firme.rds")
write.xlsx(or_firme, file = "OR ajustadas por firme.xls")

## ANÁLISIS POR DÍA DE LA SEMANA
# L a V → diacat = 0
# S o D → diacat = 1

# Obtengo primero una columna, diacat, a la cual luego volveré una variable categórica

or_dia <- inocentes %>% 
  mutate(diacat = weekdays(inocentes$FECHA_ACCIDENTE)) %>% 
  mutate(diacat = ifelse(diacat == "sábado" | diacat == "domingo", 1, ifelse(diacat != "sábado" & diacat != "domingo", 0, NA))) %>% 
  group_by(cat_edad, sexo) %>% count(diacat)

hombres <- or_dia %>% filter(sexo == 1)
mujeres <- or_dia %>% filter(sexo == 2)
or_dia <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)

or_dia <- add_row(or_dia, cat_edad = 0, diacat = 0, 
                    mujeres = sum(filter(or_dia, diacat == 0) %>% select(mujeres)),
                    hombres = sum(filter(or_dia, diacat == 0) %>% select(hombres)))
or_dia <- add_row(or_dia, cat_edad = 0, diacat = 1, 
                    mujeres = sum(filter(or_dia, diacat == 1) %>% select(mujeres)),
                    hombres = sum(filter(or_dia, diacat == 1) %>% select(hombres)))

or_dia <- add_column(or_dia, odds = or_dia$mujeres/or_dia$hombres) %>% 
  arrange(cat_edad)

expuestos <- or_dia %>% filter(diacat == 1)
noexp <- or_dia %>% filter(diacat == 0)

expuestos <- add_column(expuestos, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)
noexp <- add_column(noexp, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)

or_dia <- bind_rows(expuestos, noexp) %>% 
  arrange(cat_edad, diacat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 2)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 2))

# Creo los IC al 95%
or_dia <- add_column(or_dia, muj_INE = rep(or_crudas$muj_INE, each = 2)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 2))

or_dia <- mutate(or_dia, 
                   IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_dia <- add_column(or_dia, muj_DGT = rep(or_crudas$muj_DGT, each = 2)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 2))

or_dia <- mutate(or_dia, 
                   IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_dia, file = "OR ajustadas por dia.rds")
write.xlsx(or_dia, file = "OR ajustadas por dia.xls")

## ESTADO METEREOLÓGICO
# 0 = buena (1)
# 1 = adversa (2-7)

or_meteo <- inocentes %>% 
  mutate(meteocat = ifelse(meteo == 1, 0, ifelse(meteo != 1, 1, NA))) %>% 
  group_by(cat_edad, sexo) %>% 
  count(meteocat)

hombres <- or_meteo %>% filter(sexo == 1)
mujeres <- or_meteo %>% filter(sexo == 2)
or_meteo <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)

or_meteo <- add_row(or_meteo, cat_edad = 0, meteocat = 0, 
                  mujeres = sum(filter(or_meteo, meteocat == 0) %>% select(mujeres)),
                  hombres = sum(filter(or_meteo, meteocat == 0) %>% select(hombres)))
or_meteo <- add_row(or_meteo, cat_edad = 0, meteocat = 1, 
                  mujeres = sum(filter(or_meteo, meteocat == 1) %>% select(mujeres)),
                  hombres = sum(filter(or_meteo, meteocat == 1) %>% select(hombres)))

or_meteo <- add_column(or_meteo, odds = or_meteo$mujeres/or_meteo$hombres) %>% 
  arrange(cat_edad)

expuestos <- or_meteo %>% filter(meteocat == 1)
noexp <- or_meteo %>% filter(meteocat == 0)

expuestos <- add_column(expuestos, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)
noexp <- add_column(noexp, odds_INE = or_crudas$odds_muj_INE, odds_DGT = or_crudas$odds_muj_DGT)

or_meteo <- bind_rows(expuestos, noexp) %>% 
  arrange(cat_edad, meteocat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 2)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 2))

# Creo los IC al 95%
or_meteo <- add_column(or_meteo, muj_INE = rep(or_crudas$muj_INE, each = 2)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 2))

or_meteo <- mutate(or_meteo, 
                 IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_meteo <- add_column(or_meteo, muj_DGT = rep(or_crudas$muj_DGT, each = 2)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 2))

or_meteo <- mutate(or_meteo, 
                 IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_meteo, file = "OR ajustadas por meteo.rds")
write.xlsx(or_meteo, file = "OR ajustadas por meteo.xls")

## TIPO DE VÍA - viacat
# 0 = autopista o autovía (1-3)
# 1 = carretera convencional (4-6)
# 2 = calle (9)
# 3 = otros (7, 8, 10-14)

or_via <- inocentes %>% 
  mutate(viacat = ifelse(tipovia %in% c(1,2,3), 0,
                         ifelse(tipovia %in% c(4,5,6), 1,
                                ifelse(tipovia == 9, 2, 3)))) %>% 
  group_by(cat_edad, sexo) %>% count(viacat)

hombres <- or_via %>% filter(sexo == 1)
mujeres <- or_via %>% filter(sexo == 2)
or_via <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)

or_via <- add_row(or_via, cat_edad = 0, viacat = 0, 
                    mujeres = sum(filter(or_via, viacat == 0) %>% select(mujeres)),
                    hombres = sum(filter(or_via, viacat == 0) %>% select(hombres)))
or_via <- add_row(or_via, cat_edad = 0, viacat = 1, 
                    mujeres = sum(filter(or_via, viacat == 1) %>% select(mujeres)),
                    hombres = sum(filter(or_via, viacat == 1) %>% select(hombres)))
or_via <- add_row(or_via, cat_edad = 0, viacat = 2, 
                  mujeres = sum(filter(or_via, viacat == 2) %>% select(mujeres)),
                  hombres = sum(filter(or_via, viacat == 2) %>% select(hombres)))
or_via <- add_row(or_via, cat_edad = 0, viacat = 3, 
                  mujeres = sum(filter(or_via, viacat == 3) %>% select(mujeres)),
                  hombres = sum(filter(or_via, viacat == 3) %>% select(hombres)))

or_via <- add_column(or_via, odds = or_via$mujeres/or_via$hombres) %>% 
  arrange(cat_edad)

or_via <- or_via %>%
  group_by(cat_edad) %>% 
  mutate(odds_INE = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_INE[1],
    cat_edad == 1 ~ or_crudas$odds_muj_INE[2],
    cat_edad == 2 ~ or_crudas$odds_muj_INE[3],
    cat_edad == 3 ~ or_crudas$odds_muj_INE[4],
    cat_edad == 4 ~ or_crudas$odds_muj_INE[5],
    cat_edad == 5 ~ or_crudas$odds_muj_INE[6],
    cat_edad == 6 ~ or_crudas$odds_muj_INE[7],
    cat_edad == 7 ~ or_crudas$odds_muj_INE[8],
    cat_edad == 8 ~ or_crudas$odds_muj_INE[9],
    cat_edad == 9 ~ or_crudas$odds_muj_INE[10],
    cat_edad == 10 ~ or_crudas$odds_muj_INE[11],
    cat_edad == 11 ~ or_crudas$odds_muj_INE[12],
    cat_edad == 12 ~ or_crudas$odds_muj_INE[13],
    cat_edad == 13 ~ or_crudas$odds_muj_INE[14],
  ))

or_via <- or_via %>%
  group_by(cat_edad) %>% 
  mutate(odds_DGT = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_DGT[1],
    cat_edad == 1 ~ or_crudas$odds_muj_DGT[2],
    cat_edad == 2 ~ or_crudas$odds_muj_DGT[3],
    cat_edad == 3 ~ or_crudas$odds_muj_DGT[4],
    cat_edad == 4 ~ or_crudas$odds_muj_DGT[5],
    cat_edad == 5 ~ or_crudas$odds_muj_DGT[6],
    cat_edad == 6 ~ or_crudas$odds_muj_DGT[7],
    cat_edad == 7 ~ or_crudas$odds_muj_DGT[8],
    cat_edad == 8 ~ or_crudas$odds_muj_DGT[9],
    cat_edad == 9 ~ or_crudas$odds_muj_DGT[10],
    cat_edad == 10 ~ or_crudas$odds_muj_DGT[11],
    cat_edad == 11 ~ or_crudas$odds_muj_DGT[12],
    cat_edad == 12 ~ or_crudas$odds_muj_DGT[13],
    cat_edad == 13 ~ or_crudas$odds_muj_DGT[14],
  ))

or_via <- or_via %>% 
  arrange(cat_edad, viacat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 4)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 4))

# Creo los IC al 95%
or_via <- add_column(or_via, muj_INE = rep(or_crudas$muj_INE, each = 4)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 4))

or_via <- mutate(or_via, 
                   IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_via <- add_column(or_via, muj_DGT = rep(or_crudas$muj_DGT, each = 4)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 4))

or_via <- mutate(or_via, 
                   IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_via, file = "OR ajustadas por via.rds")
write.xlsx(as.data.frame(or_via), file = "OR ajustadas por via.xls")

## HORA DEL DÍA - horacat
# 0 = [0-6)
# 1 = [6-12)
# 2 = [12-18)
# 3 = [18-0)

or_hora <- inocentes %>% 
  mutate(horacat = ifelse(hora %in% c(0:5), 0,
                         ifelse(hora %in% c(6:11), 1,
                                ifelse(hora %in% c(12:17), 2, 3)))) %>% 
  group_by(cat_edad, sexo) %>% count(horacat)

hombres <- or_hora %>% filter(sexo == 1)
mujeres <- or_hora %>% filter(sexo == 2)
or_hora <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)

or_hora <- add_row(or_hora, cat_edad = 0, horacat = 0, 
                  mujeres = sum(filter(or_hora, horacat == 0) %>% select(mujeres)),
                  hombres = sum(filter(or_hora, horacat == 0) %>% select(hombres)))
or_hora <- add_row(or_hora, cat_edad = 0, horacat = 1, 
                  mujeres = sum(filter(or_hora, horacat == 1) %>% select(mujeres)),
                  hombres = sum(filter(or_hora, horacat == 1) %>% select(hombres)))
or_hora <- add_row(or_hora, cat_edad = 0, horacat = 2, 
                  mujeres = sum(filter(or_hora, horacat == 2) %>% select(mujeres)),
                  hombres = sum(filter(or_hora, horacat == 2) %>% select(hombres)))
or_hora <- add_row(or_hora, cat_edad = 0, horacat = 3, 
                  mujeres = sum(filter(or_hora, horacat == 3) %>% select(mujeres)),
                  hombres = sum(filter(or_hora, horacat == 3) %>% select(hombres)))

or_hora <- add_column(or_hora, odds = or_hora$mujeres/or_hora$hombres) %>% 
  arrange(cat_edad)

or_hora <- or_hora %>%
  group_by(cat_edad) %>% 
  mutate(odds_INE = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_INE[1],
    cat_edad == 1 ~ or_crudas$odds_muj_INE[2],
    cat_edad == 2 ~ or_crudas$odds_muj_INE[3],
    cat_edad == 3 ~ or_crudas$odds_muj_INE[4],
    cat_edad == 4 ~ or_crudas$odds_muj_INE[5],
    cat_edad == 5 ~ or_crudas$odds_muj_INE[6],
    cat_edad == 6 ~ or_crudas$odds_muj_INE[7],
    cat_edad == 7 ~ or_crudas$odds_muj_INE[8],
    cat_edad == 8 ~ or_crudas$odds_muj_INE[9],
    cat_edad == 9 ~ or_crudas$odds_muj_INE[10],
    cat_edad == 10 ~ or_crudas$odds_muj_INE[11],
    cat_edad == 11 ~ or_crudas$odds_muj_INE[12],
    cat_edad == 12 ~ or_crudas$odds_muj_INE[13],
    cat_edad == 13 ~ or_crudas$odds_muj_INE[14],
  ))

or_hora <- or_hora %>%
  group_by(cat_edad) %>% 
  mutate(odds_DGT = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_DGT[1],
    cat_edad == 1 ~ or_crudas$odds_muj_DGT[2],
    cat_edad == 2 ~ or_crudas$odds_muj_DGT[3],
    cat_edad == 3 ~ or_crudas$odds_muj_DGT[4],
    cat_edad == 4 ~ or_crudas$odds_muj_DGT[5],
    cat_edad == 5 ~ or_crudas$odds_muj_DGT[6],
    cat_edad == 6 ~ or_crudas$odds_muj_DGT[7],
    cat_edad == 7 ~ or_crudas$odds_muj_DGT[8],
    cat_edad == 8 ~ or_crudas$odds_muj_DGT[9],
    cat_edad == 9 ~ or_crudas$odds_muj_DGT[10],
    cat_edad == 10 ~ or_crudas$odds_muj_DGT[11],
    cat_edad == 11 ~ or_crudas$odds_muj_DGT[12],
    cat_edad == 12 ~ or_crudas$odds_muj_DGT[13],
    cat_edad == 13 ~ or_crudas$odds_muj_DGT[14],
  ))

or_hora <- or_hora %>% 
  arrange(cat_edad, horacat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 4)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 4))

# Creo los IC al 95%
or_hora <- add_column(or_hora, muj_INE = rep(or_crudas$muj_INE, each = 4)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 4))

or_hora <- mutate(or_hora, 
                 IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_hora <- add_column(or_hora, muj_DGT = rep(or_crudas$muj_DGT, each = 4)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 4))

or_hora <- mutate(or_hora, 
                 IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_hora, file = "OR ajustadas por hora.rds")
write.xlsx(as.data.frame(or_hora), file = "OR ajustadas por hora.xls")

## CONDICIONES ILUMINACIÓN - luzcat
# 0 = 1
# 1 = 2
# 2 = 3
# 3 = 4
# 4 = 5,6

or_luz <- inocentes %>% 
  mutate(luzcat = ifelse(luz == 1, 0,
                          ifelse(luz == 2, 1,
                                 ifelse(luz == 3, 2,
                                        ifelse(luz == 4, 3, 4))))) %>% 
  group_by(cat_edad, sexo) %>% count(luzcat)

hombres <- or_luz %>% filter(sexo == 1)
mujeres <- or_luz %>% filter(sexo == 2)
mujeres <- ungroup(mujeres) %>% add_row(cat_edad=12,sexo=2,luzcat=2,n=0) %>% arrange(cat_edad,luzcat) %>% group_by(cat_edad,sexo)
or_luz <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)

or_luz <- add_row(or_luz, cat_edad = 0, luzcat = 0, 
                   mujeres = sum(filter(or_luz, luzcat == 0) %>% select(mujeres)),
                   hombres = sum(filter(or_luz, luzcat == 0) %>% select(hombres)))
or_luz <- add_row(or_luz, cat_edad = 0, luzcat = 1, 
                   mujeres = sum(filter(or_luz, luzcat == 1) %>% select(mujeres)),
                   hombres = sum(filter(or_luz, luzcat == 1) %>% select(hombres)))
or_luz <- add_row(or_luz, cat_edad = 0, luzcat = 2, 
                   mujeres = sum(filter(or_luz, luzcat == 2) %>% select(mujeres)),
                   hombres = sum(filter(or_luz, luzcat == 2) %>% select(hombres)))
or_luz <- add_row(or_luz, cat_edad = 0, luzcat = 3, 
                   mujeres = sum(filter(or_luz, luzcat == 3) %>% select(mujeres)),
                   hombres = sum(filter(or_luz, luzcat == 3) %>% select(hombres)))
or_luz <- add_row(or_luz, cat_edad = 0, luzcat = 4, 
                  mujeres = sum(filter(or_luz, luzcat == 4) %>% select(mujeres)),
                  hombres = sum(filter(or_luz, luzcat == 4) %>% select(hombres)))

or_luz <- add_column(or_luz, odds = or_luz$mujeres/or_luz$hombres) %>% 
  arrange(cat_edad)

or_luz <- or_luz %>%
  group_by(cat_edad) %>% 
  mutate(odds_INE = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_INE[1],
    cat_edad == 1 ~ or_crudas$odds_muj_INE[2],
    cat_edad == 2 ~ or_crudas$odds_muj_INE[3],
    cat_edad == 3 ~ or_crudas$odds_muj_INE[4],
    cat_edad == 4 ~ or_crudas$odds_muj_INE[5],
    cat_edad == 5 ~ or_crudas$odds_muj_INE[6],
    cat_edad == 6 ~ or_crudas$odds_muj_INE[7],
    cat_edad == 7 ~ or_crudas$odds_muj_INE[8],
    cat_edad == 8 ~ or_crudas$odds_muj_INE[9],
    cat_edad == 9 ~ or_crudas$odds_muj_INE[10],
    cat_edad == 10 ~ or_crudas$odds_muj_INE[11],
    cat_edad == 11 ~ or_crudas$odds_muj_INE[12],
    cat_edad == 12 ~ or_crudas$odds_muj_INE[13],
    cat_edad == 13 ~ or_crudas$odds_muj_INE[14],
  ))

or_luz <- or_luz %>%
  group_by(cat_edad) %>% 
  mutate(odds_DGT = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_DGT[1],
    cat_edad == 1 ~ or_crudas$odds_muj_DGT[2],
    cat_edad == 2 ~ or_crudas$odds_muj_DGT[3],
    cat_edad == 3 ~ or_crudas$odds_muj_DGT[4],
    cat_edad == 4 ~ or_crudas$odds_muj_DGT[5],
    cat_edad == 5 ~ or_crudas$odds_muj_DGT[6],
    cat_edad == 6 ~ or_crudas$odds_muj_DGT[7],
    cat_edad == 7 ~ or_crudas$odds_muj_DGT[8],
    cat_edad == 8 ~ or_crudas$odds_muj_DGT[9],
    cat_edad == 9 ~ or_crudas$odds_muj_DGT[10],
    cat_edad == 10 ~ or_crudas$odds_muj_DGT[11],
    cat_edad == 11 ~ or_crudas$odds_muj_DGT[12],
    cat_edad == 12 ~ or_crudas$odds_muj_DGT[13],
    cat_edad == 13 ~ or_crudas$odds_muj_DGT[14],
  ))

or_luz <- or_luz %>% 
  arrange(cat_edad, luzcat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 5)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 5))

# Creo los IC al 95%
or_luz <- add_column(or_luz, muj_INE = rep(or_crudas$muj_INE, each = 5)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 5))

or_luz <- mutate(or_luz, 
                  IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_luz <- add_column(or_luz, muj_DGT = rep(or_crudas$muj_DGT, each = 5)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 5))

or_luz <- mutate(or_luz, 
                  IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_luz, file = "OR ajustadas por luz.rds")
write.xlsx(as.data.frame(or_luz), file = "OR ajustadas por luz.xls")

## DENSIDAD DE CIRCULACIÓN - densicat
# 0 = 1
# 1 = 2
# 2 = 3
# 3 = 4,5

or_densi <- inocentes %>% 
  filter(CONDICION_NIVEL_CIRCULA %in% c(1:5)) %>% 
  mutate(densicat = ifelse(CONDICION_NIVEL_CIRCULA == 1, 0,
                          ifelse(CONDICION_NIVEL_CIRCULA == 2, 1,
                                 ifelse(CONDICION_NIVEL_CIRCULA == 3, 2,
                                        ifelse(CONDICION_NIVEL_CIRCULA %in% c(4,5), 3, NA))))) %>% 
  group_by(cat_edad, sexo) %>% count(densicat)

hombres <- or_densi %>% filter(sexo == 1)
mujeres <- or_densi %>% filter(sexo == 2)
or_densi <- bind_cols(mujeres[-2], hombres[4]) %>% 
  rename("mujeres" = n, "hombres" = n1)

or_densi <- add_row(or_densi, cat_edad = 0, densicat = 0, 
                   mujeres = sum(filter(or_densi, densicat == 0) %>% select(mujeres)),
                   hombres = sum(filter(or_densi, densicat == 0) %>% select(hombres)))
or_densi <- add_row(or_densi, cat_edad = 0, densicat = 1, 
                   mujeres = sum(filter(or_densi, densicat == 1) %>% select(mujeres)),
                   hombres = sum(filter(or_densi, densicat == 1) %>% select(hombres)))
or_densi <- add_row(or_densi, cat_edad = 0, densicat = 2, 
                   mujeres = sum(filter(or_densi, densicat == 2) %>% select(mujeres)),
                   hombres = sum(filter(or_densi, densicat == 2) %>% select(hombres)))
or_densi <- add_row(or_densi, cat_edad = 0, densicat = 3, 
                   mujeres = sum(filter(or_densi, densicat == 3) %>% select(mujeres)),
                   hombres = sum(filter(or_densi, densicat == 3) %>% select(hombres)))

or_densi <- add_column(or_densi, odds = or_densi$mujeres/or_densi$hombres) %>% 
  arrange(cat_edad)

or_densi <- or_densi %>%
  group_by(cat_edad) %>% 
  mutate(odds_INE = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_INE[1],
    cat_edad == 1 ~ or_crudas$odds_muj_INE[2],
    cat_edad == 2 ~ or_crudas$odds_muj_INE[3],
    cat_edad == 3 ~ or_crudas$odds_muj_INE[4],
    cat_edad == 4 ~ or_crudas$odds_muj_INE[5],
    cat_edad == 5 ~ or_crudas$odds_muj_INE[6],
    cat_edad == 6 ~ or_crudas$odds_muj_INE[7],
    cat_edad == 7 ~ or_crudas$odds_muj_INE[8],
    cat_edad == 8 ~ or_crudas$odds_muj_INE[9],
    cat_edad == 9 ~ or_crudas$odds_muj_INE[10],
    cat_edad == 10 ~ or_crudas$odds_muj_INE[11],
    cat_edad == 11 ~ or_crudas$odds_muj_INE[12],
    cat_edad == 12 ~ or_crudas$odds_muj_INE[13],
    cat_edad == 13 ~ or_crudas$odds_muj_INE[14],
  ))

or_densi <- or_densi %>%
  group_by(cat_edad) %>% 
  mutate(odds_DGT = case_when(
    cat_edad == 0 ~ or_crudas$odds_muj_DGT[1],
    cat_edad == 1 ~ or_crudas$odds_muj_DGT[2],
    cat_edad == 2 ~ or_crudas$odds_muj_DGT[3],
    cat_edad == 3 ~ or_crudas$odds_muj_DGT[4],
    cat_edad == 4 ~ or_crudas$odds_muj_DGT[5],
    cat_edad == 5 ~ or_crudas$odds_muj_DGT[6],
    cat_edad == 6 ~ or_crudas$odds_muj_DGT[7],
    cat_edad == 7 ~ or_crudas$odds_muj_DGT[8],
    cat_edad == 8 ~ or_crudas$odds_muj_DGT[9],
    cat_edad == 9 ~ or_crudas$odds_muj_DGT[10],
    cat_edad == 10 ~ or_crudas$odds_muj_DGT[11],
    cat_edad == 11 ~ or_crudas$odds_muj_DGT[12],
    cat_edad == 12 ~ or_crudas$odds_muj_DGT[13],
    cat_edad == 13 ~ or_crudas$odds_muj_DGT[14],
  ))

or_densi <- or_densi %>% 
  arrange(cat_edad, densicat) %>% 
  mutate(OR_INE = odds/odds_INE, OR_DGT = odds/odds_DGT) %>% 
  add_column(OR_INE_cruda = rep(or_crudas$or_muj_2, each = 4)) %>% 
  add_column(OR_DGT_cruda = rep(or_crudas$or_muj_3, each = 4))

# Creo los IC al 95%
or_densi <- add_column(or_densi, muj_INE = rep(or_crudas$muj_INE, each = 4)) %>% 
  add_column(all_INE = rep(or_crudas$all_INE, each = 4))

or_densi <- mutate(or_densi, 
                  IC_OR_INE_inf = OR_INE * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_INE_sup = OR_INE * exp(+(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/mujeres + 1/hombres))))

or_densi <- add_column(or_densi, muj_DGT = rep(or_crudas$muj_DGT, each = 4)) %>% 
  add_column(hom_DGT = rep(or_crudas$hom_DGT, each = 4))

or_densi <- mutate(or_densi, 
                  IC_OR_DGT_inf = OR_DGT * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres)))) %>% 
  mutate(IC_OR_DGT_sup = OR_DGT * exp(+(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/mujeres + 1/hombres))))

saveRDS(or_densi, file = "OR ajustadas por densi.rds")
write.xlsx(as.data.frame(or_densi), file = "OR ajustadas por densi.xls")

