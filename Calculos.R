# Calculo de la OR de ser mujer conductora frente a serlo en la poblacion general
# Para ello, primero tengo que calcular los totales de mujeres

library(tidyverse)
censoDGT_corregido <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/censoDGT_corregido.rds")
INE_corregido <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/INE_corregido.rds")
# Creo una funcion para extraer valores del INE y DGT segun el ano

extraer <- function(anno){
  x <- 1:13 #para ciclar y buscar las categorias de edad
  y <- vector() #creo cuatro vectores vacios
  z <- vector()
  t <- vector()
  u <- vector()
  for(i in x){
    y <- c(y, INE_corregido %>% 
             filter(ano == anno, cat_sex == 2, cat_edad == i) %>% 
             select(poblacion)) ## Selecciona mujeres INE
    z <- c(z, INE_corregido %>% 
             filter(ano == anno, cat_sex == 3, cat_edad == i) %>% 
             select(poblacion)) ## Selecciona total INE
    t <- c(t, censoDGT_corregido %>% 
        filter(ano == anno, cat_sex == 2, cat_edad == i) %>% 
        select(conductores)) ## Selecciona mujeres censo DGT
    u <- c(u, censoDGT_corregido %>% 
             filter(ano == anno, cat_sex == 1, cat_edad == i) %>% 
             select(conductores)) ## Selecciona hombres censo DGT
  }
  abc <- tibble(x, y, z, t, u)
  colnames(abc) <- c("cat_edad", "muj_INE", "all_INE", "muj_DGT", "hom_DGT")
  return(abc)
}

or2014 <- extraer(2014)

for(i in colnames(or2014)){
  or2014[i] <- unlist(or2014[i])
}

or2014 <- or2014 %>%
  add_row(cat_edad = 0, muj_INE = sum(or2014$muj_INE), all_INE = sum(or2014$all_INE),
          hom_DGT = sum(or2014$hom_DGT), muj_DGT = sum(or2014$muj_DGT)) %>% 
  mutate(all_DGT = muj_DGT + hom_DGT) %>% 
  mutate(prop_muj_DGT = muj_DGT/all_DGT) %>% 
  mutate(odds_muj_DGT = prop_muj_DGT / (1 - prop_muj_DGT)) %>% 
  mutate(prop_muj_INE = muj_INE/all_INE) %>% 
  mutate(odds_muj_INE = prop_muj_INE / (1 - prop_muj_INE)) %>% 
  mutate(or_muj_1 = odds_muj_DGT/odds_muj_INE) %>% 
  arrange(cat_edad)

#Voy a reaordenar el tibble para que siga el orden: muj - tot - prop - odds
#2014
or2014 <- or2014[, c(1,2,3,9,10,4,5,6,7,8,11)]
saveRDS(or2014, file = "or_muj_cond_2014.rds")


#2015
or2015 <- extraer(2015)

for(i in colnames(or2015)){
  or2015[i] <- unlist(or2015[i])
}

or2015 <- or2015 %>%
  add_row(cat_edad = 0, muj_INE = sum(or2015$muj_INE), all_INE = sum(or2015$all_INE),
          hom_DGT = sum(or2015$hom_DGT), muj_DGT = sum(or2015$muj_DGT)) %>% 
  mutate(all_DGT = muj_DGT + hom_DGT) %>% 
  mutate(prop_muj_DGT = muj_DGT/all_DGT) %>% 
  mutate(odds_muj_DGT = prop_muj_DGT / (1 - prop_muj_DGT)) %>% 
  mutate(prop_muj_INE = muj_INE/all_INE) %>% 
  mutate(odds_muj_INE = prop_muj_INE / (1 - prop_muj_INE)) %>% 
  mutate(or_muj_1 = odds_muj_DGT/odds_muj_INE) %>% 
  arrange(cat_edad)

or2015 <- or2015[, c(1,2,3,9,10,4,5,6,7,8,11)]
saveRDS(or2015, file = "or_muj_cond_2015.rds")


#2016
or2016 <- extraer(2016)

for(i in colnames(or2016)){
  or2016[i] <- unlist(or2016[i])
}

or2016 <- or2016 %>%
  add_row(cat_edad = 0, muj_INE = sum(or2016$muj_INE), all_INE = sum(or2016$all_INE),
          hom_DGT = sum(or2016$hom_DGT), muj_DGT = sum(or2016$muj_DGT)) %>% 
  mutate(all_DGT = muj_DGT + hom_DGT) %>% 
  mutate(prop_muj_DGT = muj_DGT/all_DGT) %>% 
  mutate(odds_muj_DGT = prop_muj_DGT / (1 - prop_muj_DGT)) %>% 
  mutate(prop_muj_INE = muj_INE/all_INE) %>% 
  mutate(odds_muj_INE = prop_muj_INE / (1 - prop_muj_INE)) %>% 
  mutate(or_muj_1 = odds_muj_DGT/odds_muj_INE) %>% 
  arrange(cat_edad)

or2016 <- or2016[, c(1,2,3,9,10,4,5,6,7,8,11)]
saveRDS(or2016, file = "or_muj_cond_2016.rds")

#2017

or2017 <- extraer(2017)

for(i in colnames(or2017)){
  or2017[i] <- unlist(or2017[i])
}

or2017 <- or2017 %>%
  add_row(cat_edad = 0, muj_INE = sum(or2017$muj_INE), all_INE = sum(or2017$all_INE),
          hom_DGT = sum(or2017$hom_DGT), muj_DGT = sum(or2017$muj_DGT)) %>% 
  mutate(all_DGT = muj_DGT + hom_DGT) %>% 
  mutate(prop_muj_DGT = muj_DGT/all_DGT) %>% 
  mutate(odds_muj_DGT = prop_muj_DGT / (1 - prop_muj_DGT)) %>% 
  mutate(prop_muj_INE = muj_INE/all_INE) %>% 
  mutate(odds_muj_INE = prop_muj_INE / (1 - prop_muj_INE)) %>% 
  mutate(or_muj_1 = odds_muj_DGT/odds_muj_INE) %>% 
  arrange(cat_edad)

or2017 <- or2017[, c(1,2,3,9,10,4,5,6,7,8,11)]
saveRDS(or2017, file = "or_muj_cond_2017.rds")

#Calculo total

extraer2 <- function(){
  x <- 0:13 #para ciclar y buscar las categorias de edad
  y <- vector() #creo cuatro vectores vacios
  z <- vector()
  t <- vector()
  u <- vector()
  for(i in x){
    y <- c(y, sum(or2014 %>%
                    filter(cat_edad == i) %>%
                    select(muj_INE),
                  or2015 %>%
                    filter(cat_edad == i) %>%  
                    select(muj_INE),
                  or2016 %>% 
                    filter(cat_edad == i) %>% 
                    select(muj_INE),
                  or2017 %>% 
                    filter(cat_edad == i) %>% 
                    select(muj_INE))) ## Selecciona mujeres INE
    z <- c(z, sum(or2014 %>%
                    filter(cat_edad == i) %>%
                    select(all_INE),
                  or2015 %>%
                    filter(cat_edad == i) %>%  
                    select(all_INE),
                  or2016 %>% 
                    filter(cat_edad == i) %>% 
                    select(all_INE),
                  or2017 %>% 
                    filter(cat_edad == i) %>% 
                    select(all_INE))) ## Selecciona todos INE
    t <- c(t, sum(or2014 %>%
                    filter(cat_edad == i) %>%
                   select(muj_DGT),
                  or2015 %>%
                    filter(cat_edad == i) %>%  
                    select(muj_DGT),
                  or2016 %>% 
                    filter(cat_edad == i) %>% 
                    select(muj_DGT),
                  or2017 %>% 
                    filter(cat_edad == i) %>% 
                    select(muj_DGT))) ## Selecciona mujeres DGT
    u <- c(u, sum(or2014 %>%
                    filter(cat_edad == i) %>%
                    select(hom_DGT),
                  or2015 %>%
                    filter(cat_edad == i) %>%  
                    select(hom_DGT),
                  or2016 %>% 
                    filter(cat_edad == i) %>% 
                    select(hom_DGT),
                  or2017 %>% 
                    filter(cat_edad == i) %>% 
                    select(hom_DGT))) ## Selecciona hombres DGT
  }
  abc <- tibble(x, y, z, t, u)
  colnames(abc) <- c("cat_edad", "muj_INE", "all_INE", "muj_DGT", "hom_DGT")
  return(abc)
}

ortotal <- extraer2()

for(i in colnames(ortotal)){
  ortotal[i] <- unlist(ortotal[i])
}

ortotal <- ortotal %>%
  mutate(all_DGT = muj_DGT + hom_DGT) %>% 
  mutate(prop_muj_DGT = muj_DGT/all_DGT) %>% 
  mutate(odds_muj_DGT = prop_muj_DGT / (1 - prop_muj_DGT)) %>% 
  mutate(prop_muj_INE = muj_INE/all_INE) %>% 
  mutate(odds_muj_INE = prop_muj_INE / (1 - prop_muj_INE)) %>% 
  mutate(or_muj_1 = odds_muj_DGT/odds_muj_INE) %>% 
  arrange(cat_edad)

ortotal <- ortotal[, c(1,2,3,9,10,4,5,6,7,8,11)]

saveRDS(ortotal, file = "or_muj_cond_total.rds")

#Hago un archivo .xls todo junto
library(xlsx)
write.xlsx(or2014, file = "or_muj_cond.xls", sheetName = "2014",
           append = FALSE)
write.xlsx(or2015, file = "or_muj_cond.xls", sheetName = "2015",
           append = TRUE)
write.xlsx(or2016, file = "or_muj_cond.xls", sheetName = "2016",
           append = TRUE)
write.xlsx(or2017, file = "or_muj_cond.xls", sheetName = "2017",
           append = TRUE)
write.xlsx(ortotal, file = "or_muj_cond.xls", sheetName = "Total",
           append = TRUE)

