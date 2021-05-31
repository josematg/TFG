library(tidyverse)
library(xlsx)
options(scipen = 999)

# Importamos registro accidentes DGT
library(haven)
muestradef <- read_dta("Datos brutos/muestradef.dta")

# Buscamos colisiones limpias, que serian colisiones no simples
# con numero de infractores == 1

inocentes <- muestradef %>%
  filter(simples == 0,               #elimina colisiones simples
         numinfractores == 1,        #selecciona colisiones limpias
         PRES_INFRAC_COND == 1,      #selecciona gente sin infracciones ni errores
         PRES_INFRAC_VEL_COND == 1,
         PRESUNTOS_ERRORES == 1,
         sexo != 999,                #elimina sexo no conocido
         !is.na(edad))               #elimina edad no conocida


# Turno del calculo de las categorias de edad
    
groupage <- function(val){
  catage <- c()
  for(age in val){
    if((age >= 18) & (age <= 20)){
      catage <- append(catage, values = 1)
    } else if((age >= 21) & (age <= 24)){
      catage <- append(catage, values = 2)
    } else if((age >= 25) & (age <= 29)){
      catage <- append(catage, values = 3)
    } else if((age >= 30) & (age <= 34)){
      catage <- append(catage, values = 4)
    } else if((age >= 35) & (age <= 39)){
      catage <- append(catage, values = 5)
    } else if((age >= 40) & (age <= 44)){
      catage <- append(catage, values = 6)
    } else if((age >= 45) & (age <= 49)){
      catage <- append(catage, values = 7)
    } else if((age >= 50) & (age <= 54)){
      catage <- append(catage, values = 8)
    } else if((age >= 55) & (age <= 59)){
      catage <- append(catage, values = 9)
    } else if((age >= 60) & (age <= 64)){
      catage <- append(catage, values = 10)
    } else if((age >= 65) & (age <= 69)){
      catage <- append(catage, values = 11)
    } else if((age >= 70) & (age <= 74)){
      catage <- append(catage, values = 12)
    } else if(age >= 75){
      catage <- append(catage, values = 13)
    }
  }
  return(catage)
}

inocentes <- inocentes %>% 
  mutate(cat_edad = groupage(inocentes$edad))

saveRDS(inocentes, file="inocentes.rds")
# Por tanto, tenemos ahora mismo el año y las categorias de sexo y edad
# Voy a crear un tibble para calcular las odds ratios

extraer3 <- function(anno){
  x <- 1:13 #para ciclar y buscar las categorias de edad
  y <- vector() #creo 2 vectores vacios
  z <- vector()
  for(i in x){
    y <- c(y, inocentes %>% 
             filter(ano == anno, sexo == 2, cat_edad == i) %>% 
             nrow) ## Selecciona mujeres conductoras inocentes
    z <- c(z, inocentes %>% 
             filter(ano == anno, sexo == 1, cat_edad == i) %>% 
             nrow) ## Selecciona hombres conductores inocentes
  }
  abc <- tibble(x, y, z)
  colnames(abc) <- c("cat_edad", "muj_inoc", "hom_inoc")
  return(abc)
}

#2014

or_muj_inocentes_2014 <- extraer3(2014)

or_muj_inocentes_2014 <- or_muj_inocentes_2014 %>% 
  add_row(cat_edad = 0, muj_inoc = sum(or_muj_inocentes_2014$muj_inoc),
          hom_inoc = sum(or_muj_inocentes_2014$hom_inoc)) %>% 
  arrange(cat_edad) %>% 
  mutate(all_inoc = muj_inoc + hom_inoc) %>% 
  mutate(prop_muj_inoc = muj_inoc/all_inoc) %>% 
  mutate(odds_muj_inoc = prop_muj_inoc / (1 - prop_muj_inoc))

#2015
or_muj_inocentes_2015 <- extraer3(2015)

or_muj_inocentes_2015 <- or_muj_inocentes_2015 %>% 
  add_row(cat_edad = 0, muj_inoc = sum(or_muj_inocentes_2015$muj_inoc),
          hom_inoc = sum(or_muj_inocentes_2015$hom_inoc)) %>% 
  arrange(cat_edad) %>% 
  mutate(all_inoc = muj_inoc + hom_inoc) %>% 
  mutate(prop_muj_inoc = muj_inoc/all_inoc) %>% 
  mutate(odds_muj_inoc = prop_muj_inoc / (1 - prop_muj_inoc))

#2016
or_muj_inocentes_2016 <- extraer3(2016)

or_muj_inocentes_2016 <- or_muj_inocentes_2016 %>% 
  add_row(cat_edad = 0, muj_inoc = sum(or_muj_inocentes_2016$muj_inoc),
          hom_inoc = sum(or_muj_inocentes_2016$hom_inoc)) %>% 
  arrange(cat_edad) %>% 
  mutate(all_inoc = muj_inoc + hom_inoc) %>% 
  mutate(prop_muj_inoc = muj_inoc/all_inoc) %>% 
  mutate(odds_muj_inoc = prop_muj_inoc / (1 - prop_muj_inoc))

#2017
or_muj_inocentes_2017 <- extraer3(2017)

or_muj_inocentes_2017 <- or_muj_inocentes_2017 %>% 
  add_row(cat_edad = 0, muj_inoc = sum(or_muj_inocentes_2017$muj_inoc),
          hom_inoc = sum(or_muj_inocentes_2017$hom_inoc)) %>% 
  arrange(cat_edad) %>% 
  mutate(all_inoc = muj_inoc + hom_inoc) %>% 
  mutate(prop_muj_inoc = muj_inoc/all_inoc) %>% 
  mutate(odds_muj_inoc = prop_muj_inoc / (1 - prop_muj_inoc))

#Total
extraer4 <- function(){
  x <- 1:13 #para ciclar y buscar las categorias de edad
  y <- vector() #creo 2 vectores vacios
  z <- vector()
  for(i in x){
    y <- c(y, inocentes %>% 
             filter(sexo == 2, cat_edad == i) %>% 
             nrow) ## Selecciona mujeres conductoras inocentes
    z <- c(z, inocentes %>% 
             filter(sexo == 1, cat_edad == i) %>% 
             nrow) ## Selecciona hombres conductores inocentes
  }
  abc <- tibble(x, y, z)
  colnames(abc) <- c("cat_edad", "muj_inoc", "hom_inoc")
  return(abc)
}

or_muj_inocentes_total <- extraer4()

or_muj_inocentes_total <- or_muj_inocentes_total %>% 
  add_row(cat_edad = 0, muj_inoc = sum(or_muj_inocentes_total$muj_inoc),
          hom_inoc = sum(or_muj_inocentes_total$hom_inoc)) %>% 
  arrange(cat_edad) %>% 
  mutate(all_inoc = muj_inoc + hom_inoc) %>% 
  mutate(prop_muj_inoc = muj_inoc/all_inoc) %>% 
  mutate(odds_muj_inoc = prop_muj_inoc / (1 - prop_muj_inoc))

# Guardo como rds y como xls
saveRDS(or_muj_inocentes_2014, file="or_muj_inocentes_2014.rds")
saveRDS(or_muj_inocentes_2015, file="or_muj_inocentes_2015.rds")
saveRDS(or_muj_inocentes_2016, file="or_muj_inocentes_2016.rds")
saveRDS(or_muj_inocentes_2017, file="or_muj_inocentes_2017.rds")
saveRDS(or_muj_inocentes_total, file="or_muj_inocentes_total.rds")

write.xlsx(or_muj_inocentes_2014, file = "or_muj_inocentes.xls", 
           sheetName = "2014", append = FALSE)
write.xlsx(or_muj_inocentes_2015, file = "or_muj_inocentes.xls", 
           sheetName = "2015", append = TRUE)
write.xlsx(or_muj_inocentes_2016, file = "or_muj_inocentes.xls", 
           sheetName = "2016", append = TRUE)
write.xlsx(or_muj_inocentes_2017, file = "or_muj_inocentes.xls", 
           sheetName = "2017", append = TRUE)
write.xlsx(or_muj_inocentes_total, file = "or_muj_inocentes.xls", 
           sheetName = "Total", append = TRUE)

# Cargo archivos or_muj_cond
or_muj_cond_2014 <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR mujeres conductoras vs INE/or_muj_cond_2014.rds")
or_muj_cond_2015 <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR mujeres conductoras vs INE/or_muj_cond_2015.rds")
or_muj_cond_2016 <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR mujeres conductoras vs INE/or_muj_cond_2016.rds")
or_muj_cond_2017 <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR mujeres conductoras vs INE/or_muj_cond_2017.rds")
or_muj_cond_total <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR mujeres conductoras vs INE/or_muj_cond_total.rds")

#2014
or2014 <- bind_cols(or_muj_cond_2014, or_muj_inocentes_2014[,-1])
or2014 <- or2014 %>% 
  mutate(or_muj_2 = odds_muj_inoc/odds_muj_INE) %>% 
  mutate(or_muj_3 = odds_muj_inoc/odds_muj_DGT)
or2014 <- or2014[,c(1:10,12:16,11,17,18)]
#A?ado IC
or2014 <- mutate(or2014, 
                 IC_OR2_inf = or_muj_2 * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/muj_inoc + 1/hom_inoc))))
or2014 <- mutate(or2014, 
                 IC_OR2_sup = or_muj_2 * exp(1.96*sqrt(1/muj_INE + 1/(all_INE-muj_INE) + 1/muj_inoc + 1/hom_inoc)))
or2014 <- mutate(or2014, 
                 IC_OR3_inf = or_muj_3 * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc))))
or2014 <- mutate(or2014, 
                 IC_OR3_sup = or_muj_3 * exp(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc)))

#2015
or2015 <- bind_cols(or_muj_cond_2015, or_muj_inocentes_2015[,-1])
or2015 <- or2015 %>% 
  mutate(or_muj_2 = odds_muj_inoc/odds_muj_INE) %>% 
  mutate(or_muj_3 = odds_muj_inoc/odds_muj_DGT)
or2015 <- or2015[,c(1:10,12:16,11,17,18)]
#A?ado IC
or2015 <- mutate(or2015, 
                 IC_OR2_inf = or_muj_2 * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/muj_inoc + 1/hom_inoc))))
or2015 <- mutate(or2015, 
                 IC_OR2_sup = or_muj_2 * exp(1.96*sqrt(1/muj_INE + 1/(all_INE-muj_INE) + 1/muj_inoc + 1/hom_inoc)))
or2015 <- mutate(or2015, 
                 IC_OR3_inf = or_muj_3 * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc))))
or2015 <- mutate(or2015, 
                 IC_OR3_sup = or_muj_3 * exp(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc)))

#2016
or2016 <- bind_cols(or_muj_cond_2016, or_muj_inocentes_2016[,-1])
or2016 <- or2016 %>% 
  mutate(or_muj_2 = odds_muj_inoc/odds_muj_INE) %>% 
  mutate(or_muj_3 = odds_muj_inoc/odds_muj_DGT)
or2016 <- or2016[,c(1:10,12:16,11,17,18)]
#A?ado IC
or2016 <- mutate(or2016, 
                 IC_OR2_inf = or_muj_2 * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/muj_inoc + 1/hom_inoc))))
or2016 <- mutate(or2016, 
                 IC_OR2_sup = or_muj_2 * exp(1.96*sqrt(1/muj_INE + 1/(all_INE-muj_INE) + 1/muj_inoc + 1/hom_inoc)))
or2016 <- mutate(or2016, 
                 IC_OR3_inf = or_muj_3 * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc))))
or2016 <- mutate(or2016, 
                 IC_OR3_sup = or_muj_3 * exp(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc)))

#2017
or2017 <- bind_cols(or_muj_cond_2017, or_muj_inocentes_2017[,-1])
or2017 <- or2017 %>% 
  mutate(or_muj_2 = odds_muj_inoc/odds_muj_INE) %>% 
  mutate(or_muj_3 = odds_muj_inoc/odds_muj_DGT)
or2017 <- or2017[,c(1:10,12:16,11,17,18)]
#A?ado IC
or2017 <- mutate(or2017, 
                 IC_OR2_inf = or_muj_2 * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/muj_inoc + 1/hom_inoc))))
or2017 <- mutate(or2017, 
                 IC_OR2_sup = or_muj_2 * exp(1.96*sqrt(1/muj_INE + 1/(all_INE-muj_INE) + 1/muj_inoc + 1/hom_inoc)))
or2017 <- mutate(or2017, 
                 IC_OR3_inf = or_muj_3 * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc))))
or2017 <- mutate(or2017, 
                 IC_OR3_sup = or_muj_3 * exp(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc)))

#Total
ortotal <- bind_cols(or_muj_cond_total, or_muj_inocentes_total[,-1])
ortotal <- ortotal %>% 
  mutate(or_muj_2 = odds_muj_inoc/odds_muj_INE) %>% 
  mutate(or_muj_3 = odds_muj_inoc/odds_muj_DGT)
ortotal <- ortotal[,c(1:10,12:16,11,17,18)]
#A?ado IC
ortotal <- mutate(ortotal, 
                 IC_OR2_inf = or_muj_2 * exp(-(1.96*sqrt(1/muj_INE + 1/(all_INE - muj_INE) + 1/muj_inoc + 1/hom_inoc))))
ortotal <- mutate(ortotal, 
                 IC_OR2_sup = or_muj_2 * exp(1.96*sqrt(1/muj_INE + 1/(all_INE-muj_INE) + 1/muj_inoc + 1/hom_inoc)))
ortotal <- mutate(ortotal, 
                 IC_OR3_inf = or_muj_3 * exp(-(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc))))
ortotal <- mutate(ortotal, 
                 IC_OR3_sup = or_muj_3 * exp(1.96*sqrt(1/muj_DGT + 1/hom_DGT + 1/muj_inoc + 1/hom_inoc)))

write.xlsx(or2014, file = "or_crudas.xls", 
           sheetName = "2014", append = FALSE)
write.xlsx(or2015, file = "or_crudas.xls", 
           sheetName = "2015", append = TRUE)
write.xlsx(or2016, file = "or_crudas.xls", 
           sheetName = "2016", append = TRUE)
write.xlsx(or2017, file = "or_crudas.xls", 
           sheetName = "2017", append = TRUE)
write.xlsx(ortotal, file = "or_crudas.xls", 
           sheetName = "Total", append = TRUE)
