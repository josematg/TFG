library(tidyverse)

#leer csv INE:
INEpobgen <- read_delim("INEpobgen.csv", 
                        ";", escape_double = FALSE, col_types = cols(Nacionalidad = col_skip(), 
                                                                     Provincias = col_skip(), Total = col_number()), 
                        locale = locale(date_names = "es", decimal_mark = ",", 
                                        grouping_mark = ".", encoding = "ISO-8859-1"), 
                        trim_ws = TRUE)

#cambio nombres de las columnas a algo manejable en UTF-8
INEpobgen_bruto <- INEpobgen %>% 
  rename(
    edad = 'Grupo quinquenal de edad',
    poblacion = 'Total',
    sexo = 'Sexo',
    ano = 'Periodo'
  )

#los datos no están igual que en el archivo de DGT, así que voy a buscar los datos individuales
INEpobesp_bruto <- INEpobesp %>% 
  rename(
    edad = 'Edad',
    poblacion = 'Total',
    sexo = 'Sexo',
    ano = 'Periodo'
  )

#Hombres 18 a 20 años 2018
INE2018 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2018", sexo == "Hombres", edad != "21 años") 
INE2018 <- add_row(INE2018, edad = "De 18 a 20 años", sexo = "Hombres", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion))

#Hombres 18 a 20 años 2017
INE2017 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2017", sexo == "Hombres", edad != "21 años") 
INE2017 <- add_row(INE2017, edad = "De 18 a 20 años", sexo = "Hombres", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion))

#Hombres 18 a 20 años 2016
INE2016 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2016", sexo == "Hombres", edad != "21 años") 
INE2016 <- add_row(INE2016, edad = "De 18 a 20 años", sexo = "Hombres", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion))

#Hombres 18 a 20 años 2015
INE2015 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2015", sexo == "Hombres", edad != "21 años") 
INE2015 <- add_row(INE2015, edad = "De 18 a 20 años", sexo = "Hombres", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion))

#junto todas las tablas anteriores en una y filtro "de 18 a 20 años"
joven <- bind_rows(INE2015,INE2016,INE2017,INE2018) %>% filter(edad == "De 18 a 20 años")

#Mujeres 18 a 20 años 2018
INE2018 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2018", sexo == "Mujeres", edad != "21 años") 
INE2018 <- add_row(INE2018, edad = "De 18 a 20 años", sexo = "Mujeres", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion))

#Mujeres 18 a 20 años 2017
INE2017 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2017", sexo == "Mujeres", edad != "21 años") 
INE2017 <- add_row(INE2017, edad = "De 18 a 20 años", sexo = "Mujeres", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion))

#Mujeres 18 a 20 años 2016
INE2016 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2016", sexo == "Mujeres", edad != "21 años") 
INE2016 <- add_row(INE2016, edad = "De 18 a 20 años", sexo = "Mujeres", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion))

#Mujeres 18 a 20 años 2015
INE2015 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2015", sexo == "Mujeres", edad != "21 años") 
INE2015 <- add_row(INE2015, edad = "De 18 a 20 años", sexo = "Mujeres", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion))

#junto todas las tablas anteriores en una y filtro "de 18 a 20 años"
joven <- bind_rows(joven,INE2015,INE2016,INE2017,INE2018) %>% filter(edad == "De 18 a 20 años")

#Ambos sexos 18 a 20 años 2018
INE2018 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2018", sexo == "Ambos sexos", edad != "21 años") 
INE2018 <- add_row(INE2018, edad = "De 18 a 20 años", sexo = "Ambos sexos", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion))

#Ambos sexos 18 a 20 años 2017
INE2017 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2017", sexo == "Ambos sexos", edad != "21 años") 
INE2017 <- add_row(INE2017, edad = "De 18 a 20 años", sexo = "Ambos sexos", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion))

#Ambos sexos 18 a 20 años 2016
INE2016 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2016", sexo == "Ambos sexos", edad != "21 años") 
INE2016 <- add_row(INE2016, edad = "De 18 a 20 años", sexo = "Ambos sexos", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion))

#Ambos sexos 18 a 20 años 2015
INE2015 <- INEpobesp_bruto %>% filter(ano == "1 de enero de 2015", sexo == "Ambos sexos", edad != "21 años") 
INE2015 <- add_row(INE2015, edad = "De 18 a 20 años", sexo = "Ambos sexos", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion))

#junto todas las tablas anteriores en una y filtro "de 18 a 20 años"
joven <- bind_rows(joven,INE2015,INE2016,INE2017,INE2018) %>% filter(edad == "De 18 a 20 años")

#junto las tablas anteriores con el total del INE
INEpobgen_neto <- bind_rows(INEpobgen_bruto,joven)

#sumo los totales de todas las categorías que son de mayores de 75 años
INE2018 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                           | edad == "De 85 a 90 años" | edad == "90 y más años", 
                           sexo == "Hombres", ano == "1 de enero de 2018")
INE2018 <- add_row(INE2018, edad = "75 y más años", sexo ="Hombres", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion))

INE2017 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Hombres", ano == "1 de enero de 2017")
INE2017 <- add_row(INE2017, edad = "75 y más años", sexo ="Hombres", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion))

INE2016 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Hombres", ano == "1 de enero de 2016")
INE2016 <- add_row(INE2016, edad = "75 y más años", sexo ="Hombres", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion))

INE2015 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Hombres", ano == "1 de enero de 2015")
INE2015 <- add_row(INE2015, edad = "75 y más años", sexo ="Hombres", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion))

#junto todas las tablas anteriores en una y filtro "75 y más años"
anciano <- bind_rows(INE2015,INE2016,INE2017,INE2018) %>% filter(edad == "75 y más años")

#hago lo mismo con las mujeres ancianas
INE2018 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Mujeres", ano == "1 de enero de 2018")
INE2018 <- add_row(INE2018, edad = "75 y más años", sexo ="Mujeres", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion))

INE2017 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Mujeres", ano == "1 de enero de 2017")
INE2017 <- add_row(INE2017, edad = "75 y más años", sexo ="Mujeres", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion))

INE2016 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Mujeres", ano == "1 de enero de 2016")
INE2016 <- add_row(INE2016, edad = "75 y más años", sexo ="Mujeres", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion))

INE2015 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Mujeres", ano == "1 de enero de 2015")
INE2015 <- add_row(INE2015, edad = "75 y más años", sexo ="Mujeres", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion))

#junto todas las tablas anteriores en una y filtro "75 y más años"
anciano <- bind_rows(anciano,INE2015,INE2016,INE2017,INE2018) %>% filter(edad == "75 y más años")

#turno de ambos sexos
INE2018 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Ambos sexos", ano == "1 de enero de 2018")
INE2018 <- add_row(INE2018, edad = "75 y más años", sexo ="Ambos sexos", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion))

INE2017 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Ambos sexos", ano == "1 de enero de 2017")
INE2017 <- add_row(INE2017, edad = "75 y más años", sexo ="Ambos sexos", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion))

INE2016 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Ambos sexos", ano == "1 de enero de 2016")
INE2016 <- add_row(INE2016, edad = "75 y más años", sexo ="Ambos sexos", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion))

INE2015 <- INEpobgen_bruto %>% filter(edad == "De 75 a 79 años" | edad == "De 80 a 84 años" 
                                      | edad == "De 85 a 90 años" | edad == "90 y más años", 
                                      sexo == "Ambos sexos", ano == "1 de enero de 2015")
INE2015 <- add_row(INE2015, edad = "75 y más años", sexo ="Ambos sexos", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion))

#junto todas las tablas anteriores en una y filtro "75 y más años"
anciano <- bind_rows(anciano,INE2015,INE2016,INE2017,INE2018) %>% filter(edad == "75 y más años")

#junto las tablas anteriores con el total del INE
INEpobgen_neto <- bind_rows(INEpobgen_neto,anciano)

INEpobgen_neto <- arrange(INEpobgen_neto, edad)

#Ahora queda restar a la categoría 20 a 24 años la población con 20 años
#Empezamos con los hombres
#2015
INE2015 <- INEpobgen_neto %>% filter(sexo == "Hombres", edad == "De 20 a 24 años", ano == "1 de enero de 2015")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Hombres", edad == "20 años", ano == "1 de enero de 2015")
INE2015 <- bind_rows(INE2015,INE15)
INE2015 <- add_row(INE2015, edad = "De 21 a 24 años", sexo = "Hombres", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion[1]-INE2015$poblacion[2]))
anos20 <- INE2015[3,]
#2016
INE2016 <- INEpobgen_neto %>% filter(sexo == "Hombres", edad == "De 20 a 24 años", ano == "1 de enero de 2016")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Hombres", edad == "20 años", ano == "1 de enero de 2016")
INE2016 <- bind_rows(INE2016,INE15)
INE2016 <- add_row(INE2016, edad = "De 21 a 24 años", sexo = "Hombres", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion[1]-INE2016$poblacion[2]))
anos20 <- bind_rows(anos20,INE2016[3,])
#2017
INE2017 <- INEpobgen_neto %>% filter(sexo == "Hombres", edad == "De 20 a 24 años", ano == "1 de enero de 2017")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Hombres", edad == "20 años", ano == "1 de enero de 2017")
INE2017 <- bind_rows(INE2017,INE15)
INE2017 <- add_row(INE2017, edad = "De 21 a 24 años", sexo = "Hombres", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion[1]-INE2017$poblacion[2]))
anos20 <- bind_rows(anos20,INE2017[3,])
#2018
INE2018 <- INEpobgen_neto %>% filter(sexo == "Hombres", edad == "De 20 a 24 años", ano == "1 de enero de 2018")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Hombres", edad == "20 años", ano == "1 de enero de 2018")
INE2018 <- bind_rows(INE2018,INE15)
INE2018 <- add_row(INE2018, edad = "De 21 a 24 años", sexo = "Hombres", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion[1]-INE2018$poblacion[2]))
anos20 <- bind_rows(anos20,INE2018[3,])
#Turno de las mujeres
#2015
INE2015 <- INEpobgen_neto %>% filter(sexo == "Mujeres", edad == "De 20 a 24 años", ano == "1 de enero de 2015")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Mujeres", edad == "20 años", ano == "1 de enero de 2015")
INE2015 <- bind_rows(INE2015,INE15)
INE2015 <- add_row(INE2015, edad = "De 21 a 24 años", sexo = "Mujeres", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion[1]-INE2015$poblacion[2]))
anos20 <- bind_rows(anos20,INE2015[3,])
#2016
INE2016 <- INEpobgen_neto %>% filter(sexo == "Mujeres", edad == "De 20 a 24 años", ano == "1 de enero de 2016")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Mujeres", edad == "20 años", ano == "1 de enero de 2016")
INE2016 <- bind_rows(INE2016,INE15)
INE2016 <- add_row(INE2016, edad = "De 21 a 24 años", sexo = "Mujeres", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion[1]-INE2016$poblacion[2]))
anos20 <- bind_rows(anos20,INE2016[3,])
#2017
INE2017 <- INEpobgen_neto %>% filter(sexo == "Mujeres", edad == "De 20 a 24 años", ano == "1 de enero de 2017")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Mujeres", edad == "20 años", ano == "1 de enero de 2017")
INE2017 <- bind_rows(INE2017,INE15)
INE2017 <- add_row(INE2017, edad = "De 21 a 24 años", sexo = "Mujeres", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion[1]-INE2017$poblacion[2]))
anos20 <- bind_rows(anos20,INE2017[3,])
#2018
INE2018 <- INEpobgen_neto %>% filter(sexo == "Mujeres", edad == "De 20 a 24 años", ano == "1 de enero de 2018")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Mujeres", edad == "20 años", ano == "1 de enero de 2018")
INE2018 <- bind_rows(INE2018,INE15)
INE2018 <- add_row(INE2018, edad = "De 21 a 24 años", sexo = "Mujeres", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion[1]-INE2018$poblacion[2]))
anos20 <- bind_rows(anos20,INE2018[3,])
#Turno de ambos sexos
#2015
INE2015 <- INEpobgen_neto %>% filter(sexo == "Ambos sexos", edad == "De 20 a 24 años", ano == "1 de enero de 2015")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Ambos sexos", edad == "20 años", ano == "1 de enero de 2015")
INE2015 <- bind_rows(INE2015,INE15)
INE2015 <- add_row(INE2015, edad = "De 21 a 24 años", sexo = "Ambos sexos", ano = "1 de enero de 2015", poblacion = sum(INE2015$poblacion[1]-INE2015$poblacion[2]))
anos20 <- bind_rows(anos20,INE2015[3,])
#2016
INE2016 <- INEpobgen_neto %>% filter(sexo == "Ambos sexos", edad == "De 20 a 24 años", ano == "1 de enero de 2016")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Ambos sexos", edad == "20 años", ano == "1 de enero de 2016")
INE2016 <- bind_rows(INE2016,INE15)
INE2016 <- add_row(INE2016, edad = "De 21 a 24 años", sexo = "Ambos sexos", ano = "1 de enero de 2016", poblacion = sum(INE2016$poblacion[1]-INE2016$poblacion[2]))
anos20 <- bind_rows(anos20,INE2016[3,])
#2017
INE2017 <- INEpobgen_neto %>% filter(sexo == "Ambos sexos", edad == "De 20 a 24 años", ano == "1 de enero de 2017")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Ambos sexos", edad == "20 años", ano == "1 de enero de 2017")
INE2017 <- bind_rows(INE2017,INE15)
INE2017 <- add_row(INE2017, edad = "De 21 a 24 años", sexo = "Ambos sexos", ano = "1 de enero de 2017", poblacion = sum(INE2017$poblacion[1]-INE2017$poblacion[2]))
anos20 <- bind_rows(anos20,INE2017[3,])
#2018
INE2018 <- INEpobgen_neto %>% filter(sexo == "Ambos sexos", edad == "De 20 a 24 años", ano == "1 de enero de 2018")
INE15 <- INEpobesp_bruto %>% filter(sexo == "Ambos sexos", edad == "20 años", ano == "1 de enero de 2018")
INE2018 <- bind_rows(INE2018,INE15)
INE2018 <- add_row(INE2018, edad = "De 21 a 24 años", sexo = "Ambos sexos", ano = "1 de enero de 2018", poblacion = sum(INE2018$poblacion[1]-INE2018$poblacion[2]))
anos20 <- bind_rows(anos20,INE2018[3,])
#Borro los antiguos de la base de datos
### INEpobgen_neto <- INEpobgen_neto[-c(25:36),]

#Añado los nuevos
INEpobgen_neto <- bind_rows(INEpobgen_neto, anos20)

### Toca limpiar un poco los datos para hacerlos más manejables.

# Lo primero: cambiar los años a algo más manejable (y razonable, 
# ya que los datos del INE eran edad el 1 de enero del siguiente año)

INECorregido <- INECorregido %>% 
  separate(col = "ano", sep = "1 de enero de ", into = c("ano", "anno")) %>% 
  unite(ano, anno, col = "ano", sep = "") %>% 
  mutate(ano, ano = as.numeric(ano) - 1)

# Toca ahora utilizar las mismas categorías de sexo que utilizan en el registro de accidentes
# Emplean 1 para hombres, 2 para mujeres. Como no quiero arrastrar NAs, utilizaré 3 para ambos sexos

groupsex <- function(val){
  d <- c()
  for(x in val){
    if(x == "Hombres"){
    d <- append(d, values = 1)
  } else if(x == "Mujeres"){
    d <- append(d, values = 2)
  } else{
    d <- append(d, values = 3)
  }
    }
  return(d)
}

INECorregido <- INECorregido %>%  
  mutate(sexo, catsex = groupsex(INECorregido$sexo))

#Para mejor manejo, añado también las categorías de edad y ordeno de menor a mayor (que ahora mismo está desordenado)
groupage <- function(val){
  d <- c()
  for(x in val){
    if(x == "De 18 a 20 años"){
      d <- append(d, values = 1)
    } else if(x == "De 21 a 24 años"){
      d <- append(d, values = 2)
    } else if(x == "De 25 a 29 años"){
      d <- append(d, values = 3)
    } else if(x == "De 30 a 34 años"){
      d <- append(d, values = 4)
    } else if(x == "De 35 a 39 años"){
      d <- append(d, values = 5)
    } else if(x == "De 40 a 44 años"){
      d <- append(d, values = 6)
    } else if(x == "De 45 a 49 años"){
      d <- append(d, values = 7)
    } else if(x == "De 50 a 54 años"){
      d <- append(d, values = 8)
    } else if(x == "De 55 a 59 años"){
      d <- append(d, values = 9)
    } else if(x == "De 60 a 64 años"){
      d <- append(d, values = 10)
    } else if(x == "De 65 a 69 años"){
      d <- append(d, values = 11)
    } else if(x == "De 70 a 74 años"){
      d <- append(d, values = 12)
    } else{
      d <- append(d, values = 13)
    }
  }
  return(d)
}
categoria_edad <- data.frame(edad = unique(INECorregido$edad), cat_edad = 1:13)
categoria_sexo <- data.frame(sexo = unique(INECorregido$sexo), cat_sexo = 1:3)

INECorregido <- INECorregido %>%  
  mutate(edad, cat_edad = groupage(INECorregido$edad))

INECorregido <- rename(INECorregido, cat_sex = catsex)

INECorregido <- arrange(INECorregido, cat_edad)
#Guardo como rdata

saveRDS(INECorregido, file = "INECorregido.rds")
saveRDS(categoria_edad, file = "categoria_edad.rds")
saveRDS(categoria_sexo, file = "categoria_sexo.rds")
