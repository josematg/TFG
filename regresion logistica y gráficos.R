library(biostat3)
library(lmtest)
library(tidyverse)
library(xlsx)
library(readxl)

options(scipen = 999)
inocenteslog <- readRDS("~/Archivos Medicina/TFG/Raw Data/inocentes.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")

                     
### VÍA NO URBANA VS VÍA URBANA

#Creo tabla aparte, mas manejable, fuerzo que se consideren como factores las variables

datos <- inocenteslog %>% dplyr::select(sexo, zonacat, cat_edad)
datos$sexo <- factor(datos$sexo)
datos$cat_edad <- factor(datos$cat_edad)
datos$zonacat <- factor(datos$zonacat)

modelo <- glm(zonacat ~ sexo,
              data = datos,
              family = "binomial"(link = "logit"))

summary(modelo)
exp(coefficients(modelo))
exp(cbind(coefficients(modelo), confint(modelo)))

# Para comprobar que el modelo es correcto, hay que calcular esta misma odds ratio "a mano"
# Ésta sería la ratio entre la odds de ser "hombre que cirucula" en "carretera" vs "mujer que circula" "en carretera"
# y la odds de lo mismo pero en zona urbana

a <- as.integer(table(datos$sexo, datos$zonacat))
table(datos$sexo, datos$zonacat)
(a[1]*a[4])/(a[3]*a[2])

# es el mismo resultado que el modelo

# Calculo ahora los modelos con más términos, como la edad

modelo2 <- glm(zonacat ~ cat_edad*sexo,
              data = datos,
              family = "binomial"(link = "logit"))
summary(modelo2)
# Hay que hacer un test para ver que se ajusta mejor... lrtest! (usar paquete lmtest)
lrtest(modelo2, modelo)

##### Usando paquete biostat3 obtengo las OR de ser mujer que conduce en carretera vs vía urbana

log_zonacat <- lincom(modelo2,
       c('sexo2',
         'sexo2+cat_edad2:sexo2',
         'sexo2+cat_edad3:sexo2',
         'sexo2+cat_edad4:sexo2',
         'sexo2+cat_edad5:sexo2',
         'sexo2+cat_edad6:sexo2',
         'sexo2+cat_edad7:sexo2',
         'sexo2+cat_edad8:sexo2',
         'sexo2+cat_edad9:sexo2',
         'sexo2+cat_edad10:sexo2',
         'sexo2+cat_edad11:sexo2',
         'sexo2+cat_edad12:sexo2',
         'sexo2+cat_edad13:sexo2'),
       level = 0.95,
       eform = T)

log_zonacat <- data.frame(matrix(unlist(log_zonacat, use.names = T), nrow=13, byrow=F)) %>% 
  cbind(c(1:13))
colnames(log_zonacat) <- c("Estimate", "IC_inf", "IC_sup", "Chisq", "Pr(>Chisq)", "cat_edad")
log_zonacat <- log_zonacat[c(6,1:5)]

saveRDS(log_zonacat, file = "OR zona log.rds")
write.xlsx(log_zonacat, file = "OR zona log.xls")
#gráfico
logplot <- ggplot(log_zonacat) + 
  geom_hline(aes(yintercept = 1)) +
  geom_line(size = 1, color = "#ff6361", aes(x = cat_edad, y = Estimate)) +
  geom_point(size = 2, color = "#ff6361", aes(x = cat_edad, y = Estimate)) +
  geom_ribbon(color = "#ff6361", fill = "#ff6361", aes(x = cat_edad, 
                  ymin = IC_inf, 
                  ymax = IC_sup), 
              alpha = 0.2,
              show.legend = FALSE) +
  scale_x_continuous(name = "Grupo de edad",
                     breaks = c(1:13), 
                     labels = unique(categoria_edad$edad)) +
  scale_y_log10(name = "OR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
  labs(title = "Odds Ratio (OR) de ser mujer que circula por vía no urbana.",
       subtitle = " Referencia: vía urbana. Intervalo de confianza al 95%",
       caption = "Datos para el periodo 2014-2017")
plot(logplot)  


### ANÁLISIS POR TIPO DE FIRME
#Creo variable firmecat
inocenteslog <- inocenteslog %>% 
  mutate(firmecat = ifelse(CONDICION_FIRME == 1, 0, ifelse(CONDICION_FIRME != 1, 1, NA)))

#Creo un df con las variables de interés, y las transformo en factores
datos <- inocenteslog %>% dplyr::select(sexo, firmecat, cat_edad)
datos$sexo <- factor(datos$sexo)
datos$cat_edad <- factor(datos$cat_edad)
datos$firmecat <- factor(datos$firmecat)

#Creo el modelo univariante
modelo <- glm(firmecat ~ sexo,
              data = datos,
              family = "binomial"(link = "logit"))

summary(modelo)
exp(coefficients(modelo))
exp(cbind(coefficients(modelo), confint(modelo)))

# Calculo ahora los modelos con la edad
modelo2 <- glm(firmecat ~ cat_edad*sexo,
               data = datos,
               family = "binomial"(link = "logit"))
summary(modelo2)

# Hay que hacer un test para ver que se ajusta mejor... lrtest! (usar paquete lmtest)
lrtest(modelo2, modelo)

# El modelo multivariante NO se ajusta mejor

### ANÁLISIS POR DÍA DE LA SEMANA
# Creo variable diacat
# L a V → diacat = 0
# S o D → diacat = 1

inocenteslog <- inocenteslog %>% 
  mutate(diacat = weekdays(inocenteslog$FECHA_ACCIDENTE)) %>% 
  mutate(diacat = ifelse(diacat == "sábado" | diacat == "domingo", 1, ifelse(diacat != "sábado" & diacat != "domingo", 0, NA)))
  

#Creo un df con las variables de interés, y las transformo en factores
datos <- inocenteslog %>% dplyr::select(sexo, diacat, cat_edad)
datos$sexo <- factor(datos$sexo)
datos$cat_edad <- factor(datos$cat_edad)
datos$diacat <- factor(datos$diacat)

#Creo el modelo univariante
modelo <- glm(diacat ~ sexo,
              data = datos,
              family = "binomial"(link = "logit"))

summary(modelo)
exp(coefficients(modelo))
exp(cbind(coefficients(modelo), confint(modelo)))

# Calculo ahora los modelos con la edad
modelo2 <- glm(diacat ~ cat_edad*sexo,
               data = datos,
               family = "binomial"(link = "logit"))
summary(modelo2)

# Hay que hacer un test para ver que se ajusta mejor... lrtest! (usar paquete lmtest)
lrtest(modelo, modelo2)

# El modelo multivariante SÍ se ajusta mejor
# Obtengo las OR correspondientes
log_diacat <- lincom(modelo2,
                      c('sexo2',
                        'sexo2+cat_edad2:sexo2',
                        'sexo2+cat_edad3:sexo2',
                        'sexo2+cat_edad4:sexo2',
                        'sexo2+cat_edad5:sexo2',
                        'sexo2+cat_edad6:sexo2',
                        'sexo2+cat_edad7:sexo2',
                        'sexo2+cat_edad8:sexo2',
                        'sexo2+cat_edad9:sexo2',
                        'sexo2+cat_edad10:sexo2',
                        'sexo2+cat_edad11:sexo2',
                        'sexo2+cat_edad12:sexo2',
                        'sexo2+cat_edad13:sexo2'),
                      level = 0.95,
                      eform = T)

log_diacat <- data.frame(matrix(unlist(log_diacat, use.names = T), nrow=13, byrow=F)) %>% 
  cbind(c(1:13))
colnames(log_diacat) <- c("Estimate", "IC_inf", "IC_sup", "Chisq", "Pr(>Chisq)", "cat_edad")
log_diacat <- log_diacat[c(6,1:5)]

saveRDS(log_diacat, file = "OR dia log.rds")
write.xlsx(log_diacat, file = "OR dia log.xls")

#gráfico
logplot <- ggplot(log_diacat) + 
  geom_hline(aes(yintercept = 1)) +
  geom_line(size = 1, color = "#ff6361", aes(x = cat_edad, y = Estimate)) +
  geom_point(size = 2, color = "#ff6361", aes(x = cat_edad, y = Estimate)) +
  geom_ribbon(color = "#ff6361", fill = "#ff6361", aes(x = cat_edad, 
                  ymin = IC_inf, 
                  ymax = IC_sup), 
              alpha = 0.2,
              show.legend = FALSE) +
  scale_x_continuous(name = "Grupo de edad",
                     breaks = c(1:13), 
                     labels = unique(categoria_edad$edad)) +
  scale_y_log10(name = "OR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
  labs(title = "Odds Ratio (OR) de ser mujer que circula en fin de semana.",
       subtitle = "Referencia: entre semana. Intervalo de confianza al 95%",
       caption = "Datos para el periodo 2014-2017")
plot(logplot)

### ANÁLISIS POR CONDICIONES CLIMÁTICAS
# 0 = buena (1)
# 1 = adversa (2-7)

inocenteslog <- inocenteslog %>% 
  mutate(meteocat = ifelse(meteo == 1, 0, ifelse(meteo != 1, 1, NA)))

#Creo un df con las variables de interés, y las transformo en factores
datos <- inocenteslog %>% dplyr::select(sexo, meteocat, cat_edad)
datos$sexo <- factor(datos$sexo)
datos$cat_edad <- factor(datos$cat_edad)
datos$meteocat <- factor(datos$meteocat)

#Creo el modelo univariante
modelo <- glm(meteocat ~ sexo,
              data = datos,
              family = "binomial"(link = "logit"))

summary(modelo)
exp(coefficients(modelo))
exp(cbind(coefficients(modelo), confint(modelo)))

# Calculo ahora los modelos con la edad
modelo2 <- glm(meteocat ~ cat_edad*sexo,
               data = datos,
               family = "binomial"(link = "logit"))
summary(modelo2)

# Hay que hacer un test para ver que se ajusta mejor... lrtest! (usar paquete lmtest)
lrtest(modelo, modelo2)

# El modelo multivariante SÍ se ajusta mejor
# Obtengo las OR correspondientes
log_meteocat <- lincom(modelo2,
                     c('sexo2',
                       'sexo2+cat_edad2:sexo2',
                       'sexo2+cat_edad3:sexo2',
                       'sexo2+cat_edad4:sexo2',
                       'sexo2+cat_edad5:sexo2',
                       'sexo2+cat_edad6:sexo2',
                       'sexo2+cat_edad7:sexo2',
                       'sexo2+cat_edad8:sexo2',
                       'sexo2+cat_edad9:sexo2',
                       'sexo2+cat_edad10:sexo2',
                       'sexo2+cat_edad11:sexo2',
                       'sexo2+cat_edad12:sexo2',
                       'sexo2+cat_edad13:sexo2'),
                     level = 0.95,
                     eform = T)

log_meteocat <- data.frame(matrix(unlist(log_meteocat, use.names = T), nrow=13, byrow=F)) %>% 
  cbind(c(1:13))
colnames(log_meteocat) <- c("Estimate", "IC_inf", "IC_sup", "Chisq", "Pr(>Chisq)", "cat_edad")
log_meteocat <- log_meteocat[c(6,1:5)]

saveRDS(log_meteocat, file = "OR meteo log.rds")
write.xlsx(log_meteocat, file = "OR meteo log.xls")

#gráfico
logplot <- ggplot(log_meteocat) + 
  geom_hline(aes(yintercept = 1)) +
  geom_line(size = 1, color = "#ff6361", aes(x = cat_edad, y = Estimate)) +
  geom_point(size = 2, color = "#ff6361", aes(x = cat_edad, y = Estimate)) +
  geom_ribbon(color = "#ff6361", fill = "#ff6361", aes(x = cat_edad, 
                  ymin = IC_inf, 
                  ymax = IC_sup), 
              alpha = 0.2,
              show.legend = FALSE) +
  scale_x_continuous(name = "Grupo de edad",
                     breaks = c(1:13), 
                     labels = unique(categoria_edad$edad)) +
  scale_y_log10(name = "OR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
  labs(title = "Odds Ratio (OR) de ser mujer que circula en condiciones climáticas adversas.",
       subtitle = "Referencia: condiciones buenas. Intervalo de confianza al 95%",
       caption = "Datos para el periodo 2014-2017")
plot(logplot)  

## REGRESIONES MULTINOMIALES PARA VARIABLES POLITÓMICAS

## TIPO DE VÍA
mviacat <- read_excel("mviacat.xlsx")
colores <- c("1" = "#58508d",
             "2" = "#bc5090",
             "3" = "#ffa600")
categorias <- c("Carretera convencional", 
                "Calle", 
                "Otros")

logplot <- ggplot(mviacat) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = RRR, 
                            color = factor(viacat))) +
    geom_point(size = 2, aes(x = cat_edad, y = RRR, color = factor(viacat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_inf, 
                    ymax = IC_sup,
                    fill = factor(viacat),
                    color = factor(viacat)), 
                alpha = 0.2,
                linetype = "dotted",
                show.legend = TRUE) +
    scale_color_manual(values = colores, 
                       labels = categorias) +
    scale_fill_manual(values = colores,
                      labels = categorias) +
    scale_x_continuous(name = "Grupo de edad",
                       breaks = c(1:13), 
                       labels = unique(categoria_edad$edad)) +
    scale_y_log10(name = "RRR") + theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
    labs(title = "Razón de Riesgos Relativos (RRR) de la asociación del sexo del conductor con respecto a la vía de circulación, por grupos de edad",
         subtitle = "Referencia: autopista/autovía. Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")

plot(logplot)

## HORA DEL DÍA
mhoracat <- read_excel("mhoracat.xlsx")
colores <- c("0" = "red",
             "1" = "#58508d",
             "2" = "#bc5090",
             "3" = "#003f5c")
categorias <- c("0 a 5", 
                "6 a 11", 
                "18 a 23")
logplot <- ggplot(mhoracat) +
  geom_hline(aes(yintercept = 1)) +
  geom_line(size = 1, aes(x = cat_edad, 
                          y = RRR, 
                          color = factor(horacat))) +
  geom_point(size = 2, aes(x = cat_edad, y = RRR, color = factor(horacat))) +
  geom_ribbon(aes(x = cat_edad, ymin = IC_inf, 
                  ymax = IC_sup,
                  fill = factor(horacat),
                  color = factor(horacat)), 
              alpha = 0.2,
              linetype = "dotted",
              show.legend = TRUE) +
  scale_color_manual(values = colores, 
                     labels = categorias) +
  scale_fill_manual(values = colores,
                    labels = categorias) +
  scale_x_continuous(name = "Grupo de edad",
                     breaks = c(1:13), 
                     labels = unique(categoria_edad$edad)) +
  scale_y_log10(name = "RRR") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
  labs(title = "Razón de Riesgos Relativos (RRR) de la asociación del sexo del conductor con respecto a la hora del día, por grupos de edad",
       subtitle = "Referencia: 11h a 17h. Intervalo de confianza al 95%.",
       caption = "Datos para el periodo 2014-2017")

plot(logplot)

## DENSIDAD DE TRÁFICO
mdensicat <- read_excel("mdensicat.xlsx")
colores <- c("0" = "grey",
             "1" = "green",
             "2" = "yellow",
             "3" = "red")
categorias <- c("Nivel verde", "Nivel amarillo", "Nivel rojo")

logplot <- ggplot(mdensicat) +
  geom_hline(aes(yintercept = 1)) +
  geom_line(size = 1, aes(x = cat_edad, 
                          y = RRR, 
                          color = factor(densicat))) +
  geom_point(size = 2, aes(x = cat_edad, y = RRR, color = factor(densicat))) +
  geom_ribbon(aes(x = cat_edad, ymin = IC_inf, 
                  ymax = IC_sup,
                  fill = factor(densicat),
                  color = factor(densicat)), 
              alpha = 0.2,
              linetype = "dotted",
              show.legend = TRUE) +
  scale_color_manual(values = colores, 
                     labels = categorias) +
  scale_fill_manual(values = colores,
                    labels = categorias) +
  scale_x_continuous(name = "Grupo de edad",
                     breaks = c(1:13), 
                     labels = unique(categoria_edad$edad)) +
  scale_y_log10(name = "RRR") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
  labs(title = "Razón de Riesgos Relativos (RRR) de la asociación del sexo del conductor con respecto a la densidad de tráfico, por grupos de edad",
       subtitle = "Referencia: nivel blanco. Intervalo de confianza al 95%.",
       caption = "Datos para el periodo 2014-2017")

plot(logplot)

## CONDICIÓN DE ILUMINACIÓN
mluzcat <- read_excel("mluzcat.xlsx")
colores <- c("0" = "#ffa600",
             "1" = "#ff6361", 
             "2" = "#bc5090", 
             "3" = "#58508d", 
             "4" = "#003f5c")
categorias <- c("Amanecer o atardecer, sin luz artificial", 
                "Amanecer o atardecer, con luz artificial", 
                "Sin luz natural y con iluminación artificial encendida", 
                "Sin luz natural ni artificial")

logplot <- ggplot(mluzcat) +
  geom_hline(aes(yintercept = 1)) +
  geom_line(size = 1, aes(x = cat_edad, 
                          y = RRR, 
                          color = factor(luzcat))) +
  geom_point(size = 2, aes(x = cat_edad, y = RRR, color = factor(luzcat))) +
  geom_ribbon(aes(x = cat_edad, ymin = IC_inf, 
                  ymax = IC_sup,
                  fill = factor(luzcat),
                  color = factor(luzcat)), 
              alpha = 0.2,
              linetype = "dotted",
              show.legend = TRUE) +
  scale_color_manual(values = colores, 
                     labels = categorias) +
  scale_fill_manual(values = colores,
                    labels = categorias) +
  scale_x_continuous(name = "Grupo de edad",
                     breaks = c(1:13), 
                     labels = unique(categoria_edad$edad)) +
  scale_y_log10(name = "RRR") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
  labs(title = "Razón de Riesgos Relativos (RRR) de la asociación del sexo del conductor con respecto a las condiciones de iluminación, por grupos de edad",
       subtitle = "Referencia: luz natural. Intervalo de confianza al 95%.",
       caption = "Datos para el periodo 2014-2017")

plot(logplot)
