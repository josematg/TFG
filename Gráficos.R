library(tidyverse)
library(readxl)

## X = CAT EDAD, Y = OR CRUDAS, GR?FICOS POR A?O
ortotal <- read_excel("Resultados/or_crudas.xls", sheet = "Total")
ortotal <- ortotal[,-1]
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("Población de conductores vs población general" = "#003f5c",
             "Población de expuestos vs población general" = "#bc5090",
             "Población de expuestos vs población de conductores" = "#ffa600")
categorias <- c("Población de conductores vs población general",
                "Población de expuestos vs población general",
                "Población de expuestos vs población de conductores")

orplot <- ggplot(ortotal[-1,]) +
  geom_hline(aes(yintercept = 1)) +
  geom_line(size = 1, aes(x = cat_edad,
                          y = or_muj_1,
                          color = factor(categorias[1])))+
  geom_point(size = 2, aes(x = cat_edad,
                           y = or_muj_1,
                           color = factor(categorias[1])))+
  geom_line(size = 1, aes(x = cat_edad,
                          y = or_muj_2,
                          color = factor(categorias[2])))+
  geom_point(size = 2, aes (x = cat_edad,
                            y = or_muj_2,
                            color = factor(categorias[2])))+
  geom_line(size = 1, aes(x = cat_edad,
                          y = or_muj_3,
                          color = factor(categorias[3])))+
  geom_point(size = 2, aes (x = cat_edad,
                            y = or_muj_3,
                            color = factor(categorias[3])))+
  geom_ribbon(aes(x = cat_edad, 
                  ymin = or_muj_1, 
                  ymax = or_muj_1,
                  fill = factor(categorias[1]),
                  color = factor(categorias[1])), 
              alpha = 0.2,
              linetype = "dotted",
              show.legend = T) +
  geom_ribbon(aes(x = cat_edad, 
                  ymin = IC_OR2_inf, 
                  ymax = IC_OR2_sup,
                  fill = factor(categorias[2]),
                  color = factor(categorias[2])), 
              alpha = 0.2,
              linetype = "dotted",
              show.legend = T) +
  geom_ribbon(aes(x = cat_edad, 
                  ymin = IC_OR3_inf, 
                  ymax = IC_OR3_sup,
                  fill = factor(categorias[3]),
                  color = factor(categorias[3])), 
              alpha = 0.2,
              linetype = "dotted",
              show.legend = T) +
  scale_color_manual(values = colores, 
                     labels = categorias) +
  scale_fill_manual(values = colores,
                    labels = categorias) +
  theme_bw() +
  scale_x_continuous(name = "Grupo de edad",
                     breaks = c(1:13), 
                     labels = unique(categoria_edad$edad)) +
  scale_y_log10(name = "OR") +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
  labs(title = "Odds Ratio (OR) de ser mujer entre las poblaciones de estudio por grupos de edad",
       subtitle = "Intervalo de confianza al 95%",
       caption = "Datos para el periodo 2014-2017")

plot(orplot)

## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR ZONA
or_zona <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por zona.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "#003f5c",
             "1" = "#ff6361")
categorias <- c("Zona urbana",
                "Zona no urbana")
orplot2 <- function(df){
  dfa <- df[-c(1,2),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(zonacat))) +
    geom_point(size = 2, aes(x = cat_edad, y = OR_DGT, color = factor(zonacat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
                    fill = factor(zonacat),
                    color = factor(zonacat)), 
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
    theme_bw()+
    scale_y_log10(name = "OR") +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de expuestos vs población de conductores) por grupos de edad y zona",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_zona)

## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR FIRME
or_firme <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por firme.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "#003f5c",
             "1" = "#ff6361")
categorias <- c("Firme seco y limpio",
                "Firme no seco o limpio")
orplot2 <- function(df){
  dfa <- df[-c(1,2),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(firmecat))) +
    geom_point(size = 2, aes(x = cat_edad, 
                             y = OR_DGT, 
                             color = factor(firmecat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
                    fill = factor(firmecat),
                    color = factor(firmecat)), 
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
    theme_bw() +
    scale_y_log10(name = "OR") +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de expuestos vs población de conductores) por grupos de edad y estado del firme",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_firme)

## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR DIA
or_dia <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por dia.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "#003f5c",
             "1" = "#ff6361")
categorias <- c("Lunes a viernes",
                "Sábado a domingo")
orplot2 <- function(df){
  dfa <- df[-c(1,2),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(diacat))) +
    geom_point(size = 2, aes(x = cat_edad, 
                             y = OR_DGT, 
                             color = factor(diacat))) +
    geom_ribbon(aes(x = cat_edad, 
                    ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
                    fill = factor(diacat),
                    color = factor(diacat)), 
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
    scale_y_log10(name = "OR") +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de expuestos vs población de conductores) por grupos de edad y día de la semana",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_dia)

## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR METEO
or_meteo <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por meteo.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "#003f5c",
             "1" = "#ff6361")
categorias <- c("Condiciones buenas",
                "Condiciones adversas")
orplot2 <- function(df){
  dfa <- df[-c(1,2),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(meteocat))) +
    geom_point(size = 2, aes(x = cat_edad, 
                             y = OR_DGT, 
                             color = factor(meteocat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
                    fill = factor(meteocat),
                    color = factor(meteocat)), 
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
    scale_y_log10(name = "OR") +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de expuestos vs población de conductores) por grupos de edad y condiciones climáticas",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_meteo)

## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR via
or_via <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por via.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "#003f5c",
             "1" = "#58508d",
             "2" = "#bc5090",
             "3" = "#ffa600")
categorias <- c("Autopista o autovía",
                "Carretera convencional", 
                "Calle", 
                "Otros")
orplot2 <- function(df){
  dfa <- df[-c(1,2,3,4),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(viacat))) +
    geom_point(size = 2, aes(x = cat_edad, y = OR_DGT, color = factor(viacat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
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
    scale_y_log10(name = "OR") +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de expuestos vs población de conductores) por grupos de edad y tipo de vía",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_via)

## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR hora
or_hora <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por hora.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "#ffa600",
             "1" = "#58508d",
             "2" = "#bc5090",
             "3" = "#003f5c")
categorias <- c("0 a 5","6 a 11", "12 a 17", "17 a 23")

orplot2 <- function(df){
  dfa <- df[-c(1,2,3,4),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(horacat))) +
    geom_point(size = 2, aes(x = cat_edad, y = OR_DGT, color = factor(horacat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
                    fill = factor(horacat),
                    color = factor(horacat)), 
                alpha = 0.2,
                linetype = "dotted",
                show.legend = T) +
    scale_color_manual(values = colores, 
                       labels = categorias) +
    scale_fill_manual(values = colores,
                      labels = categorias) +
    scale_x_continuous(name = "Grupo de edad",
                       breaks = c(1:13), 
                       labels = unique(categoria_edad$edad)) +
    scale_y_log10(name = "OR") +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 270, vjust = 0.2, debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de expuestos vs población de conductores) por grupos de edad y hora del día",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_hora)

## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR luz
or_luz <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por luz.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "#ffa600",
         "1" = "#ff6361", 
         "2" = "#bc5090", 
         "3" = "#58508d", 
         "4" = "#003f5c")
categorias <- c("Luz natural",
                "Amanecer o atardecer, sin luz artificial", 
                "Amanecer o atardecer, con luz artificial", 
                "Sin luz natural y con iluminación artificial encendida", 
                "Sin luz natural ni artificial")

orplot2 <- function(df){
  dfa <- df[-c(1,2,3,4,5),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(luzcat))) +
    geom_point(size = 2, 
               aes(x = cat_edad,
                   y = OR_DGT, 
                   color = factor(luzcat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
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
    scale_y_log10(name = "OR") +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, 
                                     vjust = 0.2, 
                                     debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de conductores vs población de expuestos) por grupos de edad y condiciones de
iluminación",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_luz)


## X = GRUPO DE EDAD, Y = OR AJUSTADAS POR densi
or_densi <- readRDS("~/Archivos Medicina/TFG/Raw Data/Resultados/OR ajustadas por densi.rds")
categoria_edad <- readRDS("~/Archivos Medicina/TFG/Raw Data/Archivos corregidos/categoria_edad.rds")
colores <- c("0" = "grey",
             "1" = "green",
             "2" = "yellow",
             "3" = "red")
categorias <- c("Nivel blanco","Nivel verde", "Nivel amarillo", "Nivel rojo")

orplot2 <- function(df){
  dfa <- df[-c(1,2,3,4),]
  plot_ano <- ggplot(dfa) +
    geom_hline(aes(yintercept = 1)) +
    geom_line(size = 1, aes(x = cat_edad, 
                            y = OR_DGT, 
                            color = factor(densicat))) +
    geom_point(size = 2, aes(x = cat_edad, y = OR_DGT, color = factor(densicat))) +
    geom_ribbon(aes(x = cat_edad, ymin = IC_OR_DGT_inf, 
                    ymax = IC_OR_DGT_sup,
                    fill = factor(densicat),
                    color = factor(densicat)), 
                alpha = 0.2,
                linetype = "dotted",
                show.legend = T) +
    scale_color_manual(values = colores, 
                       labels = categorias) +
    scale_fill_manual(values = colores,
                      labels = categorias) +
    scale_x_continuous(name = "Grupo de edad",
                       breaks = c(1:13), 
                       labels = unique(categoria_edad$edad)) +
    scale_y_log10(name = "OR") +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.2, debug = FALSE)) +
    labs(title = "Odds Ratio (OR) de ser mujer (población de expuestos vs población de conductores) por grupos de edad y densidad de tráfico",
         subtitle = "Intervalo de confianza al 95%",
         caption = "Datos para el periodo 2014-2017")
  plot(plot_ano)
}

orplot2(or_densi)
