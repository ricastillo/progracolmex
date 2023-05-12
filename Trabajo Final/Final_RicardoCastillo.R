#--------------------------------------------------------------------------------
# Tema:       Trabajo final. Regresion logistica
# Autor:      Ricardo Castillo Pérez <rickykcastillop47@gmail.com>
# Fecha:      09-05-2023
# Datos:      V-Dem
#--------------------------------------------------------------------------------


#0.  Preparar entorno de trabajo
rm(list=ls()); graphics.off(); options(warn=-1)              


#1. Cargar librerias

library(foreign)                 
library(questionr)              
library(tidyverse) 
library(dplyr) 
library(stats) 
library(survey) 
library(vcd)

#Cargar base

ruta="C:/Users/ricky/OneDrive/Escritorio/Ricardo/El Colegio de México/Sexto semestre/Programación/Archivos Rmd/"

setwd(paste(ruta,"/Datos", sep = ""))
vdem=read.csv("VDEM.csv")

#Seleccionar datos

parp1<-vdem%>%
  filter(year==2015 |year==2016 | year==2017 |year==2018 |year==2019) %>%
  select("country_name","year","v2dlengage_osp", 
         "v2cacamps_osp", "e_ti_cpi", "v2x_rule", "v2x_accountability_osp",
         "v2x_civlib", "v2csreprss_osp", "v2xpe_exlecon")

#Tenemos NA's, hay que eliminarlos
parp2 <- na.omit(parp1)

#Pregunta guia
#¿Cuales son los factores que influyen para que las personas participen políticamente?


#5. Regresion logistica

#5.1 Variable dependiente/objetivo

# Engaged society (C) (v2dlengage_osp)
#0: Deliberación pública prohibida

#1: Deliberación limitada y gran parte de los grupos no élites desconocen el proceso

#2: Deliberación sin restricciones, pero infrecuente y en dominio de élites

#3: Deliberación parcial, pero dominada por ciertos grupos con intereses similares

#4: Deliberación frecuente y variada

#5: Deliberación elevada, constante y común entre varios grupos y entornos.

# Recodificaremos con 0= Bajo y 1= Alto. El objetivo es encontrar factores que
# la aumenten o disminuyan, sn considerar regímenes que la obstaculizen de manera
# directa, por lo que utilizaremos las categorías 2 a 5.

#table(vdem$v2dlengage_osp)


#Redondear valores

parp3 <- parp2 %>%
  mutate(delib_r = round(v2dlengage_osp)) %>% 
  mutate(polz_r = round(v2cacamps_osp)) %>%
  mutate(csrep_r = round(v2csreprss_osp))


#Eliminar las variables originales (con decimales)

parp4 <- parp3 %>%
  select( -"v2dlengage_osp",-"v2cacamps_osp", -"v2csreprss_osp")

## Recodificación de variables

# Variable dependiente: Deliberación (delib_r)
parp5<-filter(parp4,delib_r>=2)
table(parp5$delib_r)

# Creacion de la variable binaria (0/1)
parp5$deliberacion=0
parp5$deliberacion[parp5$delib_r==4 |parp5$delib_r==5]<-1
parp5$deliberacion[parp5$delib_r==2 |parp5$delib_r==3]<-0

table(parp5$deliberacion)


#5.2 Variables independientes/covariables

# INDEPENDIENTES: 

#1.CSO repression (C) (csrep_r); 
#2.Political polarization (C) (polz_r);
#3.Accountability index (D) (v2x_accountability)
#4.Corruption perception index (E) (e_ti_cpi)
#5.Rule of law index (D) (v2x_rule)


# Represión a organizaciones de la sociedad civil (csrep_r)

#Escala de 0 a 4, donde 0 representa represión severa y 4 nula represión
#Recodificaremos como "Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"

parp5$represosc=parp5$csrep_r
parp5$represosc<-factor(parp5$represosc, 
                        levels =c(0,1,2,3,4), labels=c("Muy alto", "Alto",
                                                       "Medio", "Bajo",
                                                       "Muy bajo"))

# Polarización Política (polz_r)

#Escala de 0 a 4, donde 0 es casi inexistente y 4 es muy polarizado
#La recodificaremos como "Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"

parp5$polarizacion=parp5$polz_r
parp5$polarizacion<-factor(parp5$polarizacion, 
                           levels =c(0,1,2,3,4), labels=c("Muy bajo", "Bajo",
                                                          "Medio", "Alto",
                                                          "Muy alto"))

# índice de rendición de cuentas (v2x_accountability_osp)

#Escala continua decimal de 0 a 1, con 0 como "bajo índice" y 1 como "alto índice"

table(parp5$v2x_accountability_osp)
parp5$rendctas=parp5$v2x_accountability_osp

# índice de percepción de la corrupción (e_ti_cpi)

#Escala continua de 0 a 100, con 0 como "altamente corrupto" y 1 como "altamente limpio"

table(parp5$e_ti_cpi)
parp5$corrupcion=parp5$e_ti_cpi

# Imperio de la ley / Justicia procedimental (v2x_rule)

#Escala continua decimal de 0 a 1, con 0 como "bajo índice" y 1 como "alto índice"

table(parp5$v2x_rule)
parp5$leyes=parp5$v2x_rule

# Indice de libertaades civiles

#Escala continua decimal de 0 a 1, con 0 como "bajo índice" y 1 como "alto índice"

table(parp5$v2x_civlib)
parp5$libertades=parp5$v2x_civlib

# Exclusion por nivel socioeconómico

table(parp5$v2xpe_exlecon)
parp5$exclusion=parp5$v2xpe_exlecon

# Seleccionar las variables
parp6<-parp5%>%select(country_name, year, deliberacion, polarizacion,
                      rendctas, corrupcion, leyes, represosc, libertades, exclusion)

#5.3 Ajustar el modelo

reg.log <- glm(deliberacion ~ +polarizacion+represosc+leyes+libertades+rendctas+corrupcion+exclusion,
               data = parp6, family = "binomial")
summary(reg.log)

#Al revisar los momios, el resultado del modelo con la variable de "rendctas"
#muestra una probabilidad extremadamente elevada, por lo que la retiraremos

reg.log2 <- glm(deliberacion ~ +polarizacion+represosc+leyes+libertades+corrupcion+exclusion,
                data = parp6, family = "binomial")
summary(reg.log)

#5.4 Resultados: momios

momios<-exp(coefficients(reg.log))%>%data.frame()
View(momios)
momiosstr <- format(momios, scientific = FALSE)
View(momiosstr)

momios2<-exp(coefficients(reg.log2))%>%data.frame()
View(momios2)
momiosstr2 <- format(momios2, scientific = FALSE)
View(momiosstr2)
#Prueba ANOVA
anova(reg.log, test ='Chisq')
anova(reg.log2, test ='Chisq')

#Entre los dos modelos, el primero es de mejor calidad estadística y habrá que 
#revisar el problema con la variable de rendición de cuentas.

#### ELABORACIÓN DE LOS MAPAS

# Cargar librerias
library(ggplot2); library(sf); library(wesanderson)

# Base de datos asociada shape
world_vdem=read.dbf("World_Countries.dbf")

#Seleccionar los datos en data frame

#Filtrar por año (2018)
map1 <- parp6 %>% filter(year== 2018) %>% 
  select("country_name", "year", "deliberacion", "corrupcion")

#Juntar los datos con el dbf

world_vdem=world_vdem%>%left_join(map1,
                                  by = c("COUNTRY" = "country_name"))
write.dbf(world_vdem,"World_Countries.dbf")

#Cargamos el dbf nuevamente

rm(world_vdem)
world_vdem=read.dbf("World_Countries.dbf")

# Cargar shape
map=st_read("World_Countries.shp")

# Visualizar los datos del mapa para asegurarte de que tienes las variables
View(map)

#Sustituir NAs por "0"
map <- map %>% replace_na(list(year=0, deliberaci = 3, corrupcion = 0))

View(map)

#Mapas

#Deliberación

palette <- wes_palette("Darjeeling2", type= "discrete", n=5)

map %>%
  ggplot() +
  geom_sf(aes(fill = as.factor(deliberaci)), colour = "white", size = 0.07) +
  labs(title = "Participación política (dicotómica)") +
  scale_fill_manual(values = palette, name= "Nivel", labels = c ("Baja", "Alta",
                                                                 "No disponible")) +
  theme_bw()

#Corrupción

palette2 <- wes_palette("Royal1", type= "continuous")

ggplot() +
  geom_sf(data = map, aes(fill = corrupcion), color = "white", size = 0.2) +
  scale_fill_gradientn(colours = palette2, name = "Transparencia", labels = scales::comma) +
  labs(title = "Índice de percepción de la corrupción")
theme_bw()
