#Lista_04

##Questão 02####
#a

install.packages("readxl")
library(readxl)

setwd("C:/GitHub/Lista_4/Lista_4/dados_encontro_2_ufpe")

Atlas_2013 <- read_xlsx("atlas2013_dadosbrutos_pt.xlsx")
View(Atlas_2013)

#b

View(docentes_pe)

install.packages("magrittr")
library("magrittr")

install.packages("dplyr")
library(dplyr)

load("docentes_pe_censo_escolar_2016.RData")

names(docentes_pe)

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)

dim(docentes_pe_selecao)

head(docentes_pe_selecao)

#c

load("matricula_pe_censo_escolar_2016.RData")
View(matricula_pe) 

names(matricula_pe)

matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)

dim(matricula_pe_selecao)

head(matricula_pe_selecao)
