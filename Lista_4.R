#Lista_04

##QuestÃ£o 02####
#a

setwd("C:/GitHub/Lista_4/Lista_4/dados_encontro_2_ufpe") #definindo o diretório

load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData") #Carregando todos os dados necessários

if(require(tidyverse)==F)install.packages('tidyverse');require(tidyverse)
if(require(readxl)==F)install.packages('readxl');require(readxl) #instalando e carregando pacotes

PNUD <- read_xlsx("atlas2013_dadosbrutos_pt.xlsx", sheet = 2) #lendo base de dados do PNUD

head(PNUD)
View(PNUD)
unique(PNUD$ANO) #analisando a base de dados

pnud_pe_2010 <- PNUD %>% filter(ANO==2010&UF==26) #filtrando por ano e estado

rm(PNUD)
rm(Atlas_2013) #removendo bases que não serão mais utilizadas

#b

View(docentes_pe) #analisando a base de dados 

names(docentes_pe) #analisando variaveis da base de dados

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70) #filtrando a base de dados por idade

dim(docentes_pe_selecao) #dimensão da base

head(docentes_pe_selecao) #analisando a base

#c

View(matricula_pe) #analisando a base de dados

names(matricula_pe) #analisando as variáveis da base de dados

matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)#filtrando a base de dados por idade 

summary(matricula_pe_selecao$NU_IDADE) #analisando a base de dados do descrição

#d

# Matriculas
matriculas_pe_sel <- matricula_pe_selecao %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n(),
            alunos_media_idade = mean(NU_IDADE),
            alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T),
            alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T),
            alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T),
            matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T),
            matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T),
            matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T)
  )

# verificacao
dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))
summary(matriculas_pe_sel)

# Docentes
docentes_pe_sel <- docentes_pe_selecao %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n(),
            docentes_media_idade = mean(NU_IDADE),
            docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T),
            docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T)
  )

# verificacao
dim(docentes_pe_sel)[1] == length(unique(docentes_pe$CO_MUNICIPIO))
summary(docentes_pe_sel)

# matriculas
docentes_matriculas_pe_sel <- docentes_pe_sel %>% full_join(matriculas_pe_sel,
                                                            by = c("CO_MUNICIPIO" = "CO_MUNICIPIO")
)

View(docentes_matriculas_pe_sel)

# Média Aritmética - Alunos por docentes

mean(docentes_matriculas_pe_sel$n_matriculas)/mean(docentes_matriculas_pe_sel$n_docentes)

#Mediana - Alunos por docentes

median(docentes_matriculas_pe_sel$n_matriculas)/median(docentes_matriculas_pe_sel$n_docentes)

