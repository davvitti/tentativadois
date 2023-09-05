#' ---
#' title: "LCE0602 - Estatística Experimental - Aula03"
#' author: 
#'   - Clarice Garcia Borges Demétrio
#'   - Cristian Villegas
#'   - Renata Alcarde Sermarini
#'   - Taciana Villela Savian
#' date: "09/2022"
#' ---
#' 
# knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

#'
#' # Análise dos dados
#' 
#' ## Entrada dos dados 
#' Copiar o arquivo aula2.csv para seu computador

# dados <- read.csv2("aula2.csv")
dados <- read.csv2(choose.files())

summary(dados)

#' ## Análise exploratória
library(ggplot2)

ggplot(dados, aes(x = trat, y = y)) +
  geom_point()


#' # Análise de variância  
modelo = aov(y ~ trat, dados)
anova(modelo)

#' # Comparações múltiplas

#' ## Teste t sem correção de Bonferroni
library(ExpDes.pt)
?dic
with(dados,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "lsd",
         sigF = 0.05, 
         sigT = 0.05))

#' ## Teste t com correção de Bonferroni
with(dados,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "lsdb",
         sigF = 0.05, 
         sigT = 0.05))

#' ## Teste de Tukey
with(dados,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "tukey",
         sigF = 0.05, 
         sigT = 0.05))

#' ## Teste de Duncan
with(dados,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "duncan",
         sigF = 0.05, 
         sigT = 0.05))

#' # Teste de Dunnett (sendo B o genótipo de referência)
library(multcomp)
dados$trat <- as.factor(dados$trat)
levels(dados$trat)
modelo = aov(y ~ trat, dados)
summary(glht(modelo,
             linfct = mcp(trat = rbind("A-B" = c(1, -1, 0, 0),
                                       "C-B" = c(0, -1, 1, 0),
                                       "D-B" = c(0, -1, 0, 1)))))
#' # Contrastes ortogonais
# alpha = 0,05
summary(glht(modelo,
             linfct = mcp(trat = rbind("A+B-C-D" = c(1, 1, -1, -1)))))
# alpha = 0,05
summary(glht(modelo,
             linfct = mcp(trat = rbind("A-B" = c(1, -1, 0, 0)))))
# alpha = 0,05
summary(glht(modelo,
             linfct = mcp(trat = rbind("C-D" = c(0, 0, 1, -1)))))
# conjuntamente alpha' = 1-(1-0,05)^3 = 0,1426 aprox 0,15




#' # Outro exemplo de contrastes ortogonais
#' 
#' Aqui serão testadas as hipóteses
#' 
#' $H_0: 3\mu_A -\mu_B-\mu_C-\mu_D = 0$ *versus* $H_1: 3\mu_A -\mu_B-\mu_C-\mu_D \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = rbind("3*A-B-C-D" = c(3, -1, -1, -1)))))

#' $H_0: \mu_B+\mu_C-2\mu_D = 0$ *versus* $H_1: \mu_B+\mu_C-2\mu_D \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = rbind("B+C-2*D" = c(0, 1, 1, -2)))))

#' $H_0: \mu_B-\mu_C = 0$ *versus* $H_1: \mu_B-\mu_C \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = rbind("B-C" = c(0, 1, -1, 0)))))

