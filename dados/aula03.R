#' ---
#' title: "LCE0602 - Estatística Experimental - Aula03"
#' author: ""
#' date: ""
#' ---
#' 
#' # Análise dos dados
#' 
#' ## Entrada dos dados 
#' Copiar o arquivo aula2.csv para seu computador

dados <- read.csv2("G:/Meu Drive/ESALQ/2020/Graduacao/Experimental/R/aula03/aula2.csv")

summary(dados)

#' ## Análise exploratória
library(ggplot2)

ggplot(dados, aes(x = trat, y = y)) +
  geom_point()


#' # Análise de variância  
modelo = aov(y ~ trat, dados)
anova(modelo)

#' # Comparações múltiplas

#' ## Teste t com correção de Bonferroni
with(dados, 
     pairwise.t.test(y, 
                     trat, 
                     p.adj = "bonferroni"));

#' ## Teste de Tukey
library(ExpDes.pt)
?dic
with(dados,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "tukey",
         sigF = 0.05, 
         sigT = 0.05))

library(agricolae)
Tukey. <- with(dados,
               HSD.test(y,
                        trat,
                        16,
                        7.00))
Tukey.
plot(Tukey.)


#' ## Teste de Duncan
with(dados,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "duncan",
         sigF = 0.05, 
         sigT = 0.05))

# função da biblioteca agricolae
Duncan. <- with(dados,
                duncan.test(y,
                            trat,
                            16,
                            7.00))
Duncan.
plot(Duncan.)



#' # Teste de Dunnett (sendo B o genótipo de referência)
library(multcomp)
dados$trat <- as.factor(dados$trat)
modelo = aov(y ~ trat, dados)
summary(glht(modelo,
             linfct = mcp(trat =c("A-B == 0", 
                                  "C-B == 0",
                                  "D-B == 0"))))


#' # Contrastes ortogonais
summary(glht(modelo,
             linfct = mcp(trat = c("A+B-C-D == 0"))))
summary(glht(modelo,
             linfct = mcp(trat = c("A-B == 0"))))
summary(glht(modelo,
             linfct = mcp(trat = c("C-D == 0"))))


#' # Outro exemplo de contrastes ortogonais
#' 
#' Aqui serão testadas as hipóteses
#' 
#' $H_0: 3\mu_A -\mu_B-\mu_C-\mu_D = 0$ *versus* $H_1: 3\mu_A -\mu_B-\mu_C-\mu_D \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = c("3*A-B-C-D == 0"))))

#' $H_0: \mu_B+\mu_C-2\mu_D = 0$ *versus* $H_1: \mu_B+\mu_C-2\mu_D \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = c("B+C-2*D == 0"))))

#' $H_0: \mu_B-\mu_C = 0$ *versus* $H_1: \mu_B-\mu_C \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = c("B-C == 0"))))

#' Ainda, os mesmos contrastes podem ser apresentados em termos dos coeficientes apenas, como segue.
#' 

#' $H_0: 3\mu_A -\mu_B-\mu_C-\mu_D = 0$ *versus* $H_1: 3\mu_A -\mu_B-\mu_C-\mu_D \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = c(3,-1,-1,-1))))

#' $H_0: \mu_B+\mu_C-2\mu_D = 0$ *versus* $H_1: \mu_B+\mu_C-2\mu_D \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = c(0,1,1,-2))))

#' $H_0: \mu_B-\mu_C = 0$ *versus* $H_1: \mu_B-\mu_C \neq 0$
summary(glht(modelo,
             linfct = mcp(trat = c(0,1,-1,0))))