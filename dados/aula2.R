#' ---
#' title: "LCE0602 - Estatística Experimental - Aula02"
#' author: 
#'   - Clarice Garcia Borges Demétrio
#'   - Cristian Villegas
#'   - Renata Alcarde Sermarini
#'   - Taciana Villela Savian
#' date: "08/2022"
#' fig width: 10
#' fig height: 4
#' ---
#' Considere o caso em que se pretende avaliar a produtividade de milho de quatro diferentes variedades. 
#' Para tanto será utilizado o delineamento inteiramente casualizado com cinco repetições.
#' 
#' # Planejamento de um experimento
set.seed(1234)
sample(rep(c("A", "B", "C", "D"), 5))


#' ## Usando a biblioteca agricolae

# Instalando
# install.packages("agricolae", 
#                  dependencies = TRUE)
# Habilitando as funções
library(agricolae)
trt = LETTERS[1:4]
delineamento <- design.crd(trt,
                           r = 5,
                           serie = 0)
delineamento

# Graficamente

# install.packages("agricolaeplotr", 
#                  dependencies = TRUE)
library(agricolaeplotr)
plot_design_crd(delineamento,
                ncols = 4,
                nrows = 5)

# Para montar um croqui precisamos de um gride, definido por linhas e colunas
delineamento$book$Linha <- rep(1:5, each = 4)
delineamento$book$Coluna <- rep(1:4, times = 5)

delineamento$book


# install.packages("ggplot2", 
#                  dependencies = TRUE)
library(ggplot2)
ggplot(delineamento$book,
       aes(x = Coluna,
           y = Linha,
           fill = trt,
           label = plots)) +
  geom_tile(color="black") +
  geom_text() +
  xlab("") +
  ylab("") #+
  # guides(fill = guide_legend(title="tratamentos"))


#' # Análise dos dados
#' 
#' ## Entrada dos dados 
#' Copiar o arquivo aula2.csv para seu computador

# dados <- read.csv2(choose.files())
# setwd("G:/Meu Drive/ESALQ/2022/Graduacao/Experimental/R/aula02")
dados <- read.csv2("aula2.csv")
summary(dados)


#' ## Análise exploratória
library(ggplot2)
ggplot(dados, 
       aes(x = trat, 
           y = y)) +
  geom_point() +
  geom_point(stat = "summary",
             fun = mean,
             col = "red") +
  annotate("point", 
           x = dados$trat, 
           y = 26.75, 
           colour = "blue") +
  xlab("tratamentos") +
  ylab("produtividade")

ggplot(dados,
       aes(x = trat,
           y = y)) +
  geom_boxplot()

#' ## Estatísticas descritivas
n <- with(dados, tapply(y,trat, length))
soma <- with(dados, tapply(y,trat,sum))
media <- with(dados, tapply(y,trat,mean))
variancia <- with(dados, tapply(y,trat,var))
desv.padr <- with(dados, tapply(y,trat,sd))
dist.int <- with(dados, tapply(y,trat,IQR))

#' Criando uma função que calcula a amplitude
f1 <- function(x) max(x)-min(x)
amplitude <- with(dados, tapply(y,trat,f1))

resumo <- rbind(n, soma, media, variancia,
                desv.padr, amplitude,dist.int)
rownames(resumo) <- c("n", "Soma", "Média", 
                      "Variância", "Desvio-padrão", 
                      "Amplitude", "Amplitude Interquartílica")
round(resumo,3)


#' ## Análise de variância  
#' 
#' $H_0$: $\mu_1 = \mu_2 = \mu_3 = \mu_4$ *versus* 
#' $H_1:$ Pelo menos duas médias de tratamentos diferem entre si.
#' 
modelo <- aov(y ~ trat, dados)
anova(modelo)

