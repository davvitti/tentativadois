#' ---
#' title: "LCE0602 - Estatística Experimental - Aulas 2 e 3 "
#' author: 
#'   Renata Alcarde Sermarini e Taciana Villela Savian
#' date: "09/2021"
#' fig width: 10
#' fig height: 4
#' ---
#'
#' # Planejamento de um experimento
#' 
#' Vamos considerar um experimento que será instalado segundo o delineamento
#'  inteiramente casualizado (DIC) com 4 tratamentos e 5 repetições

set.seed(1234) # semente fixa para realização do sorteio
sample(rep(c("A", "B", "C", "D"), 5)) # Forma bastante rápida mas sem croqui


#' ## Usando a biblioteca agricolae

# Instalando o pacote "agricolae"
# install.packages("agricolae", dependencies = TRUE)

# Habilitando as funções
library(agricolae)
trt = LETTERS[1:4]
delineamento <- design.crd(trt,
                           r = 5,
                           serie = 0)
delineamento

# Graficamente

library(agricolaeplotr)
# função para fazer o gráfico do croqui de campo
plot_design_crd(delineamento,
                ncols = 4,
                nrows = 5)

# Para montar um croqui precisamos de um gride, definido por linhas e colunas
delineamento$book$Linha <- rep(1:5, each = 4)
delineamento$book$Coluna <- rep(1:4, times = 5)
# listagem para a aleatorização dos tratamentos
delineamento$book


# install.packages("ggplot2", 
#                  dependencies = TRUE)

#' # Análise dos dados
#' 
#' ## Entrada dos dados 
#' 
#' Importar os dados diretamente no RStudio (Import Dataset)

summary(aula2)

#' ## Análise exploratória dos dados
#' usamos pacote ggplot2 para elaboração de gráficos exploratórios
library(ggplot2)

ggplot(aula2, # qual o arquivo de dados? 
       aes(x = trat, # aes(x=, y=) quem fica em cada eixo?
           y = y)) +
  geom_point() +     # qual o tipo de gráfico? geom_point-> gráfico de pontos
  geom_point(stat = "summary",
             fun = mean, # calcular a produtividade média/tratamento
             col = "red") +  # adicionar os pontos (em vermelho) no gráfico 
  annotate("point", # adicionar pontos (em azul) relacionados à média geral dos dados
           x = aula2$trat, 
           y = 26.75, # média geral dos dados
           colour = "blue") +
  xlab("tratamentos") +
  ylab("produtividade")

#' Usando a função ggplot para fazer o gráfico de caixas (box-plot)
#' por tratamento

 ggplot(aula2, 
        aes(x = trat, 
            y = y)) +
   geom_boxplot()

#' ## Estatísticas descritivas
#' 
#' função with(com quem?, o que?)
#' função tapply(com quem?, estrato?,qual a função/conta?)
n <- with(aula2, tapply(y,trat, length))
soma <- with(aula2, tapply(y,trat,sum))
media <- with(aula2, tapply(y,trat,mean))
variancia <- with(aula2, tapply(y,trat,var))
desv.padr <- with(aula2, tapply(y,trat,sd))
dist.int <- with(aula2, tapply(y,trat,IQR))

#' Criando uma função que calcula a amplitude
f1 <- function(x) max(x)-min(x)
amplitude <- with(aula2, tapply(y,trat,f1))

#' rbind -> junção de objetos por linha
resumo <- rbind(n, soma, media, variancia,
                desv.padr, amplitude,dist.int)
#' cbind -> junção de objetos por coluna
resumo2 <- cbind(n, soma, media, variancia,
                desv.padr, amplitude,dist.int)

#' rownames(renomear as linhas)
rownames(resumo) <- c("n", "Soma", "Média", 
                      "Variância", "Desvio-padrão", 
                      "Amplitude", "Amplitude Interquartílica")

#' round(arredondar os valores para o número de casas decimais desejado)
round(resumo,3)


#' ## Análise de variância  
#' 
#' $H_0$: $\mu_1 = \mu_2 = \mu_3 = \mu_4$ *versus* 
#' $H_1:$ Pelo menos duas médias de tratamentos diferem entre si.
#' 
modelo <- aov(y ~ trat, aula2)
anova(modelo)

#' # Comparações múltiplas
#' 
#' Instalar o pacote ExpDes se necessário.
install.packages("ExpDes.pt")
#' Carregar o pacote ExpDes.
library(ExpDes.pt)
#' Pedindo ajuda sobre o delineamento inteiramente casualizado
?dic
#' ## Teste t sem correção de Bonferroni
#' 
#' função with(com quem?-> arquivo, o que?-> função "dic" da ExpDes)
with(aula2,
     dic(trat, #tratamentos
         y,    # variável resposta  
         quali = TRUE, #tratamentos qualitativos? verdadeiro/falso
         mcomp = "lsd",# escolha do teste de comparação múltipla
         sigF = 0.05,  # significância do teste F da ANAVA
         sigT = 0.05)) # significância do teste de comparação médias

#' ## Teste t com correção de Bonferroni
with(aula2,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "lsdb",
         sigF = 0.05, 
         sigT = 0.05))

#' ## Teste de Tukey
with(aula2,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "tukey",
         sigF = 0.05, 
         sigT = 0.05))

#' ## Teste de Duncan
with(aula2,
     dic(trat, 
         y, 
         quali = TRUE, 
         mcomp = "duncan",
         sigF = 0.05, 
         sigT = 0.05))

#' # Teste de Dunnett (sendo B o genótipo de referência)
install.packages("multcomp") # instalando pacote
library(multcomp) # carregando pacote
aula2$trat <- as.factor(aula2$trat) # definindo trat como fator
levels(aula2$trat) # visualizando os níveis do fator
modelo = aov(y ~ trat, aula2) # realizando a análise de variância
?glht # ajuda sobre a função glht

summary(glht(modelo,
             linfct = mcp(trat = rbind("A-B" = c(1, -1, 0, 0),
                                       "C-B" = c(0, -1, 1, 0),
                                       "D-B" = c(0, -1, 0, 1)))))
media <- with(aula2, tapply(y,trat,mean))
media
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

