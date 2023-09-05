#' ---
#' title: "Pressuposições da ANOVA"
#' author: 
#'   - Clarice Garcia Borges Demétrio
#'   - Cristian Villegas
#'   - Renata Alcarde Sermarini
#'   - Taciana Villela Savian
#' date: "outubro de 2022"
#' header-includes:
#'   - \usepackage[utf8]{inputenc}
#'   - \usepackage{multicol}
#' ---

#' # Exemplo (Barbin, 1994)
#' Um pesquisador pretende comparar quatro variedades de pêssego quanto ao enraizamento de estacas. Para tanto, realizou um experimento de acordo com o delineamento inteiramente casualizado com cinco repetições, sendo cada parcela um vaso com vinte estacas. Passado o tempo necessário, o pesquisador anotou o número de estacas enraizadas.

library(xlsx)
dados <- read.xlsx(file.choose(), 1)
# dados <- read.xlsx("estacas.xlsx", 1)
str(dados)

library(ggplot2)
ggplot(dados,
       aes(x = trat,
           y = y)) +
  geom_point(position = position_jitter(w = 0.2, h = 0)) +
  theme_bw() +
  ylab("Resíduos estudentizados") +
  xlab("Variedade") 

#' ## Análise de variância - ajuste do modelo

modelo <- lm(y ~ trat, dados)

#' ## Obtenção dos resíduos

(res <- residuals(modelo)) # resíduos simples
(res_Stud <- rstandard(modelo)) # resíduos estudentizados


#' - Observações discrepantes

boxplot(res_Stud)

#' São observados dois valores discrepantes para os resíduos, quando esperada a normalidade dos erros. Logo, não se observa 95\% dos resíduos entre -2 e 2, entretanto 100\% destes estão entre -3 e 3.


ggplot(dados,
       aes(x = trat,
           y = res_Stud)) +
  geom_point(position = position_jitter(w = 0.3, h = 0)) +
  theme_bw() +
  ylab("Resíduos estudentizados") +
  xlab("Variedade") 

#' Observa-se que os dois valores discrepantes estão relacionados às variedades C e D. A dispersão dos resíduos por tratamento será discutida posteriormente.
#'
#' ## Homogeneidade de Variâncias
#' 
ggplot(dados,
       aes(x = trat,
           y = res_Stud)) +
  geom_point() +
  theme_bw() +
  ylab("Resíduos Estudentizados") +
  xlab("Variedade")

#' Observa-se que as dispersões dos resíduos estudentizados associados às variedades A e B são menores do que as dispersões dos resíduos estudentizados associados às variedades C e D, aparentemente.

#'
#' - Teste de hipóteses: Teste de Levene (1960)
#' $$\begin{array}{l}
#' H_0: \mbox{Há homogeneidade de variâncias}\\
#' H_a: \mbox{Não há homogeneidade de variâncias}
#' \end{array}$$

library(lawstat)
levene.test(res_Stud, dados$trat, location = "mean")

#' Como o valor-p = 0,06786 > 0,05 = $\alpha$, considerando-se o nível de 5\% de significância não rejeitamos $H_0$. Logo, não há evidências para afirmarmos que as variâncias não são homogêneas.

#' ## Normalidade dos Erros

qqnorm(res_Stud)
qqline(res_Stud, col=2)

#' Observa-se o afastamento de dois pontos da reta que passa pelos pontos ($Q_{1_{esp}}$, $Q_{1_{obs}}$) e ($Q_{3_{esp}}$, $Q_{3_{obs}}$). Desse modo, espera-se que os erros não sigam uma distribuição normal.


library(hnp)
hnp(modelo,
    print.on = TRUE)

#' É aceitável que pelo menos 95\% dos pontos pertençam ao intervalo de confiança gerado. Para o exemplo, tem-se mais de 5\% destes foram do intervalo. Assim, é esperado que os erros não sigam uma distribuição normal.
#'
#' - Teste de hipóteses: Shapiro-Wilk (1965)
#'
#' $$\begin{array}{l}
#' H_0: \mbox{Os erros seguem uma distribuição normal}\\
#' H_a: \mbox{Os erros não seguem uma distribuição normal}
#' \end{array}$$

shapiro.test(res_Stud)

#' Como o valor-p = 0,02209 < 0,05 = $\alpha$, considerando-se o nível de 5\% de significância, rejeitamos $H_0$. Logo, há evidências para afirmarmos que os erros não seguem uma distribuição normal.

#' ## Transformação de dados

ggplot(,
       aes(x = fitted(modelo),
           y = res_Stud)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  ylab("Resíduos estudentizados") +
  xlab("Valores esperados (médias)")

#' Observa-se o aumento da dispersão conforme os valores preditos aumentam. Assim, é esperado que uma transformação da família Box-Cox seja necessária.



#' Soma-se uma constante, 0,5, pois há valores observados nulos.

library(MASS)
boxcox(dados$y+0.5 ~ dados$trat,
       ylab="logaritmo da verossimilhança") #lambda=0,5.

#' Observe que o valor 1 não pertence ao intervalo de confiança a 95\% para $\lambda$, porém, o valor 0,5 pertence, sendo este o valor indicado de $\lambda$.

#'
#' # Análise dos Dados Transformados
#'
dados$yt <- (dados$y+0.5)^0.5
modelot <- lm(yt ~ trat, dados)
#'
#' ## Normalidade dos Erros
#'
qqnorm(rstandard(modelot), xlab="Quantis da distribuição 
       normal", ylab="Resíduos estudentizados")
qqline(rstandard(modelot), col=2)

#' Com os dados transformados e novo modelo ajustado, tem-se que 95\% dos resíduos encontram-se entre -2 e 2 e, os pontos não apresentam grande afastamento da reta, o que indica possível normalidade dos erros.

hnp(modelot, print.on = TRUE)

#' Com os dados transformados e novo modelo ajustado, tem-se que todos os pontos pertencem ao envelope simulado, indicando possível normalidade dos erros.

#' $$\begin{array}{l}
#' H_0: \mbox{Os erros seguem uma distribuição normal}\\
#' H_a: \mbox{Os erros não seguem uma distribuição normal}
#' \end{array}$$

shapiro.test(rstandard(modelot))

#' Como o valor-p = 0,8943 > 0,05 = $\alpha$, considerando-se o nível de 5\% de significância, não rejeitamos $H_0$. Logo, há evidências para afirmarmos que os erros seguem uma distribuição normal.

#'
#' ## Homogeneidade de variâncias
#'
ggplot(dados,
       aes(x = trat,
           y = rstandard(modelot))) +
  geom_point(position = position_jitter(w = 0.3, h = 0)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab("Resíduos estudentizados") +
  xlab("Variedade")

#' Aparentemente, as dispersões dos resíduos por tratamento são semelhantes.


#' $$\begin{array}{l}
#' H_0: \mbox{Há homogeneidade de variâncias}\\
#' H_a: \mbox{Não há homogeneidade de variâncias}
#' \end{array}$$

levene.test(rstandard(modelot), dados$trat, location = "mean")

#' Como o valor-p = 0,9258 > 0,05 = $\alpha$, considerando-se o nível de 5\% de significância, não rejeitamos $H_0$. Logo, há evidências para afirmarmos que há homogeneidade de variâncias dos erros.

#'
#' ## Transformação de Dados
#'
ggplot(dados,
       aes(x = fitted(modelot),
           y = rstandard(modelot))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab("Resíduos Estudentizados") +
  xlab("Valores esperados (médias)")

#' Agora, aparentemente, há um leve aumento na dispersão dos resíduos conforme o valor predito aumenta.

boxcox(modelot, ylab="logaritmo da verossimilhança")

#' Quando utilizados os dados transformados, verifica-se que o valor 1 pertence ao intervalo de confiança para $\lambda$. Desse modo, nova transformação não é indicada.
#'
#' ## Análise de Variância
#'
#' Atendidas as pressuposiões para a realização da análise de variância, considerando-se os dados transformados, temos:
#'
#' Hipóteses:
#' $$\begin{array}{l}
#' H_0: \mu_A = \mu_B = \mu_C = \mu_D = \mu\\
#' H_a: \mbox{Pelo menos um constraste de médias difere de zero}
#' \end{array}$$

anova(modelot)

#' Como o valor-p = $4,623\times10^{-9} < 0,05 = \alpha$, considerando o nível de 5\% de significância, rejeitamos $H_0$. Desse modo, há evidências para afirmarmos que pelo menos um contraste de médias difere de zero.

#'
#' ## Teste de Tukey
#'
#' Hipóteses:
#'   $$\begin{array}{l}
#' H_0: \mu_i = \mu_{i'}, \text{ para } i \neq i'\\
#'   H_a: \mu_i \neq \mu_{i'}, \text{ para } i \neq i'\\
#'     \end{array}$$


library(ExpDes.pt)
with(dados,
     dic(trat,
         yt))

#' A partir do teste de Tukey, ao nível de 5\% de significância, há evidências para afirmarmos que as médias para as variedades C e D diferem das médias paras as variedades A e B, e as demais médias não diferem entre si.
#'
#' ## Médias obtidas pela transformação inversa 
#'    
(medias.TI <- round((tapply(dados$yt, dados$trat, mean))^2-0.5,4) )
#'
#' ## Apresentação das médias
#' 
mediasTI <- data.frame(trat = LETTERS[1:4],
                       media = c(medias.TI),
                       tukey = c("b", "b", "a", "a")) 
# atenção a ordem dos tratamentos na saída da ExpDes.pt
mediasTI <- mediasTI[order(mediasTI$trat),]
mediasTI

ggplot(mediasTI,
       aes(x = trat,
           y = media,
           label = tukey)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = trat,
                y = media + 0.5)) +
  theme_bw() +
  xlab("Variedade") +
  ylab("número de estacas enraizadas 
       (médias obtidas pela transformação inversa)")
