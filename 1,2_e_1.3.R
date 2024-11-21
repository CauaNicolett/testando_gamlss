# Carrega a bibliotexa gamlss, para o objeto "rent" ser bem definido.
library(gamlss)

# Plots das informações

# par faz com que as informações sejam dispostas em linhas e colunas, como se houvesse uma matriz.
# exemplo: par(mfrow = c(3, 1), mar = c(5, 10, 4, 1), cex.axis = 3, cex.lab = 3)
# aqui c(3,1) indica a forma como será apresentada, mar é a margem e o cex é o tamanho das letras.
PPP <- par(mfrow=c(2,2))

# R~Fl significa R em função de Fl. R é variável dependente e Fl é variável independente.
plot(R~Fl, data=rent, col=gray(0.7), pch=15, cex=0.5)
plot(R~A, data=rent, col=gray(0.7), pch=15, cex=0.5)
plot(R~H, data=rent, col=gray(0.7), pch=15, cex=0.5)
plot(R~loc, data=rent, col=gray(0.7), pch=15, cex=0.5)

# Restaura configurações originais.
par(PPP)

r1 <- gamlss(R ~ Fl+A+H+loc, family=NO, data=rent, trace=FALSE)
l1 <- lm(R ~ Fl+A+H+loc,data=rent)

# Mostra coeficientes estimados para cada variável explanatória.
coef(r1)
coef(l1)

# Mostra o primeiro elemento do vetor de variância estimado (EMV)
# no modelo GAMLSS.

fitted(r1, "sigma")[1]

# Mostra erros padrões e testes t dos coeficientes estimados.
# O método para calucular erros padrões é explicado na seção 5.2.

summary(r1)

# Veja que log foi usado para o desvio. Veja a seção 1.4.
# Dica: CTRL + L para limpar tela.

# Obtendo R^2

Rsq(r1)

# Checando os resíduos para ver se o ajuste está adequado
# Mais da interpretação está na seção 12.3.

plot(r1)

