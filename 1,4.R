library("gamlss")

# Usando distribuição Gamma em modelos GAMLSS e MLG.

r2 <- gamlss(R ~ Fl+A+H+loc, family=GA, data=rent)
coef(r2)
coef(r2, "sigma") # extrai log(sigma)
deviance(r2)

l2 <- glm(R ~ Fl+A+H+loc, family=Gamma(link="log"), data=rent)
coef(l2)
summary(l2)$dispersion # extrai phi
deviance(l2)

# Pegando os coeficientes dos erros padrões:

summary(r2)

# Modelo gaussiano inverso

r22 <- gamlss(R ~ Fl+A+H+loc, family=IG, data=rent, trace=FALSE)

# Modelo gaussiano

r1 <- gamlss(R ~ Fl+A+H+loc, family=NO, data=rent, trace=FALSE)

# Comparação de desvios globais
# Obs.: como todos têm o mesmo grau de liberdade, comparamos apenas os desvios
# globais.

GAIC(r1, r2, r22, k=0) # GD

# Conclusão: modelo gamma dá o melhor ajuste, na distribuição GAMLSS, entre
# essas três distribuições, neste caso.

plot(r2)

###################################################
# 1.5
###################################################

# Usando funções de alisamento para Fl e A
# pb() é implementação de suavização por splines, por Eilers e Marx, 1996.

r3 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc, family=GA, data=rent,
            trace=FALSE)
AIC(r2,r3)

# Segundo o AIC, o modelo r3 é melhor.

summary(r3)

# Devemos ter cuidado com os erros padrões.
# Veja a seção 5.2.3. para a estimativa dos erros padrões das porções lineares.
# Vamos usar drop1() para checar a contribuição do suavizador como um todo.

drop1(r3)

# Plotando os termos ajustados do modelo r3
# As áreas em cinza são bandas de confiança de 95% pontuais para as curvas
# de suavização e para os níveis dos fatores.

term.plot(r3, pages=1, ask=FALSE)

# Usamos um warm plot para checar a adequação do modelo MAG ajustado.
# [van Buuren and Fredriks, 2001]
# Cheque cap.12 para ver como interpretar um warm plot.

wp(r3, ylim.all=.6)
