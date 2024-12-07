library("gamlss")

# Carregando os dados de qualidade de ar do exercício 2.

dados <- airquality

# Plotando os dados.
plot(dados[,-c(5,6)]) 

# Comentário: parece que as concentrações de ozônio
# aumentam com a radiação solar (com alto crescimento de
# variabilidade) e com o aumento da temperatura (com maior
# crescimento da média a partir de 80 F). As concentrações
# decrescem com o decréscimo da velocidade do vento, com
# assimetria dos dados ao redor da média.

# (b) Vamos aplicar um modelo de regressão linear.
# Vamos denotar esse modelo por air.lm

air.lm <- lm(Ozone ~ Temp + Wind + Solar.R, data = dados)

# Sumário para ver os coeficientes e erros padrões.
summary(air.lm)

# Plotando os termos ajustados, usando termplot()
op <- par(mfrow = c(1,3))
termplot(air.lm, partial.resid=TRUE, se=T)
par(op)
# Comentários: os dados não parecem normalmente distribuídos
# ao redor da média. Os gráficos tendem a apresentar valores
# mais aglomerados abaixo da reta, e mais dispersos acima da
# reta.

# (c) Checando os resíduos usando plot():
op <- par(mfrow = c(1,2))
plot(air.lm, which = 1:2)
par(op)

# (d) Vamos agora usar a função gamlss()

# A FUNÇÃO gamlss() NÃO FUNCIONA COM VALORES NA.
da <- na.omit(dados) # Limpa os NA's.
mno <- gamlss(Ozone ~ Temp + Wind + Solar.R, data = da)

# Summary
summary(mno)

# Plotando usando term.plot(), correspondente para gamlss
term.plot(mno, pages = 1, partial = TRUE)

# (e) Usando plot() e wp()
 plot(mno)
# wp(mno)

# Aumente o limite de pontos no eixo vertical
wp(mno, ylim.all = 2)

# Comentário: estimei 12 pontos fora da zona de confiança.
# Isso sugere que o modelo não é bem ajustado.
# Os pontos fazem uma forma de U no gráfico, cruzando o zero.

# (f) Usando distribuições gamma, gaussiana inversa e Box-Cox
# Cole and Green usando critério AIC.

mga <- gamlss(Ozone~Temp+Wind+Solar.R, data=da, family=GA)
mig <- gamlss(Ozone~Temp+Wind+Solar.R, data=da, family=IG)
mbccg <- gamlss(Ozone~Temp+Wind+Solar.R, data=da, family=BCCGo)
GAIC(mno, mga, mig, mbccg, k = 2)

# Pelo critério de informação de Akaike, a dist. gamma ganha.

# (g) Vamos agora trabalhar com gamma, usando funções de alisamento
# pb() para todas as variáveis explanatórias, em um modelo que vamos
# nomear de mga1.

mga1=gamlss(Ozone~pb(Temp)+pb(Wind)+pb(Solar.R),data=da, 
            family=GA)
term.plot(mga1, pages=1)
plot(mga1)
wp(mga1)

# O worm plot parece bom. Não há pontos fora da zona de confiança.

# Comparamos o AIC
GAIC(mga, mga1, k = 2)

# Pelo AIC, o modelo mga1 é melhor.