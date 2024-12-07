help(package = "gamlss")
?gamlss

library("gamlss")
library("gamlss.add")
library("corrplot") # para representação visual de coeficientes de correlacao

data(film90)

dados <- film90
str(dados) # mostra a estrutura do dataframe dados.
summary(dados) # mostra um sumário dos dados.

plot(lborev1 ~ lboopen, data = dados, col = "lightblue",
     xlab = "log opening revenue", ylab = "log extra revenue")

# O scatterplot já sugere que o modelo linear não se encaixa bem,
# especialmente para valores baixos de lboopen.

# Modelo linear
m <- gamlss(lborev1 ~ lboopen, data = dados, family = NO)

# Adiciona reta de regressão considerando uma variável resposta e uma
# variável explanatória.
lines(fitted(m) ~ dados$lboopen)

# Podemos usar funções de alisamento.
# Exemplos: I() para curvas polinomiais.
# poly() para polinômios ortogonais.

 m00 <- gamlss(lborev1 ~ lboopen + I(lboopen^2) + I(lboopen^3), data = dados,
#              family = NO)

# Um jeito melhor, para que os "evaluations" só ocorram uma vez, é:

dados <- transform(dados, lb2 = lboopen^2, lb3 = lboopen^3)
m002 <- gamlss(lborev1 ~ lboopen + lb2 + lb3, data = dados, family = NO)

plot(lborev1~lboopen, col="lightblue", data=dados)
lines(fitted(m002)[order(dados$lboopen)] ~ dados$lboopen[order(dados$lboopen)])

# obtendo matrizes de variancia-covariancia, a matriz de correlacao e
# os erros padrões, usuais e robustos, dos parâmetros estimados.

# matriz de variancia-covariancia dos parametros
print(vcov(m00), digit = 3)

# matriz de correlacao
print(vcov(m00, type = "cor"), digit = 3)
# Note que o intercepto na coluna final é o do sigma, enquanto o do
# começo é do mu.

# Vamos usar polinomios ortogonais para encaixar no mesmo modelo que m00
# veja a secao 8.3.

m0 <- gamlss(lborev1 ~ poly(lboopen, 3), data = dados, family = NO)

# Usamos a biblioteca corrplot para plotar coeficientes de correlacao
col1 <- colorRampPalette(c("black", "grey"))
corrplot(vcov(m00, type="cor"), col=col1(2), outline=TRUE,
         tl.col = "black", addCoef.col = "white")
corrplot(vcov(m0, type="cor"), col=col1(2), outline=TRUE,
         tl.col = "black", addCoef.col = "white")

# Note que os parâmetros de mu para m0 não têm correlacao, pois usamos
# polinomios ortogonais. Em m00 sao altamente correlacionados

############# 2.3.2 Ajustando um modelo nao parametrico #############

### P-splines (splines penalizados) pb()
m1<-gamlss(lborev1~pb(lboopen), data=film90, family=NO)
summary(m1)

plot(lborev1~lboopen, col="lightblue", data=film90)
lines(fitted(m1)[order(film90$lboopen)]~
        film90$lboopen[order(film90$lboopen)])

# Vendo os graus efetivos de liberdade para mu
edf(m1, "mu")

### Splines cubicos
# Para detalhes, veja 9.4.6.

# Spline cubico com 10 edf em adicao a constante e aos termos lineares
# Veja que os graus de liberdade serao 10 + 2 + 1
m2<-gamlss(lborev1~cs(lboopen,df=10), data=film90, family=NO)

plot(lborev1~lboopen, col="lightblue", data=film90)
lines(fitted(m1)[order(film90$lboopen)]~
        film90$lboopen[order(film90$lboopen)])
lines(fitted(m2)[order(film90$lboopen)]~
        film90$lboopen[order(film90$lboopen)],
      col="red", lty=2, lwd=2)
legend("topleft",legend=c("m1: P-splines","m2: cubic splines"),
       lty=1:2,col=c("black","red"),cex=1)

### loess (locally weighted scatterplot smoothing)

m4 <- gamlss(lborev1~lo(~lboopen,span=.4), data=film90, family=NO)

### Redes neurais
# VEja 9.6.1 para detalhes.
# Para isso precisamos usar o pacote nnet. Use library(gamlss.add)

mnt <- gamlss(lborev1~nn(~lboopen,size=20,decay=0.1), data=film90,
              family=NO)

# Mostrando modelos mnt e m1
plot(lborev1~lboopen, col="lightblue", data=film90)
lines(fitted(m1)[order(film90$lboopen)]~
        film90$lboopen[order(film90$lboopen)])
lines(fitted(mnt)[order(film90$lboopen)]~
        film90$lboopen[order(film90$lboopen)],
      col="red", lty=2, lwd=2)
legend("topleft",legend=c("m1: P-splines","mnt: neural network"),
       lty=1:2,col=c("black","red"),cex=1)

# Informacoes do modelo ajustado com redes neurais:
coef(getSmo(mnt))

##########################################################
##########################################################

# Extraindo valores ajustados
# Para todos os casos no GAMLSS, valores ajustados podem ser obtidos por
# fitted()
plot(lboopen, fitted(m1,"mu"))
