library("gamlss")
library("gamlss.dist")
# library("gamlss.util")
library("colorspace")

################# Exercicio 3

# a

data("fabric")
summary(fabric)
plot(fabric)

# b

mPO1 <- gamlss(y ~ x, data = fabric, family = PO)
mPO2 <- gamlss(y ~ pb(x), data = fabric, family = PO)

# c

GAIC(mPO1, mPO2, k = 2)

# d checando residuos

residuals <- resid(mPO2)
plot(fitted(mPO2), residuals)
abline(0,0)
qqnorm(residuals)
qqline(residuals)
plot(density(residuals))
plot(mPO2)
wp(mPO2)

# e ajstando outros modelos

mNBI <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), 
               data = fabric, family = NBI)
mNBII <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), 
                data = fabric, family = NBII)
mPIG <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), 
               data = fabric, family = PIG)
mZIP <- gamlss(y ~ pb(x), nu.formula = ~pb(x), 
               data = fabric, family = ZIP)
mZAP <- gamlss(y ~ pb(x), nu.formula = ~pb(x), 
               data = fabric, family = ZAP)

mSICHEL <- gamlss(y ~ pb(x), sigma.formula = ~pb(x),
                  tau.formula = ~pb(x), data = fabric,
                  family = SICHEL)
mZINBI <- gamlss(y ~ pb(x), sigma.formula = ~pb(x),
                  tau.formula = ~pb(x), data = fabric,
                  family = ZINBI)
mZANBI <- gamlss(y ~ pb(x), sigma.formula = ~pb(x),
                  tau.formula = ~pb(x), data = fabric,
                  family = ZANBI)
               
# f comparacao de GAICs

GAIC(mPO1, mPO2, mNBI, mNBII, mPIG, mZIP, mZAP,
     mSICHEL, mZINBI, mZANBI, k = 2)
GAIC(mPO1, mPO2, mNBI, mNBII, mPIG, mZIP, mZAP,
     mSICHEL, mZINBI, mZANBI, k = 3)
GAIC(mPO1, mPO2, mNBI, mNBII, mPIG, mZIP, mZAP,
     mSICHEL, mZINBI, mZANBI, k = log(length(fabric$y)))

# g edf total, plot de parametros ajustados e de mu por x

edfAll(mPIG)
fittedPlot(mPIG, x = fabric$x, line.type = TRUE)
plot(y ~ x, col = "lightblue", data = fabric)
lines(fitted(mPIG)[order(fabric$x)] ~ fabric$x[order(fabric$x)])

# h plots de diagnostico

plot(mPIG)
wp(mPIG)
