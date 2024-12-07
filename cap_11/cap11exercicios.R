library("gamlss")
library("gamlss.data")
library("MASS")

# a

# Carregando e plotando dados
data("LGAclaims")
dados <- LGAclaims[-c(1:2)] # Removendo dados sem interpretação
plot(dados)
plot(dados[c(1:5)])
plot(dados[-c(2:5)])
n <- length(dados$Claims)

# b

# Vamos modelar Claims usando distribuicoes de Poisson e Binomial
# Negativa.

mPO <- gamlss(Claims ~ Pop_density+KI+Accidents+Population+
                L_Population+L_Accidents+L_KI+L_Popdensity,
              data = dados, family = PO)
mNBI <- gamlss(Claims ~ Pop_density+KI+Accidents+Population+
                L_Population+L_Accidents+L_KI+L_Popdensity,
              data = dados, family = NBI)

for (kval in c(2, 2.5, 3, 3.5, 4, log(n))){
  message("k = ", kval)
  print(GAIC(mPO, mNBI, k = kval))
}

# c

dropterm(mNBI, test = "Chisq")
mNBI2 <- gamlss(Claims ~ Pop_density+Accidents+Population+
                 L_Population+L_Accidents+L_KI+L_Popdensity,
               data = dados, family = NBI)
dropterm(mNBI2, test = "Chisq")
mNBI3 <- gamlss(Claims ~ Accidents+Population+
                  L_Population+L_Accidents+L_KI+L_Popdensity,
                data = dados, family = NBI)
dropterm(mNBI3, test = "Chisq")
mNBI4 <- gamlss(Claims ~ Population+
                  L_Population+L_Accidents+L_KI+L_Popdensity,
                data = dados, family = NBI)
dropterm(mNBI4, test = "Chisq")
mNBI5 <- gamlss(
  Claims ~ L_Population+L_Accidents+L_KI+L_Popdensity,
  data = dados,
  sigma.formula = ~1,
  nu.formula = ~1,
  tau.formula = ~1,
  family = NBI)
dropterm(mNBI5, test = "Chisq")

# d

addterm(mNBI5, scope = ~(Pop_density+KI+Accidents+Population+
          L_Population+L_Accidents+L_KI+L_Popdensity)^2,
        test = "Chisq")

mNBI6 <- stepGAIC(mNBI5, scope = ~(Pop_density+KI+Accidents+
          Population+L_Population+L_Accidents+L_KI+
            L_Popdensity)^2, direction = "both",
          k = log(n))

for (kval in c(2, 2.5, 3, 3.5, 4, log(n))){
  message("k = ", kval)
  print(GAIC(mPO, mNBI, mNBI2, mNBI3, mNBI4, mNBI5, mNBI6, k = kval))
}

plot(mNBI)
plot(mNBI6)

# f

scope_parameter <- list(
  lower = ~1,
  upper = ~(Pop_density+KI+Accidents+
      Population+L_Population+L_Accidents+L_KI+L_Popdensity)^2
  )

addterm(mNBI6, scope = scope_parameter$upper, what = "sigma", test = "Chisq")


# options(error = recover)
# options(error = NULL)
# debug(stepGAIC)
# undebug(stepGAIC)
mNBI7 <- stepGAIC(mNBI6, scope = scope_parameter,
        what = "sigma", direction = "both", k = log(n), trace = TRUE)

addterm(mNBI7, scope = scope_parameter$upper, what = "sigma", test = "Chisq")

# g

# Dado sigma, podemos simplificar o modelo para mu??
dropterm(mNBI7, test = "Chisq")

# h

par(mfrow = c(2,4))
term.plot(mNBI7, data = dados, what = "mu")
plot(Claims ~ L_Population, data = dados)
plot(Claims ~ L_Accidents, data = dados)
plot(Claims ~ L_KI, data = dados)
plot(Claims ~ L_Popdensity, data = dados)

par(mfrow = c(1,2))
term.plot(mNBI7, data = dados, what = "sigma")
plot(Claims ~ L_Population, data = dados)

dev.off()

# i

plot(mNBI6)
plot(mNBI7)

coefwp1 <- wp(mNBI7, xvar = ~L_Population+L_Accidents, n.inter = 2)
coefwp2 <- wp(mNBI7, xvar = ~L_KI+L_Popdensity, n.inter = 2)
wp(mNBI7, xvar = ~L_Population, n.inter = 2)
wp(mNBI7, xvar = ~L_Accidents, n.inter = 2)
wp(mNBI7, xvar = ~L_KI, n.inter = 2)
wp(mNBI7, xvar = ~L_Popdensity, n.inter = 2)

