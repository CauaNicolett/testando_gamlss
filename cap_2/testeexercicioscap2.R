library(gamlss);

#################### Exercicios 2

# a
data(abdom);
summary(abdom);
plot(abdom);

# b
# Encaixando o modelo normal com função de suavidade pb()
mNO = gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = NO);

# Encaixando outras distribuicoes:

families_2p = c(GA, IG, GU, RG, LO);
families_3p = c(PE, TF, BCCG);
families_4p = c(BCT, BCPE);
models_2p = list();
models_3p = list();
models_4p = list();

l = 1;
for (famly in families_2p) {
  models_2p[[l]] <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom,
                            family = famly);
  l <- l + 1;
}
l = 1;
for (famly in families_3p) {
  models_3p[[l]] <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom,
                        nu.formula = ~pb(x), family = famly);
  l <- l + 1;
}
l = 1;
for (famly in families_4p) {
  models_4p[[l]] <- gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom,
                        nu.formula = ~pb(x), tau.formula = ~pb(x), 
                        family = famly);
  l <- l + 1;
}

names(models_2p) <- c("GA", "IG", "GU", "RG", "LO");
names(models_3p) <- c("PE", "TF", "BCCG");
names(models_4p) <- c("BCT", "BCPE");

# c

