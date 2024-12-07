library(gamlss);

#################### Exercicios do capitulo 2

# a
data(abdom);
summary(abdom);
plot(y ~ x, data = abdom);

# b
# Encaixando o modelo normal com função de suavidade pb()
mNO = gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = NO);

# c

# Encaixando outras distribuicoes:
mGA = gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = GA);
mIG = gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = IG);
mGU = gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = GU);
mRG = gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = RG);
mLO = gamlss(y ~ pb(x), sigma.formula = ~pb(x), data = abdom, family = LO);

mPE = gamlss(y ~ pb(x), sigma.formula = ~pb(x), nu.formula = ~pb(x),
             data = abdom, family = PE);
mTF = gamlss(y ~ pb(x), sigma.formula = ~pb(x), nu.formula = ~pb(x),
             data = abdom, family = TF);
mBCCG = gamlss(y ~ pb(x), sigma.formula = ~pb(x), nu.formula = ~pb(x),
             data = abdom, family = BCCG);

mBCT = gamlss(y ~ pb(x), sigma.formula = ~pb(x), nu.formula = ~pb(x),
             tau.formula = ~pb(x), data = abdom, family = BCT);
mBCPE = gamlss(y ~ pb(x), sigma.formula = ~pb(x), nu.formula = ~pb(x),
             tau.formula = ~pb(x), data = abdom, family = BCPE);

# d

# Usando o critério de Akaike generalizado:
GAIC(mNO,mGA,mIG,mGU,mRG,mLO,mPE,mTF,mBCCG,mBCT,mBCPE,k=2);
GAIC(mNO,mGA,mIG,mGU,mRG,mLO,mPE,mTF,mBCCG,mBCT,mBCPE,k=3);
GAIC(mNO,mGA,mIG,mGU,mRG,mLO,mPE,mTF,mBCCG,mBCT,mBCPE,k=log(length(abdom$y)));

# e

# Selecionamos, pelo critério, o modelo da família LO.
# Vamos ver seus resíduos pelas funções plot() e wp()
plot(mLO);
wp(mLO);

# f

# Vendo os graus de liberdade totais efetivos do modelo:
edfAll(mLO);

# Plotando parametros ajustados
fittedPlot(mLO, x = abdom$x);

# Plotando os dados
plot(y ~ x, data = abdom);

# Plotando mu (ajustado) por x
lines(fitted(mLO) ~ x, data = abdom, col = "red");

# Plotando as curvas de centis
centiles(mLO, abdom$x)