library("gamlss")

##################### Calibrando modelo

data("film90")

m2<-gamlss(lborev1~pb(lboopen) +pb(lnosc)+dist,
           sigma.fo=~pb(lboopen), nu.fo=~pb(lboopen),
           tau.fo=~pb(lboopen), data=film90, family=BCPE)

# a

dtop(m2,xvar=film90$lboopen,n.inter = 9)

# b

# Vamos supor que esse é o melhor modelo ajustado.
# Queremos melhorar esse modelo para uma observacao
# particular. Vamos pegar a ultima observacao no
# conjunto de dados.

# c

# Ultima observacao
tail(film90$lboopen,1)

# O intervalo de 5 a 17 é um range adequado para Y
# na ultima observacao. (Por que isso?)
y <- seq(from=5, to=17, length.out=100)

# Valores ajustados na ultima observacao
mu<-tail(fitted(m2,parameter='mu'),1)
sigma<-tail(fitted(m2,parameter='sigma'),1)
nu<-tail(fitted(m2,parameter='nu'),1)
tau<-tail(fitted(m2,parameter='tau'),1)

# Calcula valores r correspondentes a valores y.
residuals.quantile<-pBCPE(y,mu=mu,sigma=sigma,nu=nu,tau=tau)

residuals.normalized<-qNO(residuals.quantile)

# d

# Gerando a fdp na ultima observacao, avaliada na
# sequencia y. Essa e a fdp do modelo.
density.y<-dBCPE(y,mu=mu,sigma=sigma,nu=nu,tau=tau)

# e

# Vamos ajustar agora um modelo para os resíduos,
# usando a distribuicao SHASHo.

residuals.m2<-resid(m2)
model.residuals.m2<-gamlssML(residuals.m2,family=SHASHo)

# Conseguindo os parametros ajustados
mu0<-tail(fitted(model.residuals.m2,parameter='mu'),1)
sigma0<-tail(fitted(model.residuals.m2,parameter='sigma'),1)
nu0<-tail(fitted(model.residuals.m2,parameter='nu'),1)
tau0<-tail(fitted(model.residuals.m2,parameter='tau'),1)

# Conseguindo a fdp de R avaliado no ultimo residuo
residual.density<-dSHASHo(residuals.normalized,mu=mu0,
                          sigma=sigma0,nu=nu0,tau=tau0)

# g

# Vamos calibrar a fdp original.
calibrated.density<-(residual.density/dNO(residuals.normalized,
                                         mu=0,sigma=1))*density.y

# Comparando as fdps do modelo original e calibrada:
plot(density.y~y,type='l')
lines(y=calibrated.density,x=y,col='red')

# i

wp(m2,xvar=~lboopen+lnosc,n.inter = 2,ylim.worm=2)
