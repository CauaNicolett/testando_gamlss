############# Diagnosticos

library("gamlss")
library("nortest")
library("MASS")

data("film90")

# a

m1 <- gamlss(lborev1~pb(lboopen)+pb(lnosc)+dist, data=film90,
             family=NO)

# b

# A cauda da distribuição da variável é mais pesada.
# Também se percebe certa assimetria.
# coef. of skewness  =  0.593627; coef. of kurtosis  =  5.860243 
plot(m1, xvar = film90$lboopen)

# c

# O primeiro e o último apresentam problemas relacionados à curtose.
# O terceiro indica uma assimetria.
wp(m1,xvar=~lboopen+lnosc,n.inter = 2,ylim.worm=4)

# d

m2 <- gamlss(lborev1~pb(lboopen) +pb(lnosc)+dist,
             sigma.fo=~pb(lboopen), nu.fo=~pb(lboopen),
             tau.fo=~pb(lboopen), data=film90, family=BCPE)

# e

# Percebemos que a assimetria dos resíduos é positiva no canto
# esquerdo superior e negativa no canto direito inferior.
plot(m2,xvar=film90$lboopen)
wp(m2,xvar =~lboopen+lnosc,n.inter = 2,ylim.worm=2)

# Então precisamos de um novo modelo para o parâmetro nu.

# f

m3<-gamlss(lborev1~pb(lboopen)+pb(lnosc)+dist,
           sigma.fo=~pb(lboopen),nu.fo=~pb(lboopen)+pb(lnosc),
           tau.fo=~pb(lboopen),data=film90,family=BCPE,
           start.from=m2)

# g

# Está melhor, a assimetria foi corrigida em parte.
# O segundo gráfico é o mais problemático. Ele indica assimetria
# negativa dos resíduos e desajuste na média.
# O terceiro gráfico apresenta uma leve curva, indicando que as caudas
# do modelo ajustado deveriam ser mais leves.
plot(m3, xvar = film90$lboopen)
wp(m3, xvar = ~lboopen+lnosc, n.inter = 2, ylim.worm = 2)

# h

m4<-gamlss(lborev1~pvc(lboopen,by=dist)+pvc(lnosc,by=dist)+
            dist, sigma.fo=~pb(lboopen),nu.fo=~pb(lboopen)+
            pb(lnosc),tau.fo=~pb(lboopen)+pb(lnosc),
          data=film90, family=BCPE, start.from=m3)

# In model.matrix.default(~fac - 1, contrast = "") :
# non-list contrasts argument ignored

# i

# Teste de normaldade para os resíduos de m4.
shapiro.test(resid(m4)) #Shapiro-Wilk normality test
ad.test(resid(m4)) #Anderson-Darling normality test
cvm.test(resid(m4)) #Cramer-von Mises normality test

# j

# Comparação entre resíduos de m1 e m4
# Desajustes ainda podem ocorrer pela aleatoriedade dos dados, mesmo
# se o modelo estiver correto.
wp(m1, xvar = ~lboopen+lnosc, n.inter = 2, ylim.worm = 4)
wp(m4, xvar = ~lboopen+lnosc, n.inter = 2, ylim.worm = 2)
plot(m1, xvar=film90$lboopen)
plot(m4, xvar=film90$lboopen)

# Comparando com m3:
wp(m3, xvar = ~lboopen+lnosc, n.inter = 2, ylim.worm = 4)
plot(m3, xvar=film90$lboopen)

# k

# Vamos usar a distribuição BCPE para os dados film30
data("film30")

str(film30)  # Mostra a estrutura do conjunto de dados
sapply(film30, class)  # Mostra a classe de cada coluna

pairs(film30[, -1]) # Plota gráficos de dispersão

sum(is.na(film30$total)) # Verificando se há espaços NA.
sum(is.na(film30$opening))

# Ajustando um modelo simples
mm1 <- gamlss(total ~ opening, data = film30, family = BCPE)

# Algoritmo RS não convergiu ainda
remove(mm1)

# Vamos criar um modelo auxiliar para usar suas estimativas
# a fim de ajudar na convergência do método
aux <- gamlss(total ~ opening, data = film30, family = NO) 
mm1 <- gamlss(total ~ opening, data = film30, family = BCPE, start.from = aux)

coefmm1 <- wp(mm1,xvar=film30$opening,n.inter=2, ylim.worm = 2.3)
plot(mm1, xvar = film30$opening)

# Temos problemas de assimetria positiva e de curtose
mm2 <- gamlss(total ~ opening, nu.formula = ~pb(opening), data = film30,
              family = BCPE, start.from = mm1)

coefmm2 <- wp(mm2,xvar=film30$opening,n.inter=2, ylim.worm = 2.3)
plot(mm2, xvar = film30$opening)

# O modelo mm2 é ainda pior que mm1!

mm3 <- gamlss(total ~ pb(opening), nu.formula = ~pb(opening), data = film30,
              family = BCPE, start.from = mm1)

plot(mm3, xvar = film30$opening)

mm4 <- gamlss(total ~ pb(opening), data = film30,
              family = BCPE, start.from = mm1)

plot(mm4, xvar = film30$opening)

mm5 <- gamlss(total ~ pb(opening), nu.formula = ~opening+I(opening^2),
              data = film30, family = BCPE, start.from = mm1)

plot(mm5, xvar = film30$opening)

# Por enquanto mm4 é o melhor modelo
GAIC(mm1, mm2, mm3, mm4, mm5, k = 2) # AIC
GAIC(mm1, mm2, mm3, mm4, mm5, k = 0) # GDEV

# Perceba que nenhuma variável explanatória modelando nu é significativa.
drop1(mm5, parameter = "nu")

# Vamos criar um modelo apenas com o intercepto, usando familia BCPE.
dados <- film30[, -1]
mod <- gamlss(total ~ 1, data = dados, family = BCPE)

# Agora usamos a estrategia A do stepGAIC.
mod2 <- stepGAICAll.A(mod, scope = list(lower= ~1, upper= ~opening), k = 2)

# Vamos fazer algo similar, porém com uma função de suavidade.
# Estou tendo problemas com a convergência do método.
# ESTA LINHA DE CÓDIGO DEMORA MUITO PARA RODAR!!
mod3 <- stepGAICAll.A(mod, scope = list(lower= ~1,
           upper= ~opening + pb(opening)), k = 2)
# summary(mod3)
# ******************************************************************
# Family:  c("BCPE", "Box-Cox Power Exponential") 
# 
# Call:  gamlss(formula = total ~ opening, sigma.formula = ~1, nu.formula = ~pb(opening),  
#               tau.formula = ~pb(opening), family = BCPE, data = dados,      trace = FALSE) 
# 
# Fitting method: RS() 
# 
# ------------------------------------------------------------------
#   Mu link function:  identity
# Mu Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.859e+04  1.091e+03   26.19   <2e-16 ***
#   opening     2.994e+00  3.805e-02   78.69   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
#   Sigma link function:  log
# Sigma Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.1165989  0.0003975  -293.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
#   Nu link function:  identity 
# Nu Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.706e+00  1.292e-02  -132.1   <2e-16 ***
#   pb(opening) -7.965e-06  1.877e-08  -424.4   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
#   Tau link function:  log 
# Tau Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  9.323e-01  9.102e-02  10.243   <2e-16 ***
#   pb(opening) -2.594e-07  1.432e-06  -0.181    0.856    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# ------------------------------------------------------------------
#   NOTE: Additive smoothing terms exist in the formulas: 
#   i) Std. Error for smoothers are for the linear effect only. 
# ii) Std. Error for the linear terms may not be reliable. 
# ------------------------------------------------------------------
#   No. of observations in the fit:  969 
# Degrees of Freedom for the fit:  19.78353
# Residual Deg. of Freedom:  949.2165 
# at cycle:  20 
# 
# Global Deviance:     24098.14 
# AIC:     24137.71 
# SBC:     24234.18 
# ******************************************************************
#   Warning message:
#   In summary.gamlss(mod3) :
#   summary: vcov has failed, option qr is used instead

GAIC(mm1, mm2, mm3, mm4, mm5, mod2, mod3, k = 2) # AIC
GAIC(mm1, mm2, mm3, mm4, mm5, mod2, mod3, k = 0) # GDEV
plot(mod3, xvar = dados$opening)
wp(mod3, xvar = dados$opening, n.inter = 4, ylim.worm = 2)

# O modelo nao esta bem ajustado

# Se modelarmos com as mesmas variaveis explanatorias usadas,
# existira diferencas entre os modelos?

mod4 <- gamlss(total ~ opening, nu.formula = ~pb(opening),
               tau.formula = ~pb(opening), family = BCPE, data = dados,
               start.from = aux) 

GAIC(mm1, mm2, mm3, mm4, mm5, mod2, mod3, mod4, k = 2) # AIC
# df      AIC
# mod3 19.783530 24137.71
# mod2  6.000000 24280.83
# mod4 27.838243 24333.71
# mm4   8.886718 24341.39
# mm5  10.875426 24345.37
# mm3  25.874306 24375.36
# mm1   5.000000 24387.72
# mm2  21.999761 24421.72
GAIC(mm1, mm2, mm3, mm4, mm5, mod2, mod3, mod4, k = 0) # GDEV
# df      AIC
# mod3 19.783530 24098.14
# mod2  6.000000 24268.83
# mod4 27.838243 24278.04
# mm3  25.874306 24323.61
# mm5  10.875426 24323.61
# mm4   8.886718 24323.62
# mm2  21.999761 24377.72
# mm1   5.000000 24377.72

# O modelo 3 parece melhor.

