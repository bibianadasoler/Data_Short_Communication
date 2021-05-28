library(carcass)


#### estimativa usando persistencia malha norte até jan2017 - mamiferos medios, grandes, nativos
remoc.mam.nat.medgran<-read.csv("/Users/bibianaterrad/OneDrive/Mestrado/RUMO/Análises Rumo/mamiferos_med_gra_MN17.csv", sep = ";")
PERSI<-persistence.prob(remoc.mam.nat.medgran$grupo, remoc.mam.nat.medgran$perstime, 
                        remoc.mam.nat.medgran$status, pers.const=TRUE)
s.rumo<-PERSI$persistence.prob
s.rumo.lwr<-PERSI$lower
s.rumo.upr<-PERSI$upper

mam.nat.medgra.MN<- read.csv("1203_mam_nat_med-gra.csv", sep = ";")
EFFIC.mam.nat.medgra.MN<-search.efficiency(mam.nat.medgra.MN)
EFFIC.mam.nat.medgra.MN
f.mami_C = EFFIC.mam.nat.medgra.MN$f.perperson$f
f.mami.lwr_C = EFFIC.mam.nat.medgra.MN$f.perperson$lwr
f.mami.upr_C = EFFIC.mam.nat.medgra.MN$f.perperson$upr

pEq14.rumo<-ettersonEq14(s=s.rumo, f=f.mami_C, J=IA)
CIpEq14.rumo<-CIetterson(s=s.rumo, s.lwr=s.rumo.lwr, s.upr=s.rumo.upr, f=f.mami_C, f.lwr=f.mami.lwr_C, f.upr=f.mami.upr_C, J=IA)

mortalidade.mami.RUMO<-estimateN(count=1950, p=pEq14.rumo, p.lower = CIpEq14.rumo$p.lower, 
                                 p.upper = CIpEq14.rumo$p.upper, pform = "etterson", J=IA, 
                                 maxn=30000, nsim = 10000, plot=T, postdist = T, arrival = "uniform")
plot(0:30000, mortalidade.mami.RUMO$postdist)
str(mortalidade.mami.RUMO)


####    TUDO ERRADO ABAIXO 
####### --- Correção 26marco2020
mam.nat.medgra.MN<- read.csv("1203_mam_nat_med-gra.csv", sep = ";")
EFFIC.mam.nat.medgra.MN<-search.efficiency(mam.nat.medgra.MN)
EFFIC.mam.nat.medgra.MN
f.mami_C = EFFIC.mam.nat.medgra.MN$f.perperson$f
f.mami.lwr_C = EFFIC.mam.nat.medgra.MN$f.perperson$lwr
f.mami.upr_C = EFFIC.mam.nat.medgra.MN$f.perperson$upr

remoc.mam.nat.medgran.MN<-read.csv("remocao_1203_mam_nat.csv", sep = ";")
PERSI.mam.nat<- persistence.prob(remoc.mam.nat.medgran.MN$grupo, remoc.mam.nat.medgran.MN$perstime, 
                                 remoc.mam.nat.medgran.MN$status, pers.const=TRUE)
s.mami_C<-PERSI.mam.nat$persistence.prob
s.mami.lwr_C<- PERSI.mam.nat$lower
s.mami.upr_C<- PERSI.mam.nat$upper


pEq14.mami_C<-ettersonEq14(s=s.mami_C, f=f.mami_C, J=IA)
CIpEq14.mami_C<-CIetterson(s=s.mami_C, s.lwr=s.mami.lwr_C, s.upr=s.mami.upr_C, f=f.mami_C, f.lwr=f.mami.lwr_C, f.upr=f.mami.upr_C, J=IA)

mortalidade.mami.MN_C<-estimateN(count=1950, p=pEq14.mami_C, p.lower = CIpEq14.mami_C$p.lower, 
                                 p.upper = CIpEq14.mami_C$p.upper, pform = "etterson", J=IA, 
                                 maxn=30000, nsim = 10000, plot=T, postdist = T, arrival = "uniform")
plot(0:30000, mortalidade.mami.MN_C$postdist)
str(mortalidade.mami.MN_C)

####(effic e persi nao tem so nativos)
### --------REMOCAO
# CLASSES
data_1203_classes <- read.csv("remocao_1203_classes.csv", sep=";")
pers_1203_classes<- persistence.prob(data_1203_classes$grupo, data_1203_classes$perstime, 
                                     data_1203_classes$status, pers.const=TRUE)
pers_1203_classes
table(data_1203_classes$grupo)
with(data_1203_classes, table(grupo, status))

s.mami<-pers_1203_classes$persistence.prob[2]
s.mami.lwr<- pers_1203_classes$lower[2]
s.mami.upr<- pers_1203_classes$upper[2]


### --------DETECCAO
detec_1203_classes<- read.csv("1203_classes.csv", sep = ";")
efficiency_1203_classes<- search.efficiency(detec_1203_classes)
efficiency_1203_classes

f.mami = efficiency_1203_classes$f.perperson$f[2]
f.mami.lwr = efficiency_1203_classes$f.perperson$lwr[2] 
f.mami.upr = efficiency_1203_classes$f.perperson$upr[2]


# ----------- ESTIMATIVA MAGNITUDE
# intervalos de amostragens: DATAS INICIAIS DAS CAMPANHAS DE AUTO DE LINHA
# 2015 = 21 a 26 jan; 12 a 19 mar; 13 a 19 mai; 13 a 16 jul; 27 a 31 out; 30 a 04 nov-dez;
# 2016 = 29 a 04 mar-abr; 16 a 23 ago; 11 a 14 nov;
IA <-c(50, 62, 61, 106, 34, 120, 140, 87)

# Mamiferos
#detectabilidade
pEq14.mami<-ettersonEq14(s=s.mami, f=f.mami, J=IA)
CIpEq14.mami<-CIetterson(s=s.mami, s.lwr=s.mami.lwr, s.upr=s.mami.upr, f=f.mami, f.lwr=f.mami.lwr, f.upr=f.mami.upr, J=IA)


# Estimativa de fato
mortalidade.mami.MN<-estimateN(count=1950, p=pEq14.mami, p.lower = CIpEq14.mami$p.lower, 
                               p.upper = CIpEq14.mami$p.upper, pform = "etterson", J=IA, 
                               maxn=30000, nsim = 10000, plot=T, postdist = T, arrival = "uniform")
plot(0:30000, mortalidade.mami.MN$postdist)
str(mortalidade.mami.MN)
