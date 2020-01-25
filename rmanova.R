library("pwr")
## input for interaction##
etap<-.1 #eta
k1<-3 # levels of the first factor
k2<-2 # levels of the second factor
k3<-2 # levels of the third factor
k4<-2 # levels of the fourth factor
k5<-2 # levels of the fifth factor
k6<-2 # levels of the sixth factor
## end of input ##
# main interaction
#f2= (1 - (1 - etap) * (3000 - 96 + 5)/(3000 - 96))*rho
rho=.5 #correlation of responses
k=5 # number of measurments
c = sqrt(k/(1+(k-1)*rho)) # design effect
(f2=etap/(1-etap)*c) #effect size
dfn_main<-(k1-1)*(k2-1)*(k3-1)*(k4-1)*(k5-1)*(k6-1)  # df for the interaction
(res_main<-pwr.f2.test(u = dfn_main,f2 = f2,sig.level = .05,power = .80))
## required N ##
int_main <- ceiling(res_main$v)+(k1*k2*k3*k4*k5*k6)-1

dfn1<-(k1-1) # df for the main effect 1
(res1<-pwr.f2.test(u = dfn1 ,f2 = f2,sig.level = .05,power = .80))
## required N ##
int1 <- ceiling(res1$v)+(k1*k2*k3*k4*k5*k6)-1

dfn2<-(k2-1) # df for the main effect 2
(res2 <-pwr.f2.test(u = dfn2 ,f2 = f2,sig.level = .05,power = .80))
## required N ##
int2 <- ceiling(res2$v)+(k1*k2*k3*k4*k5*k6)-1

dfn3<-(k3-1) # df for the main effect 3
(res3 <-pwr.f2.test(u = dfn3 ,f2 = f2,sig.level = .05,power = .80))
## required N ##
int3 <- ceiling(res3$v)+(k1*k2*k3*k4*k5*k6)-1

dfn4<-(k4-1) # df for the main effect 4
(res4 <-pwr.f2.test(u = dfn4 ,f2 = f2,sig.level = .05,power = .80))
## required N ##
int4 <- ceiling(res4$v)+(k1*k2*k3*k4*k5*k6)-1

dfn5<-(k5-1) # df for the main effect 5
(res5 <-pwr.f2.test(u = dfn5 ,f2 = f2,sig.level = .05,power = .80))
## required N ##
int5 <- ceiling(res5$v)+(k1*k2*k3*k4*k5*k6)-1

dfn6<-(k6-1) # df for the main effect 6
(res6 <-pwr.f2.test(u = dfn6 ,f2 = f2,sig.level = .05,power = .80))
## required N ##
int6 <- ceiling(res6$v)+(k1*k2*k3*k4*k5*k6)-1

n <- int_main+int1+int2+int3+int4+int5+int6 #n with main interaction
n
