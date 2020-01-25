

## One-Way ANOVA Priori Power Analysis of a Full Factorial Design with Repeated Measures in R

#### Adi Tantravahi

When planning the design of an experiment, a power analysis is one of the most fundamental tools to ensure replicability. Computing a power analysis when designing a study increases the probability of finding an effect that interests researchers and also increase the chances of obtaining accurate predictions. When researchers discover a low probability of power, it allows prudent researchers the opportunity to consider design changes. In this post, we will go over the concepts of the power analysis and the R code to carry out the analysis. 

Conducting a power analysis for a factorial design, of which a conjoint experiment is a variant, is done similarly to a one-way ANOVA with some slight modifications. Because of the nature of the larger design, we must specify the total number of groups in the design and degrees of freedom to calculate accurate power parameters. 

For the purposes of this post, we'll use a $2^5 3$ design -  meaning there are 5 factors with 2 levels and 1 factor with 3 levels. Lets go ahead and input the design into R.

```R
k1 <- 3 # levels of the first factor
k2 <- 2 # levels of the second factor
k3 <- 2 # levels of the third factor
k4 <- 2 # levels of the fourth factor
k5 <- 2 # levels of the fifth factor
k6 <- 2 # levels of the sixth factor
```

Now say we are searching for the required N to gain 80% power, we'll need to determine the following things:

* Degrees of freedom for the effect: $k-1=5$, where k is the number of factors

* The total number of groups (treatments): $2*2*2*2*2*3 = 96$

* The effect size: $\eta^2_p$

* The power of the interaction for the design, which is the numerator for the degrees of freedom: $(2-1)*(2-1)*(2-1)*(2-1)*(2-1)*(3-1) = 2$

  

With regard to effect size, $\eta^2_p$ represents the variance explained by the effect as a proportion of the variance not explained by other effects. So, $\sigma^2_f$ as population variance explained by the effect and $\sigma^2$ as population residual variance gives us


​    $$\eta^2_p = \frac{\sigma^2_f}{\sigma^2_f + \sigma^2}$$

Similar to a one-way design, the expected effect size is the population effect size, and, as such, we must consider the same empirical estimates. We adjust the sample eta-squared by calculating the partial epsilon-squared:

​    $$\epsilon^2_p = 1 - (1- \eta^2_p) * \frac{N - K + df}{N - K}$$


\noindent in which $df$ are the degrees of freedom, and $K$ is the total number of groups in the design. If our $2^5 3$ design has a total sample of 3000, and $\eta^2_p = .$, 1the formula will yield: 

​    $$\epsilon^2_p = 1 - (1 - .20) * \frac{3000 - 96 + 5}{3000 - 96} =  0.09845041$$

When an effect size is not available, we can guess the variance explained and the residual variance as a proportion. To do this, we also need to guess the variance explained by other factors in the design, because that variance influences the residual variance.  Lets go ahead and set $\epsilon^2_p$ to .1 in R.

```R
etap <- .1 #eta
```



In order to account for the correlation from repeated measures in our theoretical sample, we need to compute the design effect (C) and multiply that by the effect size specified to gain the real effect size due to the repeated measures. We compute design effect by: 

​    $$C = \sqrt{\frac{R}{1 + (R-1) \rho}}$$

where R is the number of repeated measures (5 in our case), and $\rho$ is the correlation. Lets assume there is a correlation of .5 between the responses. We then compute the effect size accounting for correlated responses by:

 $$f = \frac{\eta^2_p}{1-\eta^2_p} * \sqrt{\frac{R}{1 + (R-1) \rho}}$$



which becomes: 
   $$f = \frac{.1}{1-.1} * \sqrt{\frac{5}{1 + (5-1) .5}} = 0.1434438$$

Now lets input this into R.

```R
rho=.5 #correlation of responses
r=5 # number of repeated measurments
c = sqrt(r/(1+(r-1)*rho)) # design effect
f2=etap/(1-etap)*c #effect size
```

Next, we need to caculate the required N by computing all the interactions of the factors, and taking the sum of each interaction. We can do so by running the following code. We are using the ```pwr``` package in R to do this computing so ensure you have it downloaded by running ```install.packages("pwr")```. 

``` R
library("pwr")
dfn_main<-(k1-1)*(k2-1)*(k3-1)*(k4-1)*(k5-1)*(k6-1)  # df for the main interaction
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
n # print the n
```

When running this, we see that for a $2^5 3$ factorial, with 5 repeated measures that have a .5 correlation, aiming for 80% power requires an N of 1076. 