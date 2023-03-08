library(rethinking)

data(WaffleDivorce)
d <- WaffleDivorce


#5M3

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

lds <- read.csv("C:\\Users\\aly\\Documents\\statistical_rethinking\\ch5_LDS_data.csv")
str(lds)

#extract state and mormon rate
lds_subset <- lds[c("state","mormonRate")]
colnames(lds_subset)[1]  <- "Location" 

#join 2 datasets by state
df = merge(x = d, y = lds_subset, by = "Location")
str(df)

df$mormonRateStandardized <- standardize(df$mormonRate*100)


sd(d$M)
sd(d$A)
sd(d$mormonRateStandardized)

mD <- quap(
		alist(
			D ~ dnorm(mu, sigma),
			mu <- alpha + bM*M + bA*A + bMormon*mormonRateStandardized,
			alpha ~ dnorm(0,1),			# flat prior, 95% of divorce rates fall within 2 stdev
			bM ~ dnorm(0, 0.5),			# 95% of slopes fall within a one unit change in divorce rate is a 1 unit change in marriage rate
			bA ~ dnorm(0, 0.5),			# 95% of slopes fall within a one unit change in divorce rate is a 1 unit change in median age of marriage
			bMormon ~ dnorm(0, 0.5),		# 95% of slopes fall within a one unit change in divorce rate is a 1 unit change in mormon rate
			sigma ~ dexp(1)				#constrain sigma to be positive
			), data=df )

precis(mD) # mormon rate is strongly negatively associated to divorce rate



mD2 <- quap(
		alist(
			D ~ dnorm(mu, sigma),
			mu <- alpha + bM*M + bA*A,
			alpha ~ dnorm(0,1),			# flat prior, 95% of divorce rates fall within 2 stdev
			bM ~ dnorm(0, 0.5),			# 95% of slopes fall within a one unit change in divorce rate is a 1 unit change in marriage rate
			bA ~ dnorm(0, 0.5),			# 95% of slopes fall within a one unit change in divorce rate is a 1 unit change in median age of marriage
			sigma ~ dexp(1)				#constrain sigma to be positive
			), data=d )

precis(mD2)

#The inclusion of the mormon predictor variable, reduced the association between age at marriage and divorce.
#The mormon population predictor variable is weakly associated to divorce rate.

#5H1.
#What are the implied conditional independencies of the DAG: M -> A -> D
#
#The implied conditional independencies of the DAG are 
#
#M directly influences A
#A directly influences D
#M indirectly influences D

#5H2. Assuming that the DAG for the divorce example is indeed M-> A -> D,
#fit a new model and use it to estimate the counterfactual effect of halving
#a states marriage rate M. Use the counterfactual example from the chapter 
#(p.140) as a template
#

m5.3 <- quap(
			alist(
				## A -> D <- M
				D ~ dnorm(mu, sigma),
				mu <- a + bM*M + bA*A,
				a ~ dnorm(0, 0.2),
				bM ~ dnorm(0, 0.5),
				bA ~ dnorm(0, 0.5),
				sigma ~ dexp(1)
				), data=d )

precis(m5.3)

m5.3_A <- quap(
			alist(
				## A -> D <- M
				D ~ dnorm(mu, sigma),
				mu <- a + bM*M + bA*A,
				a ~ dnorm(0, 0.2),
				bM ~ dnorm(0, 0.5),
				bA ~ dnorm(0, 0.5),
				sigma ~ dexp(1),

				## A -> M
				M ~ dnorm( mu_M, sigma_M),
				mu_M <- aM + bAM*A,
				aM ~ dnorm(0, 0.2),
				bAM ~ dnorm( 0,0.5 ),
				sigma_M ~ dexp(1)
				), data=d 
			)

precis(m5.3_A) #looking at bAM, M & A are strongly negatively correlated


#create a model M -> A -> D
m5.3_B<- quap(
			alist(
				## A -> D
				D ~ dnorm(mu, sigma),
				mu <- a + bA*A,
				a ~ dnorm(0, 0.2),
				bA ~ dnorm(0, 0.5),
				sigma ~ dexp(1),

				## M -> A
				A ~ dnorm( mu_M, sigma_M),
				mu_M <- aM + bMA*M,
				aM ~ dnorm(0, 0.2),
				bMA ~ dnorm( 0,0.5 ),
				sigma_M ~ dexp(1)
				), data=d 
			)
precis(m5.3_B) 
#A is negativiely correlated with D, 
#while M is negatively correlated with A

#estimate the effect of varying M while holding A constant
#
M_seq <- seq(from=-2, to=2, length.out=30)

#simulate A using observed M then use the simulated data of A inside model D
sim_dat <- data.frame(M=M_seq)
s <- sim(m5.3_B, data=sim_dat, vars=c("A", "D") )

plot(sim_dat$M, colMeans(s$D), ylim=c(-2,2), type="l",
		xlab="maniupulated M", ylab="counterfactual D" )

shade( apply(s$D,2, PI), sim_dat$M)
mtext("Counterfactual effect of M on D")


plot(sim_dat$M, colMeans(s$A), ylim=c(-2,2), type="l",
		xlab="maniupulated M", ylab="counterfactual A" )

shade( apply(s$A,2, PI), sim_dat$M)
mtext("Counterfactual effect of M on A")

#M is nagatively associated to A, while M is positively associated to D

#simulate the effect of halving marriage rate M
sim2_dat <- data.frame( M = (c(mean(d$Marriage)/2,mean(d$Marriage)) - mean(d$Marriage))/sd(d$Marriage) ) 
str(sim2_dat)
s2 <- sim(m5.3_B, data=sim2_dat, vars=c("A","D") )
mean(s2$D[,2] -s2$D[,1]) # halving the marriage rate, reduces the divorce rate by one standard deviation.


#5H3. model m5.7, assume the true causal relationship among the variables is:
# M-> N -> K, M-> K
#compute the counterfactual effect of doubling M.
#account for both the direct and indirect paths of causation.

library(rethinking)
data(milk)
d <- milk

d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))

dcc <- d[complete.cases(d$K, d$N, d$M), ]
m5.7 <- quap(
			alist(
				## M -> K <- N (direct path) use simulated N and oberserved M to simulate K
				K ~ dnorm(mu, sigma),
				mu <- a + bN*N + bM*M,
				a ~ dnorm(0,0.2),
				bN ~ dnorm(0,0.5),
				bM ~ dnorm(0, 0.5),
				sigma ~ dexp(1),


				##M -> N (indirect path) use observed M to simulate N
				N ~ dnorm(mu_MN,sigma_MN),
				mu_MN <- aMN + bMN*M,
				aMN ~ dnorm(0,0.2),
				bMN ~ dnorm(0, 0.5),
				sigma_MN ~ dexp(1)
				
				), data=dcc
				
		)

precis(m5.7) #M is positively associated to N


M_seq <- seq(from=-2, to=2, length.out=30)

#simulate N using observed M then use the simulated data of N inside model K
sim_dat <- data.frame(M=M_seq)
s <- sim(m5.7, data=sim_dat, vars=c("N", "K") )

#M is negatively associated to K
plot(sim_dat$M, colMeans(s$K), ylim=c(-2,2), type="l",xlab="maniupulated M", ylab="counterfactual K" )
shade( apply(s$K,2, PI), sim_dat$M)
mtext("Counterfactual effect of M on K") 

#simulate the effect of doubling body mass M
sim2_dat <- data.frame( M = (c( mean( log(d$mass*2) ), mean( log(d$mass) ) ) - mean( log(d$mass) ) )/sd( log(d$mass) ) ) 
str(sim2_dat)
s2 <- sim(m5.7, data=sim2_dat, vars=c("N","K") )
mean(s2$K[,1] -s2$K[,2]) # doubling the body mass decreases the milk energy by 0.11 standard deviations.


#5H4.
