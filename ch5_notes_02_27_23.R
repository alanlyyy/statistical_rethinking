#load data into dataframe d
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
str(d)

#standardize variables
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

sd(d$MedianAgeMarriage)

#model for standardized median age of marriage vs standardized divorce
m5.1 <- quap(
		alist(
			D ~ dnorm(mu, sigma),
			mu <- a + bA *A,
			a ~ dnorm( 0 , 0.2), # divorce rate is standardized
			bA ~ dnorm( 0 , 0.5 ),
			sigma ~ dexp(1)	   # stdev constrained to positive
			), data=d )

# sample 1000 parameters from the prior distribution of (a,bA, sigma)
set.seed(10)
prior <- extract.prior(m5.1)	
str(prior)

#apply the sampled priors to simulated divorce rates between -2 and 2 stdev.
mu <- link(m5.1, post=prior,  data=list( A=c(-2,2)) )

plot(NULL, xlim=c(-2,2), ylim=c(-2,2) )
for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha("black",0.4) )

#compute percentile interval of mean

#create unique standardized values
A_seq	<- seq( from =-3, to=3.2, length.out=30)

#apply the model, sampling from the prior, and applying samples to the sequence
mu <- link(m5.1, data=list(A=A_seq) )

#calculate the MAP
mu.mean <- apply(mu, 2, mean)

#calculate uncertainty around the MAP
mu.PI <- apply(mu,2, PI)

#plot it all 
plot(D ~ A, data=d, col=rangi2 )
lines( A_seq, mu.mean, lwd=2 ) # plot the map line
shade(mu.PI, A_seq ) 		#plot the uncertainty around the map line

precis(m5.1)


#plot standardized divorce rate vs standardized marriage rate
m5.2 <- quap( 
			alist(
				D ~ dnorm(mu, sigma),
				mu <- a + bM *M,
				a ~ dnorm( 0 , 0.2),
				bM ~ dnorm( 0 , 0.5),
				sigma ~ dexp(1)
				),
				data=d
				)

#create unique standardized values
A_seq	<- seq( from =-3, to=3.2, length.out=30)

#apply the model, sampling from the prior, and applying samples to the sequence
mu <- link(m5.2, data=list(M=A_seq) )

#calculate the MAP
mu.mean <- apply(mu, 2, mean)

#calculate uncertainty around the MAP
mu.PI <- apply(mu,2, PI)

#plot it all 
plot(D ~ M, data=d, col=rangi2 )
lines( A_seq, mu.mean, lwd=2 ) # plot the map line
shade(mu.PI, A_seq ) 		#plot the uncertainty around the map line

precis(m5.2)

#fit model to both predictor variables: Median age at marriage & Marriage rate
m5.3 <- quap(
			alist(
				D~ dnorm( mu , sigma),
				mu <- a + bM*M + bA* A,
				a ~ dnorm( 0 ,0.2),
				bM ~ dnorm(0,0.5),
				bA ~ dnorm(0,0.5),
				sigma ~ dexp(1)
				), data=d
		)

precis(m5.3)


m5.4 <- quap(
		alist(
			M ~ dnorm(mu, sigma), #marriage rate is normally distributed with parameters mu sigma
			mu <- a + bAM * A,    #average marriage rate is a function of median age of marriage
			a ~ dnorm(0, 0.2),
			bAM ~ dnorm( 0, 0.5),
			sigma ~ dexp(1)
			), data=d
		)

mu <- link(m5.4)
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$M - mu_mean


#plot posterior prediction plots

#create a distribution of sampled parameters using the original data for each case.
mu <- link(m5.3)

#summarize samples across cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2 , PI )


#sample parameters from the posterior, simulate observations for each case
D_sim <- sim(m5.3, n=1e4)
D_PI <- apply(D_sim, 2 , PI)
str(D_sim)

#plot the predicted MAP line vs observed data
plot(mu_mean ~ d$D, col=rangi2, ylim=range(mu_PI),
		xlab='Observed divorce', ylab="Predicted Divorce")
abline( a=0, b=1, lty=2)
for( i in 1:nrow(d) ) lines( rep(d$D[i], 2), mu_PI[,i], col=rangi2 )

identify(x=d$D, y=mu_mean, labels=d$Loc ) 

m5.3_A <- quap(
			alist(
				##A ->: D <- M
				D ~ dnorm( mu, sigma),
				mu <- a + bM *M + bA* A,
				a ~ dnorm(0,0.2),
				bM ~ dnorm(0, 0.2),
				bA ~ dnorm( 0, 0.5 ), 
				sigma ~ dexp(1),
				
				## A -> M
				M ~ dnorm( mu_M, sigma_M),
				mu_M <- aM + bAM*A,
				aM ~ dnorm( 0, 0.2),
				bAM ~ dnorm(0, 0.5),
				sigma_M ~ dexp(1)
				), data= d )

precis(m5.3_A)


#create 30 simulated median age of marriage ranging from -2 to 2 stdev from the mean.
A_seq <- seq( from=-2, to=2, length.out=30 )

#prep data
sim_dat <- data.frame(A=A_seq)

#simulate M and then D using A_seq
s <- sim( m5.3_A,  data=sim_dat, vars=c("M","D") )


#get the mean of simulated divorce rates for each simulated median age of marriage
plot( sim_dat$A, colMeans(s$D), ylim=c(-2,2), type="l",xlab="manipulated A", ylab="counterfactual D" )

#plot the prediction interval of simulated divorce rates with simulated median age of marriage
shade( apply(s$D, 2 , PI), sim_dat$A )
mtext( "Total counterfactual effect of A on D" )


#get the causal effect of median age moving from 20 to 30
sim2_dat <- data.frame( A=(c(20,30) - 26.1)/1.24)

#simulate predicted divorce rates for median age 20 & 30
s2 <- sim(m5.3_A, data=sim2_dat, vars=c("M","D") )

#get the mean difference in divorce rate between A = 20 & 30
mean(s2$D[,2] -s2$D[,1])

#
#Counterfactual plot holding A constant varying M
#

#vary M hold A constant at its mean
sim_dat <- data.frame( M =seq(from=-2, to=2, length.out=30), A=0 )

#apply model to data, vary M holding A constant
s <- sim(m5.3_A, data=sim_dat, vars="D")
str(s)


plot(sim_dat$M, colMeans(s), ylim=c(-2,2), type="l",
	xlab="Manipulated M", ylab = "Counterfactual D" )
shade( apply(s,2,PI), sim_dat$M )
mtext( "Total Counterfactual Effect of M on D" )


#
#
#
#Masked relationship
#
#
#
library(rethinking)
data(milk)
d <- milk
str(d)

d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))

#test fitting the model with flat priors
m5.5_draft <- quap(
			alist(
				K ~dnorm(mu, sigma),
				mu <- a + bN*N,
				a ~ dnorm(0,1),
				bN ~ dnorm(0,1),
				sigma ~ dexp(1)
				), data=d
			)

#identify null vvalues in the columns of neocortex percent
d$neocortex.perc

#extract cases without any null values in K,N, and M
dcc <- d[ complete.cases(d$K, d$N, d$M), ]

#retest the fitting of the model with removal of NA values
m5.5_draft <- quap(
			alist(
				K ~dnorm(mu, sigma),
				mu <- a + bN*N,
				a ~ dnorm(0,1),
				bN ~ dnorm(0,1),
				sigma ~ dexp(1)
				), data=dcc
			)


#sample 1000 parameters from the prior distributiosn of alpha and sigma
prior <- extract.prior( m5.5_draft)

#bounds of our dataset (95% compatability interval
xseq <- c(-2,2)

#apply prior parameters to compatability interval bounds
mu <- link(m5.5_draft, post=prior, data=list(N=xseq) )
str(mu)
#plot the 50 regression lines
plot(NULL, xlim=xseq, ylim=xseq )
for ( i in 1:50 ) lines(xseq, mu[i,], col=col.alpha("black",0.3) )

precis(m5.5_draft)

#model with tighter priors for a & bN
m5.5 <- quap(
			alist(
				K ~dnorm(mu, sigma),
				mu <- a + bN*N,
				a ~ dnorm(0,0.2),
				bN ~ dnorm(0,0.5),
				sigma ~ dexp(1)
				), data=dcc
			)
precis(m5.5)

#create a sequence of standardized neocortex 
xseq <- seq( from=min(dcc$N)-0.15, to=max(dcc$N)+0.15, length.out=30 )

#apply model to sequence, sample from the posterior
mu <- link (m5.5, data=list(N=xseq))

#calculate the map line
mu_mean <- apply(mu,2,mean)

#calculate uncertainty around the map line
mu_PI <- apply(mu,2,PI)

#plot the data 
plot(K ~N, data=dcc)
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq )

m5.6 <- quap(
		alist(
			K ~ dnorm(mu, sigma),
			mu <- a + bM*M,
			a ~ dnorm(0, 0.2),
			bM ~ dnorm( 0, 0.5),
			sigma ~ dexp(1)
			),
		 data= dcc)
precis(m5.6)

#create a sequence of standardized log body mass
xseq <- seq( from=min(dcc$M)-0.15, to=max(dcc$M)+0.15, length.out=30 )

#apply model to sequence, sample from the posterior
mu <- link (m5.6, data=list(M=xseq))

#calculate the map line
mu_mean <- apply(mu,2,mean)

#calculate uncertainty around the map line
mu_PI <- apply(mu,2,PI)

#plot the data 
plot(K ~M, data=dcc)
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq )


#adding both predictors to the model
m5.7 <- quap(
		alist(
			K ~ dnorm(mu, sigma),
			mu <- a + bN*N+ bM*M,
			a ~ dnorm( 0, 0.2),
			bN ~ dnorm( 0, 0.5),
			bM ~ dnorm( 0, 0.5),
			sigma ~ dexp(1)
			), data=dcc
		)
#the introduction of both predictor variables in the regression, the posterior association with the outcome
#of both with the outcome has increased.
precis(m5.7)

plot( coeftab(m5.5, m5.6, m5.7), pars=c("bM", "bN") )

pairs( ~K +M + N, dcc)

#
#Hold N constant, vary M
#
#

#create simulated data for standardized log mass
xseq <- seq( from=min(dcc$M) -0.15, to=max(dcc$M)+0.15, length.out=30 )

#fit model with simulated log mass, and hold neocortex % at its average.
mu <- link(m5.7, data=data.frame(M=xseq,N=0) )

#calculate the map, mean for each case
mu_mean <- apply(mu,2,mean)

#calculate the PI, for each case
mu_PI <- apply(mu,2,PI)

#plot vary m while holding N constant
plot(NULL, xlim=range(dcc$M), ylim=range(dcc$K) )
lines(xseq,mu_mean, lwd=2)
shade(mu_PI, xseq)

#
#holding M constant, vary N
#
#create simulated data for standardized log mass
xseq <- seq( from=min(dcc$N) -0.15, to=max(dcc$N)+0.15, length.out=30 )

#fit model with simulated log mass, and hold neocortex % at its average.
mu <- link(m5.7, data=data.frame(M=0,N=xseq) )

#calculate the map, mean for each case
mu_mean <- apply(mu,2,mean)

#calculate the PI, for each case
mu_PI <- apply(mu,2,PI)

#plot vary M while holding N constant
plot(NULL, xlim=range(dcc$N), ylim=range(dcc$K) )
lines(xseq,mu_mean, lwd=2)
shade(mu_PI, xseq)

#
#categorical variables
#
data(Howell1)
d <- Howell1
str(d)

d$sex <- ifelse(d$male==1, 2, 1)

m5.8 <- quap(
		alist(
			height ~ dnorm(mu, sigma),
			mu <- a[sex],
			a[sex] ~dnorm(178,20),
			sigma ~ dunif(0,50)
			), data=d
		)
precis(m5.8,depth=2)

#sample 10000 parameters from the posterior
post<- extract.samples(m5.8)

#examine the difference in mean between the 2 categories
post$diff_fm <- post$a[,1] - post$a[,2]
precis(post,depth=2)

#load new dataset
data(milk)
d <- milk

#dataset contains multiple categories
levels(d$clade)

#encode indicator for each category
d$clade_id <- as.integer(d$clade)
d$clade_id

#standardize K
d$K <- standardize( d$kcal.per.g)

m5.9 <- quap(
		alist(
			K ~ dnorm(mu, sigma),
			mu <- a[clade_id],
			a[clade_id] ~ dnorm(0, 0.5),
			sigma ~ dexp(1)
			), data=d
		)
labels <- paste ( "a[", 1:4, "]:", levels(d$clade), sep="" )
labels
plot( precis(m5.9, depth=2, pars="a"), labels=labels, xlab="expected kcal (std)" )


#add another categorical variable to the model
set.seed(63)
d$house <- sample( rep(1:4,each=8),  size=nrow(d) )

m5.10 <- quap(
			alist(
				K ~ dnorm(mu, sigma),
				mu <- a[clade_id] + h[house],
				a[clade_id] ~ dnorm(0, 0.5),
				h[house] ~ dnorm(0, 0.5),
				sigma ~ dexp(1)
				), data=d
		)

precis(m5.10, depth=2)
