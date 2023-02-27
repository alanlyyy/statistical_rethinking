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
sim_at <- data.frame(A=A_seq)

#simulate M and then D using A_seq
s <- sim( m5.3_A,  data=sim_dat, vars=c("M","D") )