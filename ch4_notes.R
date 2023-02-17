library(rethinking)
Errors sum

#take the sum of a sample with 16 values between -1 and 1, repeat for 1000 trials
pos <- replicate(1000, sum(runif(16,-1,1)) ) 
hist(pos) #plot density of the data

#Errors multiply

#take a sample of 12 between 1 and 1.1 and take the product of all the numbers
prod( 1 + runif(12,0,0.1)) 

Repeat above process 10000 times
growth <- replicate(10000, prod(1 + runif(12,0,0.01) ))
dens(growth, norm.comp=TRUE) #norm.comp adds a normal curve for comparison.

#The normal approximation for multiplication does not apply to larger effects.
big <- replicate(10000, prod( 1 + runif(12,0,0.5) ) )
dens(big, norm.comp=TRUE)

small <- replicate(10000, prod(1 + runif(12,0,0.01) ) )
dens(small, norm.comp=TRUE)

#large deviates produce gaussian dist on the log scale
log.big <- replicate( 10000, log(prod( 1+ runif(12,0,0.5) ) ) )
dens(log.big, norm.comp=TRUE)

#grid approximation posterior distribution
w <-6 #6 successes
n <- 9 # 9 tosses per sample
p_grid <-seq(from=0,to=1, length.out=100)
posterior <- dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior <- posterior/ sum(posterior)
dens(posterior)


#4.3.1 The data
library(rethinking)
data(Howell1)
d <- Howell1
str(d) #investigate columns in the dataframe d

#accessing the height column
d$height

#filter the dataframe down to individuals of age 18 or greater with
d2 <- d[ d$age >= 18, ]
d2

#plotting the prior for mu
curve( dnorm(x, 178, 20 ), from= 100, to=250) 
#plot the normal distribution from 100 to 250, using mu = 178, and sigma =20

#plotting the prior for sigma
#uniform prior from 0 to 50
curve( dunif( x, 0, 50 ), from= -10, to = 60)

#the prior function for mu, normal dist with mu =178, and sigma =20
sample_mu <- rnorm(1e4, 100,20)

#the prior function for sigma, uniform dist bounded by 0 and 50
sample_sigma <- runif(1e4,0,50)

#create a joint probability distribution/ prior for mu and sigma
#by creating a sampling distribution of mu and sigma.
prior_h <-rnorm(1e4, sample_mu, sample_sigma)

dens( prior_h)


#quadration approximatation example

#create a list of mu 200 points
mu.list <- seq( from = 140, to = 160, length.out=200)

#create a list of sigma 200 points
sigma.list <- seq( from=4, to=9, length.out = 200)

#create a dataframe for each mu and sigma
post <- expand.grid(mu=mu.list, sigma=sigma.list )

#for each mu, sigma pair, integrate the normal distribution for each height
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm( 
					d2$height,
					mean = post$mu[i],
					sd = post$sigma[i],
					log =TRUE ) ) )


post$prod <- post$LL + dnorm ( post$mu, 178, 20, TRUE) +
			dunif( post$sigma, 0 ,50, TRUE)

post$prob <- exp(post$prod - max(post$prod) ) 

#contour plot
contour_xyz(post$mu,post$sigma, post$prob)

#heat map
image_xyz(post$mu, post$sigma, post$prob)


#4.3.4 Sampling from the posterior:

#grab 10000 samples following the posterior distribution of heights
sample.rows <- sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob ) 

#extract the mu and sigma using the rows
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

#identify most plausible mu and sigma in the center of the plot
plot(sample.mu, sample.sigma, cex=0.5, pch=16, col=col.alpha(rangi2,0.1))

#view the normal distributions of mu and sigma
dens(sample.mu)
dens(sample.sigma)

#identiy width of densities with highest posterior density intervals
HPDI(sample.mu)
HPDI(sample.sigma)

#examining sample size effect on variance
d3 <- sample(d2$height, size=20 )

#the distribution of mu and sigma have a longer tail for larger variances
#the distribution is not cetnered.

mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
col=col.alpha(rangi2,0.1) ,
xlab="mu" , ylab="sigma" , pch=16 )

#sigma is not normal, and has a longer tail of uncertainty towards higher values.
dens( sample2.sigma , norm.comp=TRUE )

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

#store the model formula
flist <- alist(
	height ~ dnorm(mu, sigma),
	mu ~ dnorm( 178, 20),
	sigma ~ dunif(0,50 )
	)

#fit the model to the data
m4.l <- map(flist,data=d2)

#look at the summary of the fit data.
#observe the posterior mode
precis( m4.l)


#use the map algorithm change the sigma of the prior to 0.1
m4.2 <- map(
		alist(
			height ~ dnorm(mu, sigma),
			mu~ dnorm(178,0.1),
			sigma ~ dunif(0, 50)
			),	
			data = d2 )
m4.2
precis(m4.2)


vcov(m4.l)

#list of variances
diag(vcov(m4.l))

#correlation amongst 2 variables bounded by -1 to +1
cov2cor( vcov(m4.l) )

#sample vectors of values from the posterior multi-dim gaussian distribution
post <- extract.samples(m4.l, n=1e4 )
head(post)

#get the summary stats of the parameters
precis(post)


#4.4 Adding a Predictor variable

#view correlation between height and weight
plot( d2$height ~ d2$weight)

#load data again
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

#fit model
m4.3 <- map(
		alist(
			height ~ dnorm(mu, sigma), 
			mu <- a+ b*weight,
			a ~ dnorm(156, 100),
			b ~ dnorm( 0 ,10),
			sigma ~ dunif(0, 50)
			),
			data =d2 )

#inspect the estimates
precis(m4.3)

#inspect the estimates and correlation table
precis(m4.3, corr=TRUE)
cov2cor( vcov(m4.3) ) #there is a correlation between variables a and b

#Removing correlation between variables:

#centering
d2$weight.c <- d2$weight - mean(d2$weight)

#mean should be 0
mean(d2$weight.c)


#refit model with centered weight variable
m4.4 <- map(
		alist(
			height ~ dnorm(mu, sigma), 
			mu <- a+ b*weight.c,
			a ~ dnorm(178, 100),
			b ~ dnorm( 0 ,10),
			sigma ~ dunif(0, 50)
			),
			data =d2 )

precis(m4.4, corr=TRUE)
cov2cor( vcov(m4.4) ) #the correlation amongst a and b have been removed.


mean(d2$height)

#plot height and weight relationship
plot(height ~ weight, data=d2)

#plot the mean line that fits all the data.
abline(a=coef(m4.3)["a"], b=coef(m4.3)["b"] )

#sample alpha and beta from the model to plot uncertainty
post <- extract.samples(m4.3) 
#extract first 5 rows
post[1:5,]

vcov(m4.3)

#number of data points 
N <- 100
dN <- d2[ 1:N , ]

#create posterior distribution fit model using alpha,beta, snd sigma
mN <- map(
alist(
height ~ dnorm( mu , sigma ) ,
mu <- a + b*weight ,
a ~ dnorm( 178 , 100 ) ,
b ~ dnorm( 0 , 10 ) ,
sigma ~ dunif( 0 , 50 )
) , data=dN )

# extract 20 samples from the posterior (alpha, beta, and sigma)
post <- extract.samples( mN , n=20 )

# display raw data and sample size
plot( dN$weight , dN$height ,
xlim=range(d2$weight) , ylim=range(d2$height) ,
col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 )
abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3) )

#calculate mean when xi = 50 kg
mu_at_50 <- post$a + post$b * 50

#at xi= 50, the uncertainty around the mean height fluctuates following
#a gaussian curve.
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )

#calculate the 89% HDPI when mu = 50kg for the mean height
HPDI(mu_at_50, prob=0.89)

#sample the posterior distribution for alpha, beta, and sigma
#compute mu for each data point 
mu <- link(m4.3)
str(mu)

#define sequence of weights to compute predictions for 
#these values will be on the horizontal axis
weight.seq <- seq( from=25, to=70, by=1)

#use link to compute mu
#for each sample from the posterior
#and for each weight in weight.seq
mu <- link(m4.3, data=data.frame(weight=weight.seq))
str(mu)

#use type="n" to hide raw data
plot(height~weight, d2, type="n")

#for each joint posterior(a,b,sigma) sample plot the col of mu values, for xi = [25,70]
for (i in 1:100)
	points( weight.seq, mu[i,], pch=16, col=col.alpha(rangi2,0.1))

#summarize the distribution of mu

#compute the mean at each weight value from [25, 70]
mu.mean <- apply(mu, 2, mean)
mu.mean
#compute the HPDI at each weight value from [25, 70]
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
mu.HPDI

#plot raw data
#fading out points to make line and interval more visible
plot( height ~ weight, data=d2, col=col.alpha(rangi2,0.5))

#plot the MAP line, aka the mean mu for each weight
lines( weight.seq, mu.mean )

#plot a shaded region for 89% HPDI
shade( mu.HPDI, weight.seq)

#sample 1000 points for each weight from the posterior, applying the posterior params to get simulated heights
sim.height <- sim( m4.3 , data=list(weight=weight.seq))
str(sim.height)

#calculate the 89% posterior prediction interval for the 46 heights
height.PI <- apply(sim.height, 2,PI, prob=0.89)
height.PI


#-------------plot everything---------------------------

#plot raw data
plot( height ~ weight, d2, col=col.alpha(rangi2,0.5) )

#draw MAP line
lines( weight.seq, mu.mean )

#draw HPDI region for line
shade(mu.HPDI, weight.seq)

#draw PI region for simulated heights
shade(height.PI, weight.seq)

#-----------increase simulated heights from 1000 to 10000
#increase sample to 10000 points for each weight from the posterior, applying the posterior params to get simulated heights
sim.height <- sim( m4.3 , data=list(weight=weight.seq), n=1e4)
height.PI <- apply(sim.height, 2,PI, prob=0.89)
#plot raw data
plot( height ~ weight, d2, col=col.alpha(rangi2,0.5) )

#draw MAP line
lines( weight.seq, mu.mean )

#draw HPDI region for line
shade(mu.HPDI, weight.seq)

#draw PI region for simulated heights
shade(height.PI, weight.seq)


#---------polynomial regression------------------
library(rethinking)
data(Howell1)
d <- Howell1
str(d)
plot( height ~ weight,d)


#standardize weight, weight.s has mean 0 with sigma = 1
d$weight.s <- ( d$weight - mean(d$weight)) / sd(d$weight)

plot(height ~weight.s,d)


#----------fit data with the polynomoial model
d$weight.s2 <- d$weight.s^2

#fit with polynomial model
m4.5 <- map(
		alist(
			height ~dnorm( mu, sigma),
			mu <- a + b1*weight.s + b2*weight.s2,
			a ~ dnorm( 178, 100),
			b1 ~ dnorm( 0, 10),
			b2 ~ dnorm(0 ,10),
			sigma ~ dunif(0, 50)
			),
			data=d )
#review summary table
precis(m4.5)

#create a list of standardized weights
weight.seq <- seq( from=-2.2, to=2, length.out=30)

#create weight and weight^2 for model fitting
pred_dat <- list( weight.s=weight.seq,  weight.s2=weight.seq^2 )

#fit posterior distribution (a_i, b_i,b2_i,sigma_i) to each weight
mu <- link (m4.5, data=pred_dat)

#calculate mean for 30 weights
mu.mean <- apply(mu, 2, mean)

#calculate 89% confidence interval (Upper and Lower bound) for 30 weights
mu.PI <- apply(mu,2, PI, prob=0.89)
mu.PI

#simulate 1000 predicted values for each weight, for 30 weights, using the posterior distribution
sim.height <- sim(m4.5, data=pred_dat )
sim.height

#calculate the 89% prediction interval for the 30 weights
height.PI <- apply(sim.height, 2, PI, prob=0.89 )
height.PI

#plot raw data
plot( height ~ weight.s, d, col=col.alpha(rangi2,0.5) )

#plot the mean line, MAP
lines( weight.seq, mu.mean )

#plot the uncertainty around the MAP
shade( mu.PI, weight.seq)

#plot the 89% prediction interval around the MAP
shade( height.PI, weight.seq)
