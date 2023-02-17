library(rethinking)
data(Howell1)
d <- Howell1

# columns = variables
# rows = (persons)
str( d)

#access height vector in d dataframe
# $ == extract
#extract the column height from the dataframe d
d$height

#access column male using double bracket notation found in a list
d[['male']]

#filter the dataframe for rows where age is greater than 18
d2 <- d[d$age >= 18,]
str(d2)

#get all columns on row 3
d[3,]

#get all columns & rows in dataframe
d[,]

#plot distribution(density) of heights
dens(d2$height)
#distribution of sums tends to converge to a Gaussian distribution

#height is normally distributed with parameters mu and sigma
#mu is normally distributed with a mean of 178cm and sigma of 20cm
#sigma is uniformly distributed with a mean of 0cm and sigma of 50cm
#mu and sigma are priors with domain knowledge applied.

#plot priors to verify assumptions made to build a model
#plot prior for parameter mu
curve(dnorm(x, 178, 20), from= 100, to=250)

#plot prior for parameter sigma, 
#flat prior constrains sigma to be positive from 0 to 50, 
#each sigma is equally likely
curve( dunif(x,0,50) , from=-10, to=60)

#create a prior for mu, 
#sampling 10000 times from a normal distribution with a mu = 178, and sigma = 20
sample_mu <- rnorm(1e4, 178, 20)

#create a prior for sigma
#sample 10000 times from a uniform distribution with the bounds 0 to 50
sample_sigma <- runif(1e4, 0 ,50)

#create a joint prior for random variable height
#using the priors mu and sigma
prior_h <- rnorm(1e4, sample_mu, sample_sigma)

#plot a histogram of prior heights
dens( prior_h)


#playing around with priors example

#average height of males in us population = 175cm
#95% of male heights should fall 145 to 190cm
sample_mu <- rnorm(1e4, 175, 15)

#95% of heights lie within +/- 100cm of the average height - conservative
sample_sigma <- runif(1e4, 0 ,50)

#height is normally distributed, 
#for 10000 trials randomly pick a mu and sigma in each trial to get a random variable height
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens( prior_h)
mean(prior_h)

#create a list of mu
mu.list <- seq( from= 140, to=160, length.out=200)
str(mu.list)
#create a list of sigma
sigma.list <- seq( from=4, to=9, length.out=200 )
str(sigma.list)

#create a mu and sigma pair
post <- expand.grid( mu=mu.list, sigma=sigma.list )
str(post) #200 mu x 200 sigma = 40000 mu and sigma pairs

#for each mu & sigma pair in post, apply the following function:
# 1) apply normal distribution with mu and sigma to the heights vector.
# 2) sum of the densities for the current mu and sigma
# since height follows a log normal distribution, take the log
post$LL <-sapply(1:nrow(post), function(i) 
							sum( dnorm(
									d2$height,
									mean= post$mu[i],
									sd=post$sigma[i],
									log=TRUE
								)
								)
								)
#postLL is in the log scale
#create a distribution of mu with prior 178,20 in log scale
#create a distribution sigma with prior 0,50 in log scale
#
#
#addition in log scale = multiplication in linear scale
post$prod <- post$LL + dnorm( post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
#
#calculate the density in linear scale post$prod/max(post$prod)
post$prob <- exp(post$prod - max(post$prod))

#for each mu and each sigma plot the probability density.
contour_xyz(post$mu, post$sigma, post$prob )

#create a contour heat map
image_xyz( post$mu, post$sigma, post$prob )

#perform importance sampling of 10000 rows with replacement using the posterior distribution probability as weights
sample.rows <- sample( 1:nrow(post), size=1e4, replace=TRUE, prob=post$prob ) 

#extract the mean value for the sampled rows 
sample.mu <- post$mu[sample.rows]

#extract the sigma value for the sampled rows
sample.sigma <- post$sigma[sample.rows]

#plot the 100000 mu and sigma values, the most plausible combination of mu and sigma are found in the center of the plot
plot( sample.mu, sample.sigma, cex=1, pch=16, col=col.alpha(rangi2,0.1))


#plot histogram of distribution of parameter mu, while sigma is constant
dens(sample.mu)

#plot histogram of distribution of parameter sigma , while mu is constant
dens(sample.sigma)

#as sample size increases , posterior densities approach the normal distribution

#summarize posterior density intervals 89% percentile intervals
HPDI(sample.mu)
HPDI(sample.sigma)


#assessing the long tail of the posterior distribution of sigma

#randomly sample 20 heights from the heights dataset
d3 <- sample(d2$height, size=20)

#create a list of parameter mu 20 pts from 150 to 170
mu.list <- seq(from=150, to170, length.out=200)

#create a list of parameter sigma 20 pts from 4 to 20
sigma.list <-seq(from=4, to=20, length.out=200)

#create 400 unique mu and sigma pairs
post2 <- expand.grid(mu=mu.list, sigma=sigma.list)

#for each mu & sigma pair apply the following
#	1. extract log densities of the 20 height using the mu and sigma
#	2. sum the total densities
#	3. as a result each mu and sigma pair has a probability density attached to it.
post2$LL <- sapply( 1:nrow(post2), function(i)
								sum( dnorm(d3, 
										mean=post2$mu[i],
										 sd=post2$sigma[i],
										 log=TRUE
										)
									)
			)
#combine the log likelihood distribution with the prior mu and prior for sigma
#for prior mu normally distributed with mean = 178, and sigma =20, what is the probability of getting each parameter mu 
#for prior sigma uniformly distributed with bounds 0 to 50 what is the probability of getting each parameter sigma
#add all functions in the log domain
post2$prod <- post2$LL + dnorm( post2$mu, 178, 20, TRUE) + dunif( post2$sigma, 0, 50, TRUE)

#normalize by the average likeliehood and convert to linear scale, for each mu,sigma pair has a posterior pdf
post2$prob  <- exp(post2$prod - max(post2$prod) )

#ramdomly sample 10000 mu and sigma pairs, with replacement, from post2 using the probability weights of each mu, sigma pair.
sample2.rows <- sample( 1:nrow(post2), size=1e4, replace=TRUE, prob=post2$prob )

#extract the mu value from the sampled rows
sample2.mu <- post2$mu[ sample2.rows ]

#extract the sigma values from the sampled rows
sample2.sigma <- post2$sigma[sample2.rows]

plot( sample2.mu, sample2.sigma,
		 cex=1, col=col.alpha(rangi2,0.1),
			xlab="mu", ylab="sigma", pch=16)

#plot a histogram for the distribution of sigma with mu fixed. 
dens(sample2.sigma, norm.comp=TRUE)

#The posterior is not gaussian, but contains a long tail of uncertainty towards higher values.


library(rethinking)
data(Howell1)
#load howell dataset into dataframe
d <- Howell1
#filter all rows where age is at least 18 years of age
d2 <- d[ d$age >= 18, ]

flist <- alist(
			height ~ dnorm( mu, sigma), #height is normally distributed with parameters mu and sigma
			mu ~ dnorm(178, 20), #mu is normally distributed with prior values 178, 20
			sigma ~ dunif(0, 50) #sigma is normally distributed with prior values 0 and 50
)

#fit the model to the dataset
m4.1 <- map(flist, data=d2)

#review the fit of the model
precis(m4.1)

#the probability of mu, holding sigma constant,  is normally distributed with a mean = 154.6 and sd = 0.4


#to display 95% percentile intervals
precis(m4.1, prob=0.95)

#initialize starting values for the MAP modeling
start <- list(
			mu= mean(d2$height),
			sigma= sd(d2$height)
		)


# height is uniformly distributed with parameters mu and sigma
#mu is uniformly distributed with a mu=178, and sigma= 0.1 (more narrow prior)
#sigma is uniformly distributed with bounds 0 to 50
m4.2 <- map(
		alist(
			height ~ dnorm( mu, sigma),
			mu ~ dnorm( 178, 0.1),
			sigma ~ dunif(0,50)
			),
			data=d2
			)
precis(m4.2)

#look at variance matrix of 2d gaussian model
vcov(m4.1)


library(rethinking)

#extract 10000 mu & sigma pairs from model m4.1 (posterior distribution)
post <- extract.samples(m4.1,n=1e4)
head(post)
str(post)

#summarizing the samples
precis(post)

#compare post to m4.1
precis(m4.1)


# fit model
m4.3 <- map(
alist(
height ~ dnorm( mu , sigma ) ,
mu <- a + b*weight ,
a ~ dnorm( 156 , 100 ) ,
b ~ dnorm( 0 , 10 ) ,
sigma ~ dunif( 0 , 50 )
) ,
data=d2 )


#analyze the covariance of height and weight
plot(d2$height ~ d2$weight)

#center my random variable weight
d2$weight.c <- d2$weight - mean(d2$weight)

#mean of the centered variable should be 0
mean(d2$weight.c)

#fit model with centered variable weight
m4.4 <- map(
		alist(
			height ~ dnorm( mu, sigma),
			mu <- a+b*weight.c,
			a ~ dnorm( 178, 100 ),
			b ~ dnorm( 0, 10 ),
			sigma ~ dunif( 0, 50 )
			),
			data=d2
			)

precis(m4.4, corr=TRUE )

#plot raw data of weight,x, vs height
plot( height ~ weight, data=d2)

#plot the MAP line from the model that fits the relationship between weight & height
abline( a=coef(m4.3)["a"], b=coef(m4.3)["b"] )

#sample alpha,beta, sigma pairs from the posterior distribution of the model
post <- extract.samples(m4.3)
post[1:5,]

vcov(m4.3)

#extract the first 100 points
N <- 10
dN <- d2[1:N, ]

#fit the model to the first 10 points
mN <- map(
		alist(
			height ~ dnorm(mu, sigma),
			mu <- a + b*weight,
			a ~ dnorm( 178, 100),
			b ~ dnorm(0,10),
			sigma ~ dunif( 0,50)
			), 
			data= dN 
			)

#extract 20 samples from the posterior
post <- extract.samples(mN, n=20)

#display the raw data and sample size
plot( dN$weight,
	dN$height,
	xlim=range(d2$weight),
	ylim=range(d2$height) ,
	col=rangi2, 
	xlab="weight",
	ylab="height"
	)

#plot the sampled lines from the posterior distribution, with transparency
for (i in 1:20)
	abline( a=post$a[i], b=post$b[i], col=col.alpha("black",0.3) )


#grab the mu at a weight of 50kg from the posterior distribution
#each regression line, 20 from the posterior,
#has an predicted mean height at a weight =50 kg 
mu_at_50 <- post$a + post$b * 50
mu_at_50

#the variation across the those means incorporate the uncertainty in the
#between both alpha and beta

#plot the histogram/density of the vector of 20 posterior means at weight=50kg
dens( mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50" )


#get the 89% highest posterior density interval at 50kg
HPDI(mu_at_50, prob=0.89)

#1. sample the posterior distribution/model
#2. Compute predicted height for each weight in the data (weight = 30kg to 50kg)
#3. and randomly sample from the predicted heights
mu <- link(m4.3)

#1000 samples of regression lines of predicted heights
#352 columns of predicted heights, one for each weight
str(mu)

#define sequence of weights to compute predictions for
#these values will be on the horizontal axis
weight.seq <- seq( from=25, to=70 , by=1)

#use link to compute the predicted height
#for each sample regression line(alpha, beta) from the posterior
#and for each weight in weight.seq
mu <- link(m4.3, data=data.frame(weight=weight.seq) )
str(mu)

# use type"n" to hide raw data
plot( height ~ weight, d2, type="n" )
	

#loop over samples and plot each mu value
for (i in 1:100)
	#for each alpha, beta sampled from the posterior,
	#plot the weights and predicted heights
	points(weight.seq, mu[i,],pch=16, col=col.alpha(rangi2,0.1) )

#summarize the distribution of mu (alpha, beta)
mu.mean <- apply(mu, 2, mean)
mu.mean
mu.HPDI <- apply( mu, 2, HPDI, prob=0.89 )

#plot raw data
#fading out points to make line and interval more visible
plot( height ~ weight , data=d2, col=col.alpha(rangi2, 0.5) )

#plot the MAP line, aka the mean mu for each weight
#map line is the average model, alpha-beta pair or the mu of each weight.
lines( weight.seq, mu.mean)

#plot a shaded region for 89% HPDI of the mu
#"89% of mu values fall within this interval"
shade( mu.HPDI, weight.seq)

#create a vector 46 weights between 25 to 70
weight.seq <- seq( from=25, to=70 , by=1)

#1. Sample 1000 mu values (alpha, beta) pairs from the posterior
#2. for each mu, randomly sample a sigma
#3. calculate predicted heights for each weight 
sim.height <- sim( m4.3, data=list(weight=weight.seq) )

#for each 46 weight calculate the 90% percentile interval, from the 1000 posterior samples
height.PI <- apply( sim.height, 2, PI, prob=0.89 )
height.PI


#plot the raw data
plot(height ~ weight , d2, col=col.alpha(rangi2,0.5) )

#draw map line 
#(most probable relationship between height and weight in the posterior) 
lines( weight.seq, mu.mean)

#draw HPDI region for line 
#(not including sigma, 89% of average heights fall in this interval)
shade(mu.HPDI, weight.seq)

#draw PI region for simulated heights 
#(including sigma 89% of predicted heights fall inside this interval)
shade(height.PI, weight.seq)


#resimulate predicted heights with 10000 instead of 1000 samples 
#replot the function, there should be less jaggedness in the prediction interval.
sim.height <- sim( m4.3, data=list(weight=weight.seq), n=1e4 )
height.PI <- apply(sim.height , 2, PI, prob=0.89 )

#plot the raw data
plot(height ~ weight , d2, col=col.alpha(rangi2,0.5) )

#draw map line 
#(most probable relationship between height and weight in the posterior) 
lines( weight.seq, mu.mean)

#draw HPDI region for line 
#(not including sigma, 89% of average heights fall in this interval)
shade(mu.HPDI, weight.seq)

#draw PI region for simulated heights 
#(including sigma 89% of predicted heights fall inside this interval)
shade(height.PI, weight.seq)

#polynomial regression
library(rethinking)
data(Howell1)
d<- Howell1
str(d)

#we have a curvlinear relationship amongst height & weight
plot(height ~ weight, data=d)

#standardizing weight
d$weight.s <-  (d$weight - mean(d$weight) ) /sd(d$weight)
plot(height ~ weight.s, data=d)

#build polynomial regression model
d$weight.s2 <- d$weight.s^2

m4.5 <-  map(
		alist(
			height ~ dnorm(mu, sigma),
			mu <- alpha + beta*weight.s + beta2*weight.s2,
			alpha ~ dnorm(178, 100),
			beta ~ dnorm(0, 10),
			beta2 ~ dnorm(0,10),
			sigma ~ dunif(0,50)
			), data=d )
precis(m4.5)

weight.seq <- seq(from =-2, to=2, length.out=30)

#create linear term and square term from the sequence of weights
pred_dat <- list(weight.s = weight.seq, weight.s2 =weight.seq^2)

#fit the model to the sequence of weights 
mu <- link(m4.5, data=pred_dat)

#calculate the MAP line (most probable posterior line relating weight to height)
mu.mean <- apply(mu,2, mean)

#calculate the uncertainty on the average heights
#taking into account of uncertainty from the alpha and beta of the sampled posterior 
mu.PI <- apply(mu,2 ,PI, prob=0.89)

#for the simulated data, sample predicted heights from a normal distribution
#using the posterior mu and posterior sigma
sim.height <- sim(m4.5, data=pred_dat)

#create a prediction interval for predicted heights
height.PI <-apply(sim.height, 2, PI, prob=0.89)

#plot the curves
plot( height ~ weight.s, data=d,col=col.alpha(rangi2,0.5))
lines(weight.seq,mu.mean)
shade(mu.PI, weight.seq) #uncertainty of average heights (without inclusion of parameter sigma)
shade(height.PI, weight.seq) #uncertainty on predicted heights (with inclusion of parameter sigma)

#plot with natural units of weights
plot( height ~ weight.s , d , col=col.alpha(rangi2,0.5) , xaxt="n" ) #turn off the horizontal axis

at <- c(-2,-1,0,1,2) #create a sequence of z scores
labels <- at*sd(d$weight) + mean(d$weight) #transform z_scores to natural scale of weight
axis( side=1 , at=at , labels=round(labels,1) ) #reconstruct the x axis
