#4E.1
#yi = Norma( mu, sigma) is the likelihood. with priors for mu and sigma.

#4E.2
#There are 2 variables in the posterior distribution, mu & sigma.

#4E.3
p(y | mu & sigma) = p(mu & sigma | y)*p(y) / p(mu & sigma)

#4E.4
#mu = alpha + beta*x

#4E.5
#3 variables are in the linear model: alpha, beta, sigma


#4M1. For the model definition below, simulate observed heights from the prior (not the posterior).

#--------------example review of bayes theorem.-------------
w<- 6; n<- 9;
p_grid <- seq( from = 0, to = 1, length.out=100)

#posterior = binomial siareivurion with w successes in n trials
#prior = sigma uniform between 0 and 1
#normalize the posterior
posterior <- dbinom( w, n, p_grid) *dunif(p_grid, 0 ,1)
posterior <- posterior/sum(posterior)

#-----------------------------------------------------------
library(rethinking)
sample_mu <- rnorm(1e4, 0, 10)
sample_sigma <- runif(1e4, 0, 10)
#generate 10000 random sample points averaged over the priors from mu and sigma
prior_h <- rnorm( 1e4, sample_mu, sample_sigma)
dens(prior_h)

#4M2.  Translate the model just above into a map formula.

flist <- alist(
	height ~ dnorm(mu, sigma),
	mu ~ dnorm( 0,10),
	sigma ~ dunif( 0,10)
)

#m4.1 <- map( flist, data)


#4M.3 Translate the map model formula below into a mathematical model definition

#
# y ~ Normal(mu, sigma)
# mu = alpha + beta*x
# alpha ~ Normal(0,50)
# beta ~ Normal(0, 10)
# sigma ~ Uniform(0, 50)

#4M.4 8.87, 101/483 model fitting with MAP
#
#height_i ~ N( mu_i, sigma)
#
#mu_i = alpha + beta*year_i #year for each student
#
#alpha ~ N(150, 25) #specify students without regards to age use a weak prior, captures students from (100cm to 200cm or 2 stdev away)
#
#beta ~ N(4,2) 
#there is no mention of growth rate, therefore use a weak prior to capture yearly growth
#the range within 2 stdev is (0cm/yr to 8cm/yr)
#
#sigma ~ Uniform(0,50)

#4M.5 average height in the first year was 120cm , students got taller each year

#If the average height in the first year was 120cm, 
#I will change the prior for alpha ~ Normal(120,10) so the MAP will start at 120.
#This makes me more confident we are talking about elementary school students.
#Thus I can narrow the range of my prior distributions to account for heights and growth rates of elementary students instead
alpha ~ N( 120,10 ), #school age students fall between (100cm to 140cm 2 stdev)
beta ~ N( 7,1) #the growth rate of school aged students are higher, therefore we center at an average of 7cm/year
sigma ~ uniform(0,20) #reduce the sigma of the distribution as a higher sdev is less likely.

#4M6: The variance among heights for students of the same age is never more than 64cn. 
#How does this lead you to revise your priors?
#
#adjust the prior for  sigma ~ uniform(0, 8)

#4H1. 
#The weights listed below were recorded in the !Kung census, but heights were not recorded for
#these individuals. Provide predicted heights and 89% intervals (either HPDI or PI) for each of these
#individuals. That is, fill in the table below, using model-based predictions

#load the data
library(rethinking)
data(Howell1)
d <- Howell1
str(d)

#filter for adults
d2 <- d[ d$age >= 18, ]

#fit the height model to the kung data
m4.3 <- map( 
	alist(
		height ~  dnorm(mu, sigma),
		mu <- a + b*weight,
		a ~ dnorm(156,100),
		b ~ dnorm(0,10),
		sigma ~ dunif(0,50)
	), data=d) 

#create a vector of weights
v = c( 46.95, 43.72,64.78, 32.59, 54.63)

#create a df of weights
weight_df = data.frame(weight = v)

#for each sample(alpha, beta, sigma) from the posterior, calculate the distribution of heights for the given weights
mu <- link(m4.3, data=weight_df)
str(mu)

#summarize the distribution of mu for the 6 weights
mu.mean <- apply(mu, 2, mean)
mu.mean

#summarize the distribution of HPDI for the 6 weights
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
mu.HPDI

data.frame( 
	individual = 1:5,
	weight = v,
	expected = mu.mean,
	lower = mu.HPDI[1,],
	upper = mu.HPDI[2,]
	)

#4H2

#filter for kids below 18 years of age
d2 <- d[ d$age < 18, ]
str(d2)

#a. a) Fit a linear regression to these data, using map. Present and interpret the estimates. For every
#10 units of increase in weight, how much taller does the model predict a child gets?

#fit the height model to the kung data
m4.4 <- map( 
	alist(
		height ~  dnorm(mu, sigma),
		mu <- a + b*weight,
		a ~ dnorm(156,100),
		b ~ dnorm(0,10),
		sigma ~ dunif(0,50)
	), data=d2) 

precis(m4.4)
#Beta = 2.72cm/kg, for every 1 unit increase is weight,
#a child grows 27.2cm for every 10 units increase in weight.


#b. b) Plot the raw data, with height on the vertical axis and weight on the horizontal axis. 
#Superimpose the MAP regression line and 89% HPDI for the mean. Also superimpose the 89% HPDI for
#predicted heights.

# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=min(d2$weight) , to=max(d2$weight) , by=1 )


#for each sample(alpha, beta, sigma) from the posterior, calculate the distribution of heights for the weights used in the model
mu <- link(m4.4, data= data.frame(weight=weight.seq) )

#create the MAP line
mu.mean <- apply(mu, 2, mean)
mu.mean

#summarize the distribution of HPDI for the model
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
mu.HPDI


#plot the raw data of the sample of persons < 18 years of age.
plot( d2$height ~ d2$weight, col=col.alpha(rangi2,0.5) )

#plot the map line of the model
abline(a=coef(m4.4)["a"], b=coef(m4.4)["b"] )
lines( weight.seq, mu.mean)

#plot the 89% HPDI for the mean
shade(mu.HPDI, weight.seq)

#plot the 89% HPDI prediction interval that incorporates uncertainty
#
#
#
sim.height <- sim(m4.4, data= list(weight =weight.seq) )
str(sim.height)

#summarize the simulated heights 89% posterior prediction interval
height.PI <- apply(sim.height, 2, PI, prob=0.89)

#draw the 89% PI region for the simulated heights
shade ( height.PI, weight.seq)

#(c) What aspects of the model fit concern you? Describe the kinds of assumptions you would
#change, if any, to improve the model. You don’t have to write any new code. Just explain what the
#model appears to be doing a bad job of, and what you hypothesize would be a better model

#The model appears to be non linear, weight appears to be a poor predictor of height for humans < 18 years of age.
#It overestimates height <10 / > 30, and underestimates height between (10 and 30). 
#I would use a polynomial regression model instead of a linear model.

#4H3
#(a) Model the relationship between height (cm) and the natural logarithm of weight (log-kg). Use
#the entire Howell1 data frame, all 544 rows, adults and non-adults. Fit this model, using quadratic
#approximation:

#verify all rows are used for the Howell dataset (adults and non adults)
str(d)

#fit the new model with Quadratic approximation
m4.5 <- map( 
	alist(
		height ~  dnorm(mu, sigma),
		mu <- a + b*log(weight),
		a ~ dnorm(178,100),
		b ~ dnorm(0,100),
		sigma ~ dunif(0,50)
	), data=d) 

#interpret summary of fit
precis(m4.5)

#(b) plot the raw data data
#use samples from the quadratic approximate posterior of the model in (a) to superimpose on
#the plot: (1) the predicted mean height as a function of weight, (2) the 97% HPDI for the mean, and
#(3) the 97% HPDI for predicted heights

plot( height ~ weight , data=d ,
col=col.alpha(rangi2,0.4) )

# define sequence of weights to compute predictions
weight.seq <- seq( from=min(d$weight) , to=max(d$weight) , by=1 )

#for each sample(alpha, beta, sigma) from the posterior, calculate the distribution of heights for the weights used in the model
mu <- link(m4.5, data= data.frame(weight=weight.seq) )

#create the MAP line
mu.mean <- apply(mu, 2, mean)

#summarize the distribution of means using the HPDI for the model
mu.HPDI <- apply(mu, 2, HPDI, prob=0.97)

#plot the map line of the model
lines( weight.seq, mu.mean)

#plot the 97% HPDI for the mean
shade(mu.HPDI, weight.seq)

#simulate heights for the model
sim.height <- sim(m4.5, data= list(weight =weight.seq) )

#summarize the simulated heights 97% posterior prediction interval
height.PI <- apply(sim.height, 2, HDPI, prob=0.97)

#draw the 89% PI region for the simulated heights
shade ( height.PI, weight.seq)


