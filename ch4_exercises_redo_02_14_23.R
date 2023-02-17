#4E1.In the model definition below, which line is the likelihood?

#the likelihood function is yi ~ Normal*mu, sigma)
#mu ~ Normal(0, 10) & sigma ~Uniform(0, 10) are both priors that initialize the likelihood.

#4E2. In the model definition just above, how many parameters are in the posterior distribution?

#in the moel definition, there are 2 parameters in the posterior distribution, mu & sigma.

#4E3. Using the model definition above, write down the appropriate form of Bayes’ theorem that
includes the proper likelihood and priors.

#Pr((mu, sigma) | y) = Normal(yi| mu,sigma)*Normal(mu |178,20)*uniform(sigma,0, 50)/ SUM( Normal(yi| mu,sigma)*Normal(mu |178,20)*uniform(sigma,0, 50) )

#4E4.. In the model definition below, which line is the linear model?

#mu_i = a+ b*x_i is the linear model.

#4E5. In the model definition just above, how many parameters are in the posterior distribution?

# 2 parameters, mu & sigma are in the posterior distribution

#4M1. For the model definition below, simulate observed heights from the prior (not the posterior)
sample_mu <- rnorm( 1e4 , 0 , 10 )
sample_sigma <- runif( 1e4 , 0 , 10 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma ) #simulate random heights from sampling the mu & sigma

#4M2. Translate the model just above into a map formula
sim_model <- map(
	alist( y_i ~ dnorm(mu, sigma),
		 mu ~ dnorm(0,10),
		 sigma ~ dunif(0,10)
		), data=d)

#4M3.Translate the map model formula below into a mathematical model definition.

#	y ~ Normal(mu_i, sigma),
#	mu_i <- a + b*x_i,
#	a ~ Normal(0, 50),
#	b ~ Uniform(0, 10),
#	sigma ~ Uniform(0,50)

#4M4. A sample of students is measured for height each year for 3 years. After the third year, you want
to fit a linear regression predicting height using year as a predictor. Write down the mathematical
model definition for this regression, using any variable names and priors you choose. Be prepared to
defend your choice of priors

h_i ~ Normal( mu_i, sigma)
mu_i <- a + b*x_i
#the population of students from kindergarten to highschool 
#have an average height in the following range (112, 173) 3 ft 8 to 5ft 8, average=143
#a ~ Normal(143,30) 143 cm +/- 30 cm, 95% of heights of students range from 83cm to 203cm
#b ~ Normal(4,2), on average across all ages students under 18 grow at 4cm/year, (7cm/yr at 5years of age,  0.5cm/yr at 18 years of age) , 
#95% of rates fall within 0cm/yr to 8cm/yr.
#sigma(0, 50) #weak prior, 95% of predicted heights students can range from 43cm to 243cm, captures a wide range of student heights.

4M5.Now suppose I tell you that the average height in the first year was 120 cm and that every
student got taller each year. Does this information lead you to change your choice of priors? How?

#the average height on the first year = 120cm, translate to 4ft, which a height common for elementary aged students at age 6-7
#i can change the priors:
# a ~ Normal(130.5,10)  - captures the average height of a student at the third year growing at at rate of 5.5cm/yr starting at 120cm on the first year,
# 				reduce sigma to capture heights of elementary school aged students only 95% of heights fall within 110cm to 150cm.
#
#
# b ~ Normal(5.5, 0.5cm)		- elementary aged students have a growth rate of 5-6 cm /year on average, 95% of rates fall between 4.5cm/yr to 6.5cm/yr
#
#
#
#sigma ~ Uniform(0,15) - reduce sigma so 95% of predicted heights fall within +/- 30cm of the mean (101cm, 160cm)



#4M6.. Now suppose I tell you that the variance among heights for students of the same age is never
more than 64cm. How does this lead you to revise your priors?

#variance is the square of sigma, so sigma never more than 8
#i can change the prior for sigma: sigma~Uniform(0, 8), 
#for the predicted heights, 95% of predicted heights will fall within 16cm of the mean.
#Since the prediction interval is larger than the uncertainty on the average, the sigma on the alpha prior can be adjusted, however I am not confident in the adjustment. 


#4H1. Provide predicted heights and prediction, HPDI/PI, intervals for the following weights:

#load data
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

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

weight.seq<- c(46.95,43.72, 64.78, 32.59, 54.63)

#calculating the mu of the model
post <- extract.samples(m4.3)					#Sample distribution of mu from the posterior (a,b) pairs

mu.link <- function(weight) post$a + post$b*weight	#Apply posterior to  weights get distribution of average heights
mu <- sapply( weight.seq , mu.link )

mu.mean <- apply( mu , 2 , mean )				#calculate the highest probable height for each weight 
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )		#calculate the uncertainty for average heights 


#predicting heights (including uncertainty sigma in the model
sim.height <- sim( m4.3 , data=list(weight=weight.seq) ) #sample mu from the posterior & sample sigma from a uniform distribution, compute heights for each weight
height.PI <- apply( sim.height , 2 , PI , prob=0.89 ) #compute the 89% prediction interval of heights for each weight
height.PI

#mu.mean : 156.3684 153.4476 172.4917 143.3829 163.3132
#height.PI :5%  148.5714 146.0134 164.4049 135.6540 155.0116
#		94% 164.3153 161.8307 180.4352 151.4952 171.1598

#plotting the data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5), type='n') #hide the raw data

lines(weight.seq, mu.mean)					   #drawing the MAP line

shade(mu.HPDI, weight.seq)					   #draw HPDI region for line

shade( height.PI, weight.seq)					   #draw PI region for simulated heights


#4H2 Select out all the rows in the Howell1 data with ages below 18 years of age. If you do it right,
#you should end up with a new data frame with 192 rows in it.

library(rethinking)
data(Howell1)
d<- Howell1

#select all students younger than 18 years of age
d2 <- d[d$age <18,]
str(d2)
mean(d2$height)
sd(d2$height)


#(a) Fit a linear regression to these data, using map. Present and interpret the estimates. For every
#10 units of increase in weight, how much taller does the model predict a child gets?
m1 <- map(
		alist(
			height ~ dnorm(mu, sigma),
			mu <- a + b*weight,
			a <- dnorm( mean(height), sd(height) ), #start with the mean height and sd of height, alpha prior should always start at the mean
			b <- dnorm(1,0.5),			    #flat prior - 95% of growth rates fall within 0cm/lb to 2cm/lb for persons < 18 years of age
			sigma ~ dunif(0, 30)			    #flat prior encompass 95% of heights fall within 40cm to 170cm
			), data=d2
		)

precis(m1)
#there is high certainty the average growth rate, b, is 2.7cm/lb with 90% of growth rates falling between, 2.61 & 2.82 cm.
#there high certainty 95% of predicted heights fall within +/- 17cm of the mean.

#For every 10 unit increase in weight, how much taller does the model predict a child gets?
2.7cm/lb* 10lb = 27cm for every 10 unit increase in weight

#(b) plot the raw data with height on the vertical axis and weight on the horizontal axis.
#superimpose the map regression line & 89% HPDI for the mean. Also superimpose the 89% HPDI for predicted means.

plot( height ~ weight, data=d2)



#plot the MAP line of the model (most probable mu)
#abline(a=coef(m1)["a"], b=coef(m1)["b"] )

#plot the uncertainty around the mean (HPDI of the mean)

weight.seq <- seq(min(d2$weight),max(d2$weight),1)

#sample from the posterior, compute mu for each posterior sample using each weight in weight.seq
#mu <- link(m1,data=data.frame(weight=weight.seq)) 

post<- extract.samples(m1) 					#sample 10000 posterior samples from the joint distribution of (a,b,sigma)
mu.link <- function(weight) post$a + post$b*weight	#function that computes a distribution of heights for each weights
mu <- sapply(weight.seq,mu.link)	

#summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.mean
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89 )
mu.HPDI

#plot the MAP line, for each weight
lines(weight.seq, mu.mean)

#plot a shaded region for 89% HPDI
shade(mu.HPDI, weight.seq)

#plot 89% prediction interval of weights
#sim.height <- sim(m1, data=list(weight=weight.seq) )
#str(sim.height)

post <-extract.samples(m1)		#sample 10000 posterior samples of parameters, a,b,sigma from the joint distribution (a,b,sigma)

sim.height <- sapply(weight.seq, function(weight)
								#sample from a gaussian distribution with 10000 posterior samples of mu & sigma for each weight	
								rnorm(					
									nrow(post),				#10000 rows
									mean=post$a + post$b*weight,	#compute a distribution (10000 rows) of average height for each weight
									sd = post$sigma			
									)	
			)

#summarize the PI for each weight value
height.PI <-apply(sim.height,2, PI, prob=0.89,n=1e4)
height.PI

shade(height.PI, weight.seq)