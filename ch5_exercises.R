#5E1. 
#2, 3, 4 are linear models of multiple linear regression because they contain 2 variables.

#5E2. Write down a multiple regression to evaluate the claim: 
Animal diversity is linearly related to
latitude, but only after controlling for plant diversity. 
You just need to write down the model definition.

#page 125, 139/483

#animal_diversity ~ Normal(mu, sigma)
#mu <- alpha + beta_latitude*latitude + beta_plant_diversity* plant_diversity
#beta_latitude = Normal(0,1)
#beta_plant_diversity = Normal(0,1)
alpha ~ Normal(10,10)
sigma ~ uniform(0,10)


#5E3. Write down a multiple regression to evaluate the claim: 
#Neither amount of funding nor size
#of laboratory is by itself a good predictor of time to PhD degree; 
#but together these variables are both
#positively associated with time to degree. 
#Write down the model definition and indicate which side of
#zero each slope parameter should be on

time_to_receive_phd ~ Normal(mu, sigma)
mu <- alpha + beta_amount_of_funding*amount_of_funding + beta_size_of_lab*size_of_lab
beta_amount_of_funding ~ Normal(0,1)
beta_size_of_lab ~ Normal(0,1)
alpha ~ Normal(10, 10)
sigma ~ uniform(0, 10)

beta_size_of_lab & beta_amount_of_funding is positively associated with time of degree. (+)

#5E4. 3,4, and 5 are equivalent multiple regression models.

#5M1. Invent your own example of a spurious correlation. An outcome variable should be correlated
#with both predictor variables. But when both predictors are entered in the same model, the correlation
#between the outcome and one of the predictors should mostly vanish (or at least be greatly reduced).

library(rethinking)
N <- 100 # number of cases
x_real <- rnorm( N ) # x_real as Gaussian with mean 0 and stddev 1
x_spur <- rnorm( N , x_real ) # x_spur as Gaussian with mean=x_real
y <- rnorm( N , x_real ) # y as Gaussian with mean=x_real
d <- data.frame(y,x_real,x_spur) # bind all together in data frame

#fit a model without xreal, xspur is correlated xreal
mspur  <- map(
			alist(
				y ~ dnorm(mu, sigma),
				mu <- a + bspur*x_spur,
				a ~ dnorm(10, 10),
				bspur ~ dnorm(10,10),
				sigma ~ dunif(0,10)
				), data =d )
#review bspur
precis(mspur)

#fit a model with xreal and xspur
mspur  <- map(
			alist(
				y ~ dnorm(mu, sigma),
				mu <- a + breal*x_real + bspur*x_spur,
				a ~ dnorm(10, 10),
				breal ~ dnorm(0,10),
				bspur ~ dnorm(10,10),
				sigma ~ dunif(0,10)
				), data =d )

#the mean of bspur has been reduced by 1/3
precis(mspur)

#5M2.
#Invent your own example of a masked relationship. An outcome variable should be correlated
#with both predictor variables, but in opposite directions. And the two predictor variables should be
#correlated with one another.

N <- 100 # number of cases
rho <- 0.7 # correlation btw x_pos and x_neg
x_pos <- rnorm( N ) # x_pos as Gaussian
x_neg <- rnorm( N , rho*x_pos , # x_neg correlated with x_pos
sqrt(1-rho^2) )
y <- rnorm( N , x_pos - x_neg ) # y equally associated with x_pos, x_neg
d <- data.frame(y,x_pos,x_neg) # bind all together in data frame

#x_pos and x_neg are positively correlated
#x_pos is positively correlated with y
#x_neg is negatively correlated with y.
pairs(d)

#fit a model with x_pos only, model is missing x_neg
mpos  <- map(
			alist(
				y ~ dnorm(mu, sigma),
				mu <- a + bpos*x_pos,
				a ~ dnorm(10, 10),
				bpos ~ dnorm(0,10),
				sigma ~ dunif(0,10)
				), data =d )

#the mean of bspur has been reduced by 1/3
precis(mpos)


#fit a model with xpos and xneg
m_pos_neg  <- map(
			alist(
				y ~ dnorm(mu, sigma),
				mu <- a + bpos*x_pos + bneg*x_neg,
				a ~ dnorm(10, 10),
				bpos ~ dnorm(0,10),
				bneg ~ dnorm(0,10),
				sigma ~ dunif(0,10)
				), data =d )

#with the addition of x_neg, bpos increased x4 and sigma is decreased 
#(x_pos and x_neg are both associated to the outcome)
precis(m_pos_neg)

#5M3

More divorces does not cause a higher marriage rate, however is an indicator of more marriages.
However, divorces does not cause a higher marriage rate.

How might a high divorce rate cause a higher marriage rate?

- marriage rate can be tested with the relationship of the following predictors 
in a multiple regression: divorce rate, and the rate of more than 1 marriage.

If there is a strong association between the rate of more than 1 marriage and marriage rate,
while divorce rate is weakly associated to marriage rate, this supports the hypothesis.


#5M4.

#read in the lds data
setwd("C:/Users/Alan/Documents/Statistical_Rethinking")
data <- read.csv('5M4_exercise.csv')
str(data)

#calculate the percent of mormon
data$percent_mormon <- data$Estimated.Mormon.Population / data$Total.State.Population 
str(data)
#standardize mormon percent
data$standardpop <- (data$percent_mormon-mean(data$percent_mormon))/sd(data$percent_mormon)

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
str(d)

# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)

#compute inner join
df = merge(x=d, y= data, by.x="Location", by.y="X.State")
str(df)

m_lds <- map(
		alist(
			Divorce ~ dnorm( mu , sigma ) ,
			mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s + bsp*standardpop,
			a ~ dnorm( 10 , 10 ) ,
			bR ~ dnorm( 0 , 1 ) ,
			bA ~ dnorm( 0 , 1 ) ,
			bsp ~ dnorm( 0, 1 ),
			sigma ~ dunif( 0 , 10 )
			) ,
			data = df )

#adding the mormon population to the model, the median age of marriage is more negatively associated with divorce rate
#marriage rate remains highly variable because the interval contains 0. 
#sigma becomes smaller
#percent of mormon population  is negatively correlated with divorce rate (mormons are less likely to divorce)
#Thus states with larger median age of marriage or larger mormon population has a lower divorce rate.
precis(m_lds)


#5M5.
multiple regression for 2 mechanisms

obesity rate ~ dnorn(mu, sigma)
mu <- alpha + b_execise*amount_of_exercise + b_consumption_of_ff*amount_of_fast_food
b_exercise ~ dnorm(0,1),
b_consumption_of_ff ~ dnorm(0,1),
alpha ~ dnorm(0,1)
sigma~ dunif(10,10)

#5H1. Fit two bivariate Gaussian regressions, using map: 
#(1) body weight as a linear function of territory size (area), 
#(2) body weight as a linear function of groupsize. 
#(3) Plot the results of these regressions, displaying the MAP regression line and the 95% interval of the mean. 
#(4) Is either variable important for predicting fox body weight?

library(rethinking)

#load data
data(foxes)
d <- foxes
str(d)

#bodyweight is normally distributed therefore we will not need to transform the data
simplehist(d$weight)

#territory size is approximately normally distributed therefore we will not need to transform the data
simplehist(d$area)

b1 <- map( 	
		alist(
			weight ~ dnorm(mu, sigma),
			mu <- alpha + ba*area,
			alpha ~ dnorm(0 ,10),
			ba ~ dnorm(0,1),
			sigma ~ dunif(0, 10)
			
		), data=d)

#The territory size of a fox is weakly associated and highly variable in relation to the body weight of a fox, since the interval contains 0
precis(b1)

#groupsize is approximately normal no need for transformation
simplehist(d$groupsize)

# plot raw data
plot( weight ~ area , data=d , col=rangi2 )

#plot the MAP regression line
abline( b1 )


#plot the 95% prediction interval of the mean
area_size <- seq( from=min(d$area) , to=max(d$area) , length.out=30 )

#apply the model to the weights vector
mu <- link( b1 , data=data.frame(area=area_size) )

#compute the PI for the processed weights
mu.PI <- apply( mu , 2 , PI )

#draw the weights interval on the plot.
shade( mu.PI , area_size )


b2 <- map( 	
		alist(
			weight ~ dnorm(mu, sigma),
			mu <- alpha + bgs*groupsize,
			alpha ~ dnorm(0 ,10),
			bgs ~ dnorm(0,1),
			sigma ~ dunif(0, 10)
			
		), data=d)


#The group size of a fox is negatively weakly associated to the body weight of the fox pact. 
precis(b2)

# plot raw data
plot( weight ~ groupsize , data=d , col=rangi2 )

#plot the MAP regression line
abline( b2 )

#plot the 95% prediction interval of the mean
gs <- seq( from=min(d$groupsize) , to=max(d$groupsize) , length.out=30 )

#apply the model to the groupsize vector
mu <- link( b2 , data=data.frame(groupsize=gs) )

#compute the PI for the processed weights
mu.PI <- apply( mu , 2 , PI )

#draw the weights interval on the plot.
shade( mu.PI , gs )


#Groupsize is positively weakly associated to predict body weight of the fox in the bivariate model
#Area size is negatively weakly associated to predict body weight and highly varialbe in the bivariate model.

#group size should be included in the model, but area size would not add value to the model, since the relationship to
#weight is highly variable.

#5H2
# (1) Now fit a multiple linear regression with weight as the outcome and both area and groupsize as predictor variables. 
# (2) Plot the predictions of the model for each predictor, holding the other predictor constant at its mean. 
# (3) What does this model say about the importance of each variable? 
# (4) Why do you get different results than you got in the exercise just above?

#(1) test to see if group size or area is a good predictor of body weight
b3 <- map( 	
		alist(
			weight ~ dnorm(mu, sigma),
			mu <- alpha + bgs*groupsize + ba*area,
			alpha ~ dnorm(0 ,10),
			bgs ~ dnorm(0,1),
			ba ~ dnorm(0,1),
			sigma ~ dunif(0, 10)
			
		), data=d)

precis(b3)
#In the multiple regression model, group size is negatively associated to body weight whe we already know the area size.
#While area size is positively associated to body weight when knowing the group size. 
#Both predictors should be included in the model.


#(2) Plot the predictions of the model for each predictor, holding the other predictor constant at its mean. 
#(counterfactual plot)

#page 129, 143/483

#create a subplot of `x2
par(mfrow = c(1, 2))


#prepare new counterfactual data (hold area constant but vary group size)

area.avg <- mean( d$area)#hold area constant

groupsize.seq <- seq( from=min(d$groupsize) , #vary group size
				 to=max(d$groupsize) ,
					 length.out=30 )
pred.data <- data.frame(
				groupsize=groupsize.seq,
				area =area.avg
				)
#apply model to data to create a new MAP line
mu <- link( b3 , data=pred.data )
mu.mean <- apply( mu , 2 , mean ) #calculate the mean on the model
mu.PI <- apply( mu , 2 , PI ) #calculate the PI interval for the mean


# simulate counterfactual  outcomes
groupsize.sim <- sim( b3 , data=pred.data , n=1e4 )

groupsize.PI <- apply( groupsize.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( weight ~ groupsize , data=d )
mtext( "area = constant" ) # Add title to plot
lines( groupsize.seq , mu.mean )
shade( mu.PI , groupsize.seq )
shade( groupsize.PI , groupsize.seq )


# prepare new counterfactual data (hold group size constant but vary area)

groupsize.avg <- mean( d$groupsize)#hold groupsize constant

area.seq <- seq( from=min(d$area) , #vary area size
				 to=max(d$area) ,
					 length.out=30 )
pred.data <- data.frame(
				groupsize=groupsize.avg,
				area =area.seq
				)

#apply model to data to create a new MAP line
mu <- link( b3 , data=pred.data )


mu.mean <- apply( mu , 2 , mean ) #calculate the mean on the model
mu.PI <- apply( mu , 2 , PI ) #calculate the PI interval for the mean


# simulate counterfactual area outcomes
area.sim <- sim( b3 , data=pred.data , n=1e4 )

area.PI <- apply( area.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( weight ~ area , data=d )
mtext( "groupsize = constant" ) # Add title to plot
lines( area.seq , mu.mean )
shade( mu.PI , area.seq )
shade( area.PI , area.seq )


#(3) The model makes the following statements:
#When holding area constant, groupsize and bodyweight have a negative relationship.
#This could be due to the fact, a larger group size means more sharing of resources amongst the group.
#
#When holding groupsize constant, area and bodyweight have a positive relationship.
#This could be due to the fact, a larger area means more resources for the fox amongst the group

#(4) Area and groupsize are positively correlated with one another

test <- data.frame(d$area, d$groupsize, d$weight)
str(test)
pairs(test)

#The relationship amongst weight, area, and groupsize is an example of a masked relationship.
#Where predictors, area and groupsize, are positively correlated with each other. 
#The positive and negative relationship cancel each other out.
#However, area and weight are positively correlated, but groupsize and weight are negatively correlated.


#5H3.
Finally, consider the avgfood variable. 
Fit two more multiple regressions: 
(1) body weight as an additive function of avgfood and groupsize, 
(2) body weight as an additive function of all three variables, avgfood and groupsize and area. 
(3) Compare the results of these models to the previous models you’ve fit, in the first two exercises. 
(a) Is avgfood or area a better predictor of body weight? 
If you had to choose one or the other to include in a model, which would it be? Support your assessment with any tables or plots you choose. 
(b) When both avgfood or area are in the same model, their effects are reduced (closer to zero) and their standard errors are larger than when they
are included in separate models. Can you explain this result

library(rethinking)

#load data
data(foxes)
d <- foxes
str(d)

#(1)
#avgfood is approximately normal, no need to log transform
dens(d$avgfood)

#create a model for weight using the predictors avgfood and groupsize
b4 <- map(
		alist(
			weight ~ dnorm(mu, sigma),
			mu <- alpha + b_avgfood*avgfood + bgs*groupsize,
			alpha ~ dnorm(0,max(d$weight)), #flat prior
			b_avgfood ~ dnorm(0,max(d$weight)),
			bgs ~ dnorm(0,max(d$weight)),
			sigma ~ dunif(0,10)
			), data=d)

#holding groupsize constant, average food is strongly positively correlated with body weight
#more food consumption = more body weight
#holding food consumption constant, group size is negateively correlated with body weight.
#larger group sizes means more sharing of resources which results to smaller body weights.
precis(b4)

#identify pairwise relationships
dat1 <- data.frame(d$weight, d$avgfood, d$groupsize)

#avgfood is positively associated to groupsize, this may be an example of multi collinearity. 
pairs(dat1)


#(2)
#create a model for weight using the predictors avgfood, area, and groupsize
b5 <- map(
		alist(
			weight ~ dnorm(mu, sigma),
			mu <- alpha + b_avgfood*avgfood + bgs*groupsize + ba*area,
			alpha ~ dnorm(0,max(d$weight)), #flat prior
			b_avgfood ~ dnorm(0,max(d$weight)),
			bgs ~ dnorm(0,max(d$weight)),
			ba ~ dnorm(0,max(d$weight)),
			sigma ~ dunif(0,max(d$weight))
			), data=d)

precis(b5)

#(3)
#avg_food is highly variable, the distribution of slopes contains 0, with the introduction of the variable area into the model.
#groupsize is more negatively associated with body weight
#However, there is approximately 0 change to the distribution of slopes for area with the introduction of avg food.
#


#(a)
#avgfood or area is a good predictor of body weight, but should not be included in the same model. 
#both variables are positively correlated and including both would add more variability into the model.

#view pairs plot between area and avgfood
dat1 <- data.frame(d$weight, d$area, d$avgfood)
pairs(dat1)

#To identify the better predictor for body weight, 
#compare the magnitude of fits of body weights amongst inclusion of standardized avgfood and area predictors.

d$avgfood.s <- ( d$avgfood - mean(d$avgfood) ) /sd(d$avgfood)

b6 <- map(
		alist(
			weight ~ dnorm(mu, sigma),
			mu <- alpha + b_avgfood*avgfood.s + bgs*groupsize,
			alpha ~ dnorm(0,max(d$weight)), #flat prior
			b_avgfood ~ dnorm(0,max(d$weight)),
			bgs ~ dnorm(0,max(d$weight)),
			sigma ~ dunif(0,max(d$weight))
			), data=d)

precis(b6)

d$area.s <- ( d$area - mean(d$area) ) /sd(d$area)

b7 <- map(
		alist(
			weight ~ dnorm(mu, sigma),
			mu <- alpha + b_area*area.s + bgs*groupsize,
			alpha ~ dnorm(0,max(d$weight)), #flat prior
			b_area ~ dnorm(0,max(d$weight)),
			bgs ~ dnorm(0,max(d$weight)),
			sigma ~ dunif(0,max(d$weight))
			), data=d)

precis(b7)

#Comparing the magnitudes of fits, avgfood is more strongly correlated to bodyweight than area is correlated to bodyweight.
#I would include avgfood in the model to predict body weight. The same results can be visualized using the counterfactual plots
#of each variable individually plotted against groupsize. avgfood has highger correlation to bodyweight.


#plot the counterfactual plot for (groupsize, area) , and (groupsize avgfood)

#create a subplot of `x2
par(mfrow = c(1, 2))

#(a) prepare new counterfactual data (hold group size constant but vary area)

groupsize.avg <- mean( d$groupsize)#hold groupsize constant

area.seq <- seq( from=min(d$area) , #vary area size
				 to=max(d$area) ,
					 length.out=30 )
pred.data <- data.frame(
				groupsize=groupsize.avg,
				area =area.seq
				)

#apply model to data to create a new MAP line
mu <- link( b3 , data=pred.data )


mu.mean <- apply( mu , 2 , mean ) #calculate the mean on the model
mu.PI <- apply( mu , 2 , PI ) #calculate the PI interval for the mean


# simulate counterfactual area outcomes
area.sim <- sim( b3 , data=pred.data , n=1e4 )

area.PI <- apply( area.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( weight ~ area , data=d )
mtext( "groupsize = constant" ) # Add title to plot
lines( area.seq , mu.mean )
shade( mu.PI , area.seq )
shade( area.PI , area.seq )


#(b) prepare new counterfactual data (hold group size constant but vary area)

groupsize.avg <- mean( d$groupsize)#hold groupsize constant

avgfood.seq <- seq( from=min(d$avgfood) , #vary area size
				 to=max(d$avgfood) ,
					 length.out=30 )
pred.data <- data.frame(
				groupsize=groupsize.avg,
				avgfood =avgfood.seq
				)

#apply model to data to create a new MAP line
mu <- link( b4 , data=pred.data )


mu.mean <- apply( mu , 2 , mean ) #calculate the mean on the model
mu.PI <- apply( mu , 2 , PI ) #calculate the PI interval for the mean


# simulate counterfactual avgfood outcomes
avgfood.sim <- sim( b4 , data=pred.data , n=1e4 )

avgfood.PI <- apply( avgfood.sim , 2 , PI )

# display predictions, hiding raw data with type="n"
plot( weight ~ avgfood , data=d )
mtext( "groupsize = constant" ) # Add title to plot
lines( avgfood.seq , mu.mean )
shade( mu.PI , avgfood.seq )
shade( avgfood.PI , avgfood.seq )

#looking at the plots, the PI for the mean for avgfood contains more of the raw data points than the PI for the mean of area.
#There are more points that fall into the PI of the model with avgfood than the model with area.


#(b) avgfood and area are related to another an example of multicollinearity. Both variables are positively associated to groupsize.
#When including both variables in the model, the means for the slopes of avgfood and area are comingled and they never seperately influence the mean.