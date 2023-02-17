#load data
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

#standardize the predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) /
sd(d$MedianAgeMarriage)

#fit the model (Divorce Rate vs. Median Marriage age)
m5.1 <- map(
		alist(
			Divorce ~ dnorm(mu, sigma),
			mu <- a + bA * MedianAgeMarriage.s,
			a ~ dnorm( 10, 10) ,
			bA ~ dnorm(0,1),
			sigma ~ dunif(0, 10) 
			), data= d)

precis(m5.1)
#the rate of divorve falls by 1 year for a 1 unit increase in median marriage age.

#compute percentile interval of mean
MAM.seq <- seq( from=-3, to=3.5, length.out=30)
mu <- link(m5.1, data=data.frame(MedianAgeMarriage.s=MAM.seq) )



mu.PI <- apply(mu, 2, PI )

#plot it all
plot( Divorce ~ MedianAgeMarriage.s, data=d, col=rangi2 ) 
abline(m5.1)
shade(mu.PI, MAM.seq )

#Marriage rate vs. Divorce rate

#standardize marriage rate
d$Marriage.s <- (d$Marriage - mean(d$Marriage) ) / sd(d$Marriage)

m5.2 <- map(
		alist( 
			Divorce ~ dnorm(mu, sigma),
			mu <- a + bR * d$Marriage.s,
			a ~ dnorm( 10, 10),
			bR ~ dnorm( 0 , 1),
			sigma ~ dunif(0,10)
			), data=d )

precis(m5.2)



#fit the model to 2 parameters, marriage rate and median age of marriage
m5.3 <- map(
		alist(
			Divorce ~ dnorm(mu , sigma), 
			mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
			a ~ dnorm( 10, 10),
			bR ~ dnorm( 0 , 1),
			bA ~ dnorm( 0 , 1),
			sigma ~ dunif( 0 ,10)
			), data=d )
precis(m5.3)

#plot MAP value percentile intervals
plot( precis(m5.3))


#create predictor residual plot between marriage rate and median age of marriage
m5.4 <- map(
		alist(
	Marriage.s ~ dnorm(mu, sigma) , #outcome=marriage rate
	mu <- a + b*MedianAgeMarriage.s, #marriage rate is a function of median age of marriage
	
	#priors start at 0, because median age and marriage age are standardized.
	a~ dnorm( 0 ,10 ),
	b~ dnorm( 0 ,1 ),
	sigma ~ dunif( 0 , 10)
			), data= d)

#compute expected value/ MAP line of marriage rate for each state 
#as a function of median age of marriage
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s

#compute residual for each state
m.resid <- d$Marriage.s - mu

#plot raw data 
plot( Marriage.s ~ MedianAgeMarriage.s, d, col=rangi2 )

#plot the map line between marriage rate and median age of marriage
abline(m5.4 ) 

#plot distance from MAP line to point
#loop over states
for (i in 1:length(m.resid)) {
	x <- d$MedianAgeMarriage.s[i] # x location of line segment
	y <- d$Marriage.s[i] # observed endpoint of line segment

	#draw the line segment
	lines( c(x,x) , c(mu[i], y), lwd=0.5, col=col.alpha("black",0.7) )
}

#counterfactual plot
#
#prepare new counterfactual data
A.avg <- mean( d$MedianAgeMarriage.s)  #hold median age of marriage constant
R.seq <- seq(from = -3, to=3, length.out=30 ) #vary marriage rate
pred.data <- data.frame(
	Marriage.s= R.seq,
	MedianAgeMarriage.s=A.avg
)
str(pred.data)

#compute counterfactual mean divorce(mu)
#fit model to counter factual data to get predicted means
mu <- link(m5.3, data=pred.data )	
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

#simulate counterfactual divorce outcomes
R.sim <- sim(m5.3, data=pred.data, n=1e4)

#calculate poserior prediction interval for simulated data
R.PI <- apply(R.sim, 2, PI )

#display predictions, hiding raw data with type='n'
plot( Divorce ~ Marriage.s, data=d, type="n")
mtext("MedianAgeMarriage.s = 0" )
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)

#hold marraige rate constant, vary medianage of marriage
R.avg <- mean( d$Marriage.s)
A.seq <- seq(from=-3, to=3.5, length.out=30 )

pred.data2 <- data.frame(
		Marriage.s = R.avg,
		MedianAgeMarriage.s = A.seq
)

#apply model to the data
mu <- link(m5.3, data=pred.data2)

#calculate the MAP & prediction interval 
mu.mean <- apply(mu, 2, mean )
mu.PI <- apply( mu , 2, PI)

#simulate the data, and calculate the prediction interval
A.sim <- sim(m5.3, data=pred.data2, n=1e4)
A.PI <- apply(A.sim, 2, PI)

plot( Divorce ~ MedianAgeMarriage.s, data=d, type="n" )
mtext("Marriage.s = 0")
lines( A.seq, mu.mean)
shade(mu.PI, A.seq)
shade(A.PI, A.seq)

#x30mins 12-27-22

#posterior prediction plots

#call link without specifying new data
#so it uses original data
mu <- link(m5.3)

#summarize samples across cases
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


#simulate observations
#again no new data, so use original data
divorce.sim <-sim(m5.3, n=1e4 )
divorce.PI <- apply(divorce.sim, 2, PI )

plot( mu.mean ~ d$Divorce, col=rangi2, ylim=range(mu.PI),
	xlab="Observed Divorce", ylab="Predicted Divorce" )
abline(a=0, b=1, lty=2 )
for ( i in 1:nrow(d) )
	lines ( rep(d$Divorce[i],2), c(mu.PI[1,i],mu.PI[2,i]),
	col=rangi2 )

identify(x=d$Divorce, y=mu.mean, labels=d$Loc,cex=0.8)


#compute residuals
divorce.resid <- d$Divorce - mu.mean

#get ordering by divorce rate
o <- order(divorce.resid)

#make the plot

dotchart(divorce.resid[o], labels=d$Loc[o],xlim=c(-6,5), cex=0.6) #plot the residual point for each state

abline( v=0, col=col.alpha("black", 0.2) ) #plot center line at 0

for(i in 1:nrow(d) ){
	j<- o[i] # plot state in sorted order

	#plot the 89% confidence intervals
	lines(d$Divorce[j] - c(mu.PI[1,j], mu.PI[2,j]), rep(i,2) )
	points( d$Divorce[j] - c(divorce.PI[1,j], divorce.PI[2,j]), rep(i,2),
			pch=3, cex=0.6, col="gray" )

}

#simulating spurious associations
N <- 100 #number of cases
x_real <- rnorm(N) 	   #x_real as a gaussian with mean 0 and stdev = 1
x_spur <- rnorm(N, x_real) #x_spur as Gaussian with mean=x_real
y <- rnorm(N, x_real)	   #y as Gaussian with mean=x_real
d <- data.frame(y,x_real,x_spur) #bind all together in dataframes

pairs(d)


#12-27-22, 149/483, pg.135 Masked Relationships 1hr of reading

data(milk)
d <- milk
str(d)


library(rethinking)
data(milk)
d <- milk
str(d)

m5.5 <- map(
	alist(
		kcal.per.g ~ dnorm(mu, sigma),
		mu <- a + bn*neocortex.perc,
		a ~ dnorm(0, 100),
		bn ~ dnorm( 0, 1),
		sigma ~ dunif(0, 1)
		),
		data =d)

#identify na columns
d$neocortex.perc

#filter for non na columns
dcc <- d[ complete.cases(d), ]

m5.5 <- map(
	alist(
		kcal.per.g ~ dnorm(mu, sigma),
		mu <- a + bn*neocortex.perc,
		a ~ dnorm(0, 100),
		bn ~ dnorm( 0, 1),
		sigma ~ dunif(0, 1)
		),
		data =dcc)

precis(m5.5, digits=3)

np.seq <- 0:100
pred.data <- data.frame( neocortex.perc=np.seq)

#fit model to simulated data 
mu <- link(m5.5, data=pred.data, n=1e4 )

#calculate the map line
mu.mean <- apply(mu, 2, mean)

#calculate the 89% posterior interval for the mean
mu.PI <- apply(mu, 2, PI)

plot( kcal.per.g ~ neocortex.perc, data=dcc, col=rangi2)
lines( np.seq, mu.mean)
lines( np.seq, mu.PI[1,], lty=2)
lines( np.seq, mu.PI[2,], lty=2)

#transform the mass variable
dcc$log.mass <- log(dcc$mass)


#fit the bivariate model with logmass
m5.6 <- map(
		alist(
			kcal.per.g ~ dnorm( mu, sigma),
			mu <- a + bm*log.mass,
			a ~ dnorm( 0, 100),
			bm ~ dnorm( 0, 1),
			sigma ~ dunif( 0 ,1)
			),
			data=dcc)
precis(m5.6)

#30 min 12-29-22

#create a vector of 1000 evenly spaced points between 0 and 100
#take the log of the vector
np.seq <- log(seq(from = 0, to=100, length.out=1e4 ))
pred.data <- data.frame( log.mass=np.seq)

#fit model to simulated data 
mu2 <- link(m5.6, data=pred.data, n=1e4 )

#calculate the map line
mu2.mean <- apply(mu2, 2, mean)

#calculate the 89% posterior interval for the mean
mu2.PI <- apply(mu2, 2, PI)

plot( kcal.per.g ~ log.mass, data=dcc, col=rangi2)
lines( np.seq, mu2.mean)
lines( np.seq, mu2.PI[1,], lty=2)
lines( np.seq, mu2.PI[2,], lty=2)


#fit a model to both variables
m5.7 <- map(
		alist( kcal.per.g ~ dnorm( mu, sigma),
				mu <- a + bn* neocortex.perc + bm*log.mass,
					a ~ dnorm(0, 100),
					bn ~ dnorm( 0 , 1),
					bm ~ dnorm( 0 , 1),
					sigma ~ dunif( 0 ,1)
			), data=dcc )
precis(m5.7)

mean.log.mass <- mean( log(dcc$mass) )
np.seq <- 0:100
pred.data <- data.frame( 
				neocortex.perc=np.seq,
				log.mass= mean.log.mass
				)
mu <- link (m5.7, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean )
mu.PI <- apply(mu, 2, PI)

#plot kcal per g as a function of neocortex.perc
plot(kcal.per.g ~ neocortex.perc, data=dcc, type="n" ) 
lines(np.seq,  mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

#plot kcal per g as a function of body mass, using the new model
mean.neocortex <- mean( dcc$neocortex.perc )
np.seq <- log(seq(from = 0, to=100, length.out=1e4 ))
pred.data <- data.frame( 
				neocortex.perc=mean.neocortex,
				log.mass= np.seq
				)
mu <- link (m5.7, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean )
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, type="n" ) 
lines(np.seq,  mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

#12-29-22 154/483, 140
#simulating a mask relationships
N <- 100						#number of cases
rho <- 0.7						#correlation between xpos and xneg
x_pos <- rnorm(N)					#xpos is gaussian
x_neg <- rnorm(N, rho*x_pos,			#xneg is correlated with xpos
				sqrt(1-rho^2) )
y <- rnorm( N, x_pos - x_neg)			#y equally associated  with xpos and xneg
d <- data.frame(y, x_pos, x_neg)		#bind all together in data frame
pairs(d)

#simulating multicolinearity 156/483
N<- 100					#number of individuals
height <- rnorm(N,10,2)			#sim total height of each
leg_prop <- runif(N, 0.4,0.5)		#leg as a proportion of height

leg_left <- leg_prop*height + 	#simulate left leg as proportion + error
rnorm( N,0, 0.02)

leg_right <- leg_prop*height +	#simulate right leg as proportion + errpr
rnorm(N,0, 0.02)

d <- data.frame(height, leg_left, leg_right) #combine into df

#12-29 30 min at 9:14

#fit model for leg length
m5.8 <- map(
		alist(
			height ~ dnorm( mu, sigma),
			mu <-  a + bl* leg_left + br*leg_right,
			a ~ dnorm( 10, 100),
			bl ~ dnorm( 2, 10),
			br ~ dnorm( 2, 10),
			sigma ~ dunif( 0, 10)
			),
			data=d )
precis(m5.8)

plot(precis(m5.8))

#plot bl vs br from the sampled posterior distribution
post <- extract.samples(m5.8)
plot( bl ~br, post, col=col.alpha(rangi2,0.1), pch=16)

sum_blbr <- post$bl + post$br
dens( sum_blbr, col=rangi2, lwd=2, xlab=" sum of bl and br")

#fit the model with one leg length variable
m5.9 <- map(
		alist(
			height ~ dnorm(mu, sigma),
			mu <- a + bl*leg_left,
			a ~ dnorm(10, 100),
			bl ~ dnorm( 2, 10),
			sigma ~ dunif( 0 ,10)
			),
			data=d )
precis(m5.9)

#5.3.2 multicollinear milk
library(rethinking)
data(milk)
d <- milk

#Each bivariate regression model shows opposite behavior in the slope

#model kcal.per.g as a function of perc.fat
m5.10 <- map(
		alist(
			kcal.per.g ~ dnorm(mu, sigma ),
			mu <- a + bf*perc.fat,
			a ~ dnorm( 0.6, 10),
			bf ~ dnorm(0, 1),
			sigma ~ dunif(0, 10)
			), data=d )

#model kcal per g as a function of perc.lactose
m5.11 <- map(
		alist(
			kcal.per.g ~ dnorm( mu, sigma ),
			mu <- a + bl* perc.lactose,
			a ~ dnorm( 0.6, 10 ),
			bl ~dnorm(0, 1),
			sigma ~ dunif( 0, 10)
			),
			data= d)


precis(m5.10, digits=3 )
precis( m5.11, digits=3 )

#including both predictors into the model
m5.12 <- map(
		alist(
			kcal.per.g ~ dnorm(mu, sigma ),
			mu <- a + bf*perc.fat + bl*perc.lactose,
			a ~ dnorm(0.6, 10 ),
			bf ~ dnorm( 0, 1),
			bl ~ dnorm(0, 1),
			sigma ~ dunif(0, 10)
			),
			data =d )

precis(m5.12, digits=3)

#view the multicollinearity amongst perc.fat and perc.lactose
#
#The variables are negatively correlated, having both variables does not help the prediction
#
pairs( ~kcal.per.g + perc.fat + perc.lactose, data=d, col=rangi2 )

#compute the correlation amongst perc.fat and perc.lactose
cor(d$perc.fat, d$perc.lactose)


#Post Treatment Bias

#number of plants
N <- 100

#simulate initial heights
h0 <- rnorm(N, 10, 2)

#assign treatments and simulate fungus and growth
treatment <- rep(0:1, each = N/2)
fungus <- rbinom( N, size=1, prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N,5-3*fungus)

#compose a clean data frame
d <- data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)
str(d)

m5.13 <- map(
		alist(
			h1 ~ dnorm(mu, sigma),
			mu <- a + bh*h0 + bt*treatment + bf*fungus,
			a ~ dnorm(0, 100),
			c(bh, bt,bf) ~ dnorm(0,10),
			sigma ~ dunif(0,10)
			), data=d )

precis(m5.13)

#remove the post-treatment effect of fungus
m5.14 <- map(
			alist( 
					h1 ~ dnorm(mu, sigma),
					mu <- a + bh*h0 + bt*treatment,
					a ~ dnorm(0, 100),
					c(bh, bt) ~ dnorm(0,10),
					sigma ~ dunif(0,10)
				),
				data=d )
precis(m5.14)


#5.4.1 Binary Categories
data(Howell1)
d <-Howell1
str(d)

m5.15 <- map(
		alist(
			height ~ dnorm(mu, sigma ),
			mu <- a + bm*male,
			a ~ dnorm( 178, 100 ),
			bm ~ dnorm(0, 10),
			sigma ~ dunif(0,50)
			),
			data=d )
precis(m5.15)
#a = average female height, when m = 0
#bm = average difference amongst male and female heights

#get the width of the posterior distribution

post <- extract.samples(m5.15) #sample from the poseterior distribution

mu.male <- post$a + post$bm #compute male height
PI(mu.male)

#parameterizing categories 
m5.15b <- map(
		alist(
			height ~ dnorm( mu , sigma ) ,
			mu <- af*(1-male) + am*male ,
			af ~ dnorm( 178 , 100 ) ,
			am ~ dnorm( 178 , 100 ) ,
			sigma ~ dunif( 0 , 50 )
			) ,
			data=d )
precis(m5.15b)

#categorical variables with many categories
library(rethinking)
data(milk)
d <- milk
unique(d$clade)

#create a new world monkey dummy variable
d$clade.NWM <- ifelse( d$clade=="New World Monkey", 1, 0 ) 

#create an old world monkey variable
d$clade.OWM <- ifelse( d$clade=="Old World Monkey", 1, 0 ) 

#create an strepsirrhine variable
d$clade.S <- ifelse( d$clade=="strepsirrhine", 1, 0)

#fit model to data, where the model contains multiple dummy variables
m5.16 <- map(
		alist(
			kcal.per.g ~ dnorm(mu, sigma ),
			mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S,
			a ~ dnorm( 0.6, 10),
			b.NWM ~ dnorm( 0, 1 ),
			b.OWM ~ dnorm( 0,1 ),
			b.S ~ dnorm(0, 1 ) ,
			sigma ~ dunif( 0, 10 )
			),
			data= d)
precis(m5.16)

#get average milk energy in each category

#sample posterior
post<- extract.samples(m5.16)

#compute averages for each category
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

#summarize using precis
precis(data.frame(mu.ape,mu.NWM,mu.OWM, mu.S) )

#create a single variable to map different categories of monkeys
( d$clade_id <- coerce_index(d$clade) )

m5.16_alt <- map(
			alist(
				kcal.per.g ~ dnorm(mu, sigma),
				mu <- a[clade_id], #mu is a function of the intercepts
				a[clade_id] ~ dnorm(0.6, 10), #each intercept represents a species
				sigma ~ dunif(0, 10)
				),
				data=d )
precis(m5.16_alt, depth=2)
