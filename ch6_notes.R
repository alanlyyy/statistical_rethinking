#build dataframe from scratch
sppnames <- c( "afarensis","africanus","habilis","boisei",
"rudolfensis","ergaster","sapiens")

brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )

masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

#fit linear model: brain size is a function of body weight
m6.1 <- lm(brain ~ mass, data=d)

summary(m6.1)

#calculate the proportion of variation for brain size explained
#by the variable body mass
1-  var(resid(m6.1))/var(d$brain)


#Overfitting example:

#fit a polynomial model, second order
m6.2 <- lm( brain ~ mass + I(mass^2), data=d)

#fit a 3rd order model
m6.3 <- lm( brain ~ mass + I(mass^2)+ I(mass^3) , data=d )

#fit a 4th order model
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) ,
data=d )

#fit a 5th order model
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
I(mass^5) , data=d )

#fit a 6th order model
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
I(mass^5) + I(mass^6) , data=d )

#underfitting example:
#model provides no predictive power to identify brain size. Fails to describe sample.
m6.7 <- lm(brain ~ 1, data=d)
summary(m6.7)

