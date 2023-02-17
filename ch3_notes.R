"probability_of_positive_given_vampire" <- 0.95
"probability_of_positive_given_mortal" <- 0.01
"probability_of_vampire" <- 0.001
"probability_of_positive" <- probability_of_positive_given_vampire*probability_of_vampire + probability_of_positive_given_mortal*(1-probability_of_vampire)
"probability_of_vampire_given_a_positive_test" <- probability_of_positive_given_vampire*probability_of_vampire / probability_of_positive
probability_of_vampire_given_a_positive_test

#create a vector of 1000 linearly spaced points from 0 to 1
p_grid <- seq( from=0, to=1, length.out=1000)

#create a vector of 1000 with the value of 1
prior <- rep(1,1000) 

#6 successes in a sample of 9, for 1000 trials
likelihood <- dbinom( 6, size=9, prob=p_grid)


#Posterior = P(Y | X) = P(X|Y)*P(Y) / P(X)
#P(X) = P(X |Y1)*P(Y1)  + P(X| Y2)*P(Y2) + P(X | Yn)*P(Yn)
#sum(posterior) standardizes the data so the integral over the likelihood*prior = 1
posterior <- likelihood*prior
posterior <- posterior/ sum(posterior)
plot(posterior)

#pull out 10000 samples from the posterior probability proportions, and place inside p_grid
sample <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
plot(sample)

library(rethinking)
dens(sample)

#what is the probability the proportion of water is less than 0.5?
#add up posterior probability where p < 0.5
sum( posterior[ p_grid < 0.5])

#same calculattion using samples - get the frequency of samples
sum(sample < 0.5) / 1e4 # count the number of items < 0.5, divide by total number of samples

#how much probability lies between 0.5 and 0.75
sum ( sample > 0.5 & sample < 0.75) /1e4 #count number of items between 0.5 - 0.75, divide by total number of samples.

#What is the 80th percentile boundary for the posterior distribution?
quantile( sample, 0.8)

#What are the boundaries for the middle 80th percentile?
quantile(sample, c(0.1, 0.9) )



p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1,1000)
likelihood <- dbinom( 3, size=3, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

#calculate the middle 50th percentile for a skewed distribution
PI(samples, prob=0.5)

#HDPI - calculate the narrowest interval that accounts for the probability mass
#Get the 50% of datapoints with the narrowest interval.
HPDI(samples, prob=0.5) 

#identify the mode of the posterior function
p_grid[ which.max(posterior)]

#approximate the mode of the sampled posterior
chainmode(samples,adj=0.01)

#get the mean of the sample
mean(samples)

#get median of sample
median(samples)

#Get the probability of posterior and distance from 1
sum(posterior*abs(0.5 - p_grid))

#Sum the weighted posterior <= 1
#for each value d in p_grid,
loss <- sapply(p_grid, function(d) sum(posterior*abs( d-p_grid ) ) )
plot(loss)

#and identify the min sum for a given loss value.
p_grid[ which.min(loss) ]


#compute the binomial distribution for 0 to 2 successes, 
#given the sample size =2 and the probability of a success is 0,7
dbinom(0:2, size=2, prob=0.7)

#create sampled data for models.

#get 1 simulation, given a sample size of 2 and a probability of a sucess = 0.7
rbinom(1, size=2, prob=0.7)

#get 10 simulations
rbinom(10, size=2, prob=0.7)

#generate 100k simulations 
dummy_w <- rbinom(1e5, size=2, prob=0.7)

#identify proportion for number of successes
table(dummy_w)/1e5 #very similiar to numbers produced by dbinom(0:2, size=2, prob=0.7)

#run 100k simulations with a sample size of 9
dummy_w <- rbinom(1e5, size=36, prob=0.7)

#create a histogram using the results from the 100k simualations
simplehist( dummy_w, xlab="dummy water count")


#simulate predicted observations for parameter p = 0.6 (proportion of water is 0.6)
#10000 simulations using a sample size of 9, with probability of success = 0.6
w <- rbinom( 1e4, size=9, prob=0.6)
simplehist(w)

# 
dens(samples)
plot(samples)

p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1,1000)
likelihood <- dbinom( 6, size=9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

#samples is the sampled posterior distribution
#process 10000 simulations using the binomial distribution with a sample size of 9, 
#The probability of success is drawing a random parameter from the posterior distribution 
#The new simulated values average out the uncertainties over many parameters in the posterior.
w<-rbinom( 1e4, size=9, prob=samples) 
simplehist(w)