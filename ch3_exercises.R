#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

prior <- rep(1,1000) #uniform prior

#6 successes from 9 samples
likelihood <- dbinom(6, size=9, prob=p_grid)

#create a posterior distribution
posterior <- likelihood*prior

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

set.seed(100)

#sample the posterior distribution and use p_grid to store the samples
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

#3E1. How much posterior lies below p = 0.2
sum(samples < 0.2)/1e4

#3E2. How much posterior lies above p= 0.8
sum(samples > 0.8)/1e4

#3E3. How much posterior probability lies between p=0.2 to p=0.8?
sum(samples > 0.2 & samples < 0.8)/1e4

#3E4. 20% of the posterior lies below which value of p?
quantile(samples,0.2)

#3E5. 20% of the posterior lies above which value of p?
quantile(samples, 1-0.2)

#3E6. Which value of p contain the narrowest interval equal to 66% of the posterior probability?
library(rethinking)
HPDI(samples, prob=0.66) 

#3E7. Which values of p contain 66% of the posterior probability , 
#assuming equal posterior probability both below and above the interval
PI(samples , 0.66)

#3M1. Suppose the globe tossing data had turned out to be 8 waters in 15 tosses.
#Construct the posterior distribution, using grid approximation.
#Use the same flat prior as before.

#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

prior <- rep(1,1000) #uniform prior

#8 successes from 15 tosses
likelihood <- dbinom(8, size=15, prob=p_grid)

#create a posterior distribution
posterior <- likelihood*prior

plot(likelihood)

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

set.seed(100)

#3M2. Draw 10000 samples from the grid approximation from above. Then use the
#samples to calculate the 90% HDPI for p 
#(the narrowest interval that contains 90% of the probability)

#sample the posterior distribution and use p_grid to store the samples
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

HPDI(samples, 0.9) #Interval 0.3343343 0.7217217 

#3M3. Construct a posterior predictive check for this model and data. Simulate
#the distribution of samples, averaging over the posterior uncertainty in p.
#What is the probability of observing 8 waters in 15 tosses?

library(rethinking)

#simulate the posterior predictive distribution to grab observations
#compute the binomial distribution for 10000 random points from the posterior distribution set as the parameter
dummy_w <- rbinom(1e4, size=15, prob=samples)

simplehist(dummy_w)

mean(dummy_w==8) #The probability of getting 8 W in 15 tosses = 14.24%

#3M4.
#Using the posterior distribution constructed from the new (8/15) data, now calculate
#the probability of observing 6 water in 9 tosses

#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

prior <- rep(1,1000) #uniform prior

#8 successes from 15 tosses
likelihood <- dbinom(8, size=15, prob=p_grid)

#create a posterior distribution
posterior <- likelihood*prior

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

set.seed(100)

#sample the posterior distribution and use p_grid to store the samples
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

#randomly sample 10000 points with a sample size of 9, using the posterior distribution (15 choose 8)
w<- rbinom(1e4,size=9, prob=samples)
simplehist(w)

mean(w==6) #The probability of getting 6 W in 9 tosses is 17.6%

#3M5.

library(rethinking)
#using a step prior redo M1 through M4

#use a prior step function, majority of earth is covered in water.
prior <- ifelse(p_grid <0.5, 0, 1)
plot(prior)


#3M5 - M1
#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

#8 successes from 15 tosses
likelihood <- dbinom(8, size=15, prob=p_grid)

#create a posterior distribution
posterior <- likelihood*prior

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

plot(posterior)

set.seed(100)

#3M5 - M2
#draw samples from p_grid uing the posterior probabilities (p>0.5)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
length(samples)
dens(samples)

HPDI(samples, 0.9) #Interval 0.5005005 0.7117117 

#Having knowledge majority of earth is covered in water, the HPDI interval is more narrow compared to
#the HPDI interval,0.3343343 0.7217217, using the flat prior.

#3M5 - M3
#simulate the posterior predictive distribution to grab observations
#draw 10000 numbers from the binomial distribution with different parameter p found in the sampled posterior distribution
dummy_w <- rbinom(1e4, size=15, prob=samples)

simplehist(dummy_w)

mean(dummy_w==8) #The probability of getting 8 W in 15 tosses = 15.4%

#Having knowledge majority of earth is covered in water, the proportion for 8 successes in 15 tosses, is slightly higher than
#the proportion of 8/15 using the flat prior, 14.24%

#3M5 - M4
#randomly sample 10000 points with a sample size of 9, using the posterior distribution (15 choose 8)
w<- rbinom(1e4,size=9, prob=samples)
simplehist(w)

mean(w==6) #The probability of getting 6 W in 9 tosses is 23.67%

#Having knowledge majority of earth is covered in water, the proportion for 6 successes in 9 tosses sampling from the posterior, is slightly higher than
#the proportion of 6/9 using the flat prior, 17.6%

#When comparing inference to true p= 0.7 between prior

#step prior

prior <- ifelse(p_grid <0.5, 0, 1)
plot(prior)

#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

#8 successes from 15 tosses
likelihood <- dbinom(8, size=15, prob=p_grid)

#create a posterior distribution
posterior <- likelihood*prior

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

posterior[700] #for p = 0.7, 700/1000 the density = 0.00218

plot(posterior)
#uniform prior

#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

prior <- rep(1,1000) #uniform prior

#8 successes from 15 tosses
likelihood <- dbinom(8, size=15, prob=p_grid)

#create a posterior distribution
posterior <- likelihood*prior

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

posterior[700] #p = 0.7 == 700/1000, density = 0.001304

#having prior knowledge of p >0.5, majority of earth is surrounded by water, increases the density at p = 0.7 for the posterior distribution 

#3H
library(rethinking)

#load sample data
data(homeworkch3)

#compute total number of boys birth across all births
sum(birth1) + sum(birth2)

birth1

#3H1 - Using grid approximation, compute the posterior distribution for the probability of a birth being a boy.
Assume uniform prior probability, which parameter value maximizes the posterior distribution? (mode value?)

#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

prior <- rep(1,1000) #uniform prior

#parameters
#BB
#BG
#GB
#GG

#number of successes: get total number of births resulting in a boy
#number of trials: from all possible births from 2 birth families
allbirths = c(birth1,birth2)
likelihood = dbinom( sum(allbirths), size=length(allbirths) , prob = p_grid)

#create a posterior distribution
posterior <- likelihood*prior

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

plot(x = p_grid, y = posterior, type = "l")

#identify mode of the posterior distribution 
p_grid[which.max(posterior)]

#the parameter which maximizes the posterior distribution is 0.5546

#3H2. Using the sampling function, draw 10000 random parameter values
#from the posterior distribution you calculated above. Use the samples to estimate 
#the 50%, 89%, and 97% HDPI

#sample the posterior distribution and use p_grid to store the samples
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

HPDI(samples, 0.5) #0.5255255 0.5725726 
#The narrowest interval that contains 50% of the distribution, 
#is between the parameters 0.52 and 0.572.

HPDI(samples, 0.89) #0.4964965 0.6066066 
#The narrowest interval that contains 89% of the distribution,
#is between the parameters 0.49 and 0.61

HPDI(samples, 0.97) #0.4774775 0.6266266 
#The narrowest interval that contains 97% of the distribution,
#is between the parameters 0.47 and 0.63

#3H3. Use the rbinom to simulate 10k replicates of 200 births,
# you should end up with 10000 numbers, each one a count of boys out of 200 births.
# Compare the distribution of predicted numbers of boys to the actual count in the data
#(111 boys out of 200 births). There are many ood ways to visualize the simulations, but the dens
#command is probably the easiest in this case. Does it look like the model fits the data well?
#That is , does the distribution of predictions include the actual observation as a central, likely outcome?

#randomly sample 10k points with a sample size of 9, using the posterior distribution 111 successes in 200 trials
dummy_w<- rbinom(1e4,size=200, prob=samples)
dens(dummy_w)
mean(dummy_w)
#The posterior predictive distribution includes 111 as the central tendency.
#The mean of the distribution is 111

#3H4. Compare 10k counts of boys from 100 simulated first borns only to the number of boys in the first births, birth1.
How does the model look

sum(birth1) #51 births of boys from birth1
length(birth1)
#create a vector of 1000 evenly spaced points with a min of 0 and max of 1
p_grid <- seq( from= 0, to=1, length.out=1000)

#number of successes: get total number of births resulting in a boy from birth1
#number of trials: from all possible boys within birth 1
likelihood = dbinom( sum(birth1), size=length(birth1) , prob = p_grid)

#create a posterior distribution
posterior <- likelihood*prior

#standardize the posterior distribution, so the integral sums up to 1
posterior <- posterior / sum(posterior)

plot(posterior)

#sample the posterior distribution and use p_grid to store the samples
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

#simulate 10k 100 first borns using the probability weights of the sampled posterior
dummy_w<- rbinom(1e4,size=100, prob=samples)

mean(dummy_w) 
#The number of first born boys is 51/100, 
#while the mean/central tendency of the posterior predictive distribution is approximately 51/100.

dens(dummy_w)

#3H5. The model assumes sex of first and second births are independent. To check this assumption,
focus now on second births that followed female first borns. 
#Compare 10k simulated counts of boys to only those second births that followed girls. 
#To do this correctly, you need to count the number of first borns who were girls and simulate that many births, 10k times.
#Compare the counts of boys in your simulations to the actual observed count of boys following girls.
#How does the model look in this light? Any guesses what is going on in these data?

B1 = Girl
B2 = Boy or Girl

Compare 10k simulated counts of boys, where B1 = Girl & B2 = Boy

#get the number of first birth girls
number_of_first_girls = length(birth1) - sum(birth1)

#simulate 10000 trials of 49 girls, using the probability weights of
#of the posterior distribution (51 boys | 100 first births)
dummy <- rbinom(1e4,size = number_of_first_girls, prob= samples)
dens(dummy)

number_of_first_girls_then_boys = sum(birth2[birth1==0]) #39
abline(v=number_of_first_girls_then_boys, col='blue')

mean(dummy)
#Since the count of B1 = Girl & B2 = Boy does not fit the central tendency of the distribution.
#The model is a poor fit for the data.