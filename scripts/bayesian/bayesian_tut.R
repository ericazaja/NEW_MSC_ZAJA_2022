# Erica goes Bayesian
# using tutorial by Louise Litrico on Coding Club:https://ourcodingclub.github.io/tutorials/brms/ 
# last update: 20/02/2023
# RQ: Has the red knot population in France increased over time?

# libraries -----
library(tidyverse)
library(brms)
library(tidybayes)
library(dplyr)

# Load the data -----
France <- read_csv("CC-brms-main/Data/red_knot.csv")

# explore data -----
head(France)  # to get the first observations in each column
str(France)  # what type of variables do we have

(hist_france <- ggplot(France, aes(x = pop)) +
    geom_histogram(colour = "#8B5A00", fill = "#CD8500") +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nCalidris canutus abundance") +  # latin name for red knot
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))  

unique(France$year)
# we want the model to read this as Year 1 to Year n.
# i.e. change year variable to start at 1
# nb if first year is 1976, do this in the model
# I(year - 1975) 
# “I” specifying integer, and “year-1975” to make the year variable start at 1.

# or create new column:
# France <- France %>% mutate(year_2 = I(year - 1975)) 

# Basic model ----
france1_mbrms <- brms::brm(pop ~ I(year - 1975),
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000) # The iter argument defines how many times you want the model to run. 
# The warmup argument refers to the number of first iterations that the model should disregard
# The chains argument defines the number of independent times the model will run the iterations. 

# The Bayesian model runs many times by picking random values 
# and assessing how the distribution changes and fits the data,
# before deciding on a perfect fit of the posterior distribution 
# (which should end up bring a mix your data and the prior distribution). 

# The brm function has a default prior that is very uninformative:
# leave like that if you don’t have any prior information to give to the model

# your posterior distribution will be very close to the distribution of your data

saveRDS(france1_mbrms, "france1_mbrms.RDS")
# you can save the model as an RDS (Rdata) that way you don't need to run the model again if you come back to this code
# every time you (re)run a model, your output will be slightly different

# extract model output ------
summary(france1_mbrms)
# fixef(france1_mbrms) # to get more detailed values for estimates
# coef(model_name) # if you have group-level effects (hierarchical data)

# The model gives us an Estimate: the mean of our posterior distribution for each variable.
# These estimates can be used as the intercept and slope for the relationship between our two variables. 
# Est.Error is the error associated with those means (the standard error).

# 95% Credibility Interval (CI) tells us the interval in which 95%
# of the values of our posterior distribution fall. 
# The thing to look for is the interval between the values of l-95% CI and u-95% CI. 
# If this interval is strictly positive or negative, we can assume that the effect is significant (and positive or negative respectively). 
# However, if the interval encompasses 0, then we can’t be sure that the effect isn’t 0, aka non-significant. 
# In addition, the narrower the interval, the more precise the estimate of the effect.

# IN OTHER WORDS: 
# so it is if the range between the upper and lower CI cross 0:
# if one of the values is positive and one of them is negative then the interval “crosses 0” and 
# therefore there is not enough evidence for your model
# But if they are both positive or both negative then there is enough evidence 
# that the is an effect of whatever you’re looking at 





# model fit ------
# Look at summary: The Bulk_ESS a,d Tail_ESS are the effective sample size measures for each parameter. 
# These should be high (>1000) to be correct.
# the Rhat values for each effect should be equal to 1

plot(france1_mbrms)
# If you focus on the right hand plots, you want to see a sort of fuzzy caterpillar, 
# or a festive tinsel. If this is the case, it means your model explored all the possible values it could look at, 
# so it converged well. 
# On the x-axis of those trace plots, we have the iterations done 
# after the warmup (so 3000-1000 = 2000 in our case). And on the y-axis are all the values 
# of the mean of the posterior distribution that have been assessed by our model.

# On the left side, the density plots shows all of those mean values again, 
# plotted by the amount of times the model got this value (so the distribution of means basically). 
# The mean of this density plot is going to be the mean value that has been found by the model most often, 
# so probably the most “correct” one. And that value should be very close to the actual estimate that the summary function gave us. 
# In our case, the top plot is the intercept and that density plot seems to be centered around 8.70, which is the estimate value that we got in the summary!

# Posterior predictive checks
# check if you model predicts your data accurately (using the estimates).
pp_check(france1_mbrms, ndraws = 100)  
# The thin light blue lines on this plot represent 10 random draws or distributions created by the model (you can increase this by including ndraws = 100 in the code)
# The dark blue line represent the posterior distribution (which is considered to fit our data well so it can be used to compare model predictions with reality). 
# As you can see here, the two distributions look similar so everything is good.

# Model with random effects -----

# It could be that each year, the previous population level has an effect on the next year
#  This means that the population could be growing due to random variations every year, rather than throughout a whole time period.
# + (1| random variable)
# NB random variables AUTOMATICALLY become factors

france2_mbrms <- brms::brm(pop ~ I(year - 1975) + (1|year),
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
summary(france2_mbrms) # The summary doesn’t show an estimate of the effect for random variables, 
# but it accounts for it during the sampling. We can still see that there is no effect of year 
# as a random variable because our estimates have not changed compared to the first model.
plot(france2_mbrms)

# Model with 2 fixed effects ------
unique(France$Location.of.population)  # observations come from 2 locations
(boxplot_location <- ggplot(France, aes(Location.of.population, pop)) +
    geom_boxplot() +  # could be a significant effect between locations so should look at that
    theme_bw() +
    xlab("Location\n") +
    ylab("\nCalidris canutus abundance") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))  

# By including this categorical variable into our model, we can check if this difference is significant.
# As a side note, we will be including location as a fixed effect because we only have 2 locations. 
# If you want to include it as a random effect, your variable should have at least 5 “levels” or categories.

france3_mbrms <- brms::brm(pop ~ I(year - 1975) + Location.of.population,
                           data = France, family = poisson(), chains = 3,
                           iter = 3000, warmup = 1000)
summary(france3_mbrms)
plot(france3_mbrms)
pp_check(france3_mbrms)

# The summary also tells us that the effect of location is significant! 
# The estimate is -0.06 for Location.of.populationChannelCoast. 
# This means that the Channel coast population has a significantly lower abundance than the Atlantic coast population.

# LOO method to assess fit -----
# leave-one-out cross validation (LOO) method. 
# assesses the predictive ability of posterior distributions (a little like the pp_check function)
# You should look at the elpd estimate for each model, 
#the higher value the better the fit. 
#By adding compare = TRUE, we get a comparison already done for us at the bottom of the summary. 
# The value with an elpd of 0 should appear, that’s the model that shows the best fit to our data.

loo(france1_mbrms,france2_mbrms, france3_mbrms, compare = TRUE)
# 3rd model is best: elpd = 0

# Plotting model -------
# the relationship between our two main variables (abundance and time), 
# basically the line created with the intercept and slope values from our summary output. 
# In addition to that line, we can also add the credibility interval, because that shows the confidence that we have in that estimate. 
# And finally, adding the raw data points (abundance counts every year), we can show how well the model fits the original data.

(model_fit <- France %>%
   add_predicted_draws(france3_mbrms) %>%  # adding the posterior distribution
   ggplot(aes(x = year, y = pop)) +  
   stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and CI
                   alpha = 0.5, colour = "black") +
   geom_point(data = France, colour = "darkseagreen4", size = 3) +   # raw data
   scale_fill_brewer(palette = "Greys") +
   ylab("Calidris canutus abundance\n") +  # latin name for red knot
   xlab("\nYear") +
   theme_bw() +
   theme(legend.title = element_blank(),
         legend.position = c(0.15, 0.85)))

# one line per location
(location_fit <- France %>%
    group_by(Location.of.population) %>%
    add_predicted_draws(france3_mbrms) %>%
    ggplot(aes(x = year, y = pop, color = ordered(Location.of.population), fill = ordered(Location.of.population))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
    geom_point(data = France) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("Calidris canutus abundance\n") +
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank()))

# scaling variables-----
France$year.scaled <- scale(I(France$year - 1975), center = T)  # scaling time
France$pop.scaled <- scale(France$pop, center = T)  # scaling abundance

# The other thing that changes here is the distribution of the data, from poisson to normal, which you will have to change in the model as well
france4_mbrms <- brms::brm(pop.scaled ~ year.scaled + (1|location),
                                                  data = France, family = gaussian(), chains = 3,
                                                  iter = 3000, warmup = 1000)

# Informative priors ------
# random example 
prior1 <- c(set_prior(prior = 'normal(0,6)', class='b', coef='year'), #  and the numbers in the brackets describe the width and height of that shape (in this order: (mean, standard deviation)
            # global slope belongs to a normal distribution centered around 0
            set_prior(prior = 'normal(0,6)', class='Intercept', coef=''))  
# global intercept
set_prior(prior = 'cauchy(0,2)', class='sd'))		
# if we had group-level intercepts and slopes

# france5_mbrms <- brms::brm(pop ~ year + location, data = France,
#                            family = poisson(), chains = 3, prior = prior1,
#                            iter = 3000, warmup = 1000)

# Finally, increasing the number of iterations by a few thousands (and the warmup accordingly) might also help your model converge better by letting it run for longer.


                           