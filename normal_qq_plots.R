## linear regression normal qq-plot.
#
################################################################################
# I live in the tidyverse.
################################################################################

require(tidyverse)                     # I live in the tidyverse.
require(broom)                         # For analysis of regression output.
require(cowplot)
#
#
################################################################################
# Our first examples uses the R built-in data set mtcars.
################################################################################

mtcars <-                                   # Convert dataframe to tibble.
  as_tibble( mtcars )

mtcars                                      # Verify data set.
#
#
################################################################################
# Draw a scatterplot diagram if mpg versus hp.
#################################################################################

p1 <-
  ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point()

p1


##################################################################################
# Lets take the log of the response variable mpg.
##################################################################################

mtcars

mtcars <-
  mtcars %>% 
    mutate( logmpg = log( mpg ),
            loghp  = log(hp))
mtcars


################################################################################
# Draw a scatter plot of logmpg versus hp.
################################################################################

p2 <-
  ggplot(mtcars, aes(x=hp, y=logmpg)) +
  geom_point()
p2


################################################################################
# As we have extreme observations of the x-axis lets take the natural log of it
# too and plot it also.
################################################################################

mtcars <-
  mtcars %>% 
  mutate( loghp  = log(hp))
mtcars

p3 <-
  ggplot(mtcars, aes(x=loghp, y=logmpg)) +
  geom_point()
p3
  

################################################################################
# The plot looks approximately linear so we fit the log of mpg versus the log of
# hoursepower.
################################################################################

fit <- lm( logmpg ~ loghp,               # Fit or estimate the model.
           data = mtcars )

glance( fit )                            # Display model performance.                        
tidy( fit )                              # Display estimates and properties.


################################################################################
# We are not going to perform a full residual analysis.  We will only look at 
# the distribution of the residuals.  We are only going to investigate the
# distribution of the residuals with a normal qq-plot.
################################################################################

ehat <- residuals( fit )                # Obtain the residuals from the fit.


#################################################################################
# The R function I have written to draw a normal qq-plot,  You can find this on
# GitHub at: gitHubSlinkman/RProjects/RStatistics.  The name of the function is 
# plot_qq_norm.  We need to load this function into our R environment (memory).
# We do this with the source statement.
################################################################################

source("D:/R-Projects/RStatistics/plot_qq_norm.R")


################################################################################
# We can now draw a normnal qq-plot using this function.
################################################################################

plot_qq_norm( variable = ehat,
              R = 100,
              variable_name = "fitted residuals")

###########################################################################
# We see that we have one residual that is outside the pseudo-bootstrap
# gray lines. This may be either an outlier or a deficiency in the model.
#
# But in general it looks like the distributions of the outliers is 
# approximately normal.
###########################################################################
