#-------------- Example Lavaan (Confirmatory Factor Analysis) -----------------#
#------------------------------------------------------------------------------#
# We are going to perform a Confirmatory Factor Analysis on the Holzinger and 
#Swineford (1939) dataset. It consists of mental ability test scores of seventh
#and eighth-grade children from two different schools.
#------------------------------------------------------------------------------#

## Setting the working directory: 

#setwd("C:/Users/Jorge/Documents/R")

## Install lavaan and semPlot packages: 

install.packages("lavaan", dependencies=TRUE)
install.packages("semPlot", dependencies=TRUE)

## Accessing lavaan and semPlot libraries, and the data set to be used:

library(lavaan)
library(semPlot)
data(HolzingerSwineford1939)

#################### ONE-FACTOR MODEL ##########################

## 1. After checking the data, we create the model:

visual.model <- 'model=~x1 + x2 + x3 + x7 + x8 + x9'

## 2. create 'fit model' using 'Confirmatory Factor Analysis' ( cfa() ):

visual.fit <- cfa(model=visual.model,
                  data=HolzingerSwineford1939)

## 3. Summary results:

summary(visual.fit)

# Shows 'Latent variables' and 'Variances'.
#In 'Latent Variables': 'Estimate' (coeficients in scaled data) there is a paramenter 
#holding the value 1.000. That parameter has been used to scale the model. The rest 
#of the parameters have been scaled in reference to this parameter.


#3.1 Adding Standarized:
summary(visual.fit, standardized = TRUE)
# visual.fit > Model to be summarized. Basic Result #
# standardized = TRUE > adds Std.lv and Std.all (loadings)#

# The use of standarized=True helps to identify the strength in the relaionship between 
#the latent and the manifest variables (loadings). How well a latent variable predict the
#manifest variables.
#Std.all: Standarized solution. Values close to 1 shows strong relation to the latent variable.
#Std.lv: Scaled to the latent variable. i.e. Vlaues over 0.3 indicates strong relationship.

#3.2 Adding Fit measurements:
summary(visual.fit, standardized = TRUE, fit.measures = TRUE)
# visual.fit > Model to be summarized. Basic Result #
# standardized = TRUE > adds Std.lv and Std.all (loadings)#
# fit.measures = TRUE > shows fitness coeficients #

#Fit measurements indicate how well our data fit the model.The meauresments are:
#   Goodness (CFI & TLI): Values close to 1 (>0.9) => Good model 
#   Badness (RMSEA & SRMR): values close to 0 (<0.1) => Good model

##CONCLUSION:
#Running the summary function shows that CFI= 0.701; TLI= 0.502; RMSEA=0.190; SRMR=0.111
# This tells us that it is a bad fit.

#Probably because we combined two underlying factors (visual and speed related variables)
#into one latent variable (visual.model).

#A different approach is to create a multifactor model. In this case
#two-factor model: visual and speed.
#Each will hold a different set of variables:
#visual skills (x1, x2, x3) and speed skills (x7, x8, x9)


##################### TWO-FACTOR MODEL #######################

## 1. Create Model: Split the parameters into two factors: visual and speed

twofactor.model <- 'visual =~ x1 + x2 + x3
                  speed =~ x7 + x8 + x9'

## 2. Create fit model:

twofactor.fit <- cfa(model=twofactor.model,
                      data=HolzingerSwineford1939)

## 3. Summary results:

summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)

#We can see improvement in CFI= 0.879; TLI= 0.774; RMSEA=0.128; SRMR=0.079
# In multifactor analysis, a covariance table is added (~~), This tells us the
#amout by which two variables change together (Estimated). (Std.lv). On the other
#hand, tells us the 'overlaping' between the two factors.

## 4. Improving our model:
#a). Check the loadings (Std,lv & Std.all) of the manifest variables in the
#'Latent variable' table. to see if they are related (good: loadings > 0.3)
#b). Check the variances: To see if there are variances ('Estimate') extremely 
#disproportionated. The fucntion var() can give an idea of the range of the different
#variances p.e. (var(HolzingerSwineford1939$x1)) 
#c). Explore modification Indices:

modificationindices(twofactor.fit, sort=TRUE)

#The bigger the mi (modification index) the better. It means the change in the
#model once this index has been introduced. 

#Now we run the model with the chosen modification.

## 1. Create model:
twofactor.model <- 'visual =~ x1 + x2 + x3
                  speed =~ x7 + x8 + x9
                  x7 ~~  x8'

## 2. Create fit model:
twofactor.fit <- cfa(model=twofactor.model,
                     data=HolzingerSwineford1939)

## 3. Summary;
summary(twofactor.fit, standardized = TRUE, fit.measures = TRUE)

#We can see an improvement of the fitness:CFI= 0.976; TLI= 0.949; RMSEA=0.061; SRMR=0.044

## 4. Comparing models:
#To see if the additions statistically improves the model fit.
#Two ways: ANOVA (for nested models) and Fit Indices (for non-nested models)
#** Nested models: models that have the same variables but differ in one parameter **

#4.1 ANOVA:
#Set the two models to be compare (before and after the addition of the modification index)

#Creating models:
twofactor.model1 <- 'visual =~ x1 + x2 + x3
                    speed =~ x7 + x8 + x9'

twofactor.model2 <- 'visual =~ x1 + x2 + x3
                    speed =~ x7 + x8 + x9
                    x7 ~~  x8'

#Creating fit models:

twofactor.fit1 = cfa(model=twofactor.model1,
                     data=HolzingerSwineford1939)

twofactor.fit2 = cfa(model=twofactor.model2,
                     data=HolzingerSwineford1939)

summary(twofactor.fit2, standardized = TRUE, fit.measures = TRUE)
#Comparing using ANOVA:

anova(twofactor.fit1, twofactor.fit2)

#This test substracts the Chi-square of the two fit models (Chisq diff) and compares 
#if the result is significantly greater than expected for the difference in degrees 
#of freedom (Df diff).
#In this case, we can see that there is statistical differences for a p <0.0001

#Comparing with fit measure:
#This gives us a list of all the fit measure indices.
fitmeasures(twofactor.fit1)

#to select models:
fitmeasures(twofactor.fit1, c("aic", "ecvi"))
fitmeasures(twofactor.fit2, c("aic", "ecvi"))

#In this case, the smaller the indices the better the fit.
#We can see that the addition of the parameter 'x7 ~~ x8' signifficantly
#improves the model.

## 5. Diagrams:

semPaths(object = twofactor.fit2,
         whatLabels = "std", 
         edge.label.cex = 1, 
         layout = "tree", rotation = 1,
         what = "std", edge.color = "#666666")


#Manifest variables: squares (double arrow-head: variance)
#Latent variables: circles (double arrow-head: covariaince)
#Dashed arrows: marker variable for the coeficient estimate
#
#Observations:
# We can see how the two lantent varables (visual and speed) are represented by loading x1, x2, x3
#and x7, x8, x9 respectively. Also, showing the strength of this relationship (all above 0.3) 
#graphically represented by the darkness of the arrows.

#We also can see the strength of correlations of the latent variables and the variances of the manifest
#variables.









