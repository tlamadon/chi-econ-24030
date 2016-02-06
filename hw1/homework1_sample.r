# Homework 1 - ECON 24030
# heckman selection model

# ---- GETTING STARTED -------
# downlaod RStudio 
#   https://www.rstudio.com/products/rstudio/download/
# download R if needed from: 
#   https://cran.rstudio.com/

# first time in RStudio, you need to install packages
install.packages(c('data.table','ggplot2','lattice')) # only the first time!

# ------ SETTING UP THE ENVIROMNENT ------
require(data.table)
require(ggplot2)

# data.table
#   tutorial:     
#     https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.pdf
#   cheatsheet:   
#     https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

# ggplot2
#   tutorial:     
#     http://www.ceb-institute.org/bbs/wp-content/uploads/2011/09/handout_ggplot2.pdf
#   cheatsheet: 
#     http://www.rstudio.com/wp-content/uploads/2015/12/ggplot2-cheatsheet-2.0.pdf
#   fulldoc:
#     http://docs.ggplot2.org/current/

# defining some parameters
N=10000 # number of observations we will use
p=list(
  a1 =-0.5,
  a2 = 1,
  a3 =0.3) # defining a set of parameters we will use

# HELP: the command list() creates a list, big surprise! you can then 
# access the values using the dollar sign
p$a1 + p$a2

# ------ SIMULATING A DATA SET -------
# we start by drawing a bunch of bunch of covariates and wages
data = data.table(i=1:N,w=rnorm(N),x=rnorm(N),z=rnorm(N),
                    nu=rnorm(N)*0.3,mu=rnorm(N)*0.2,coinflip=runif(N)<0.5)
# HELP: rnorm randomly draws numbers from a normal(0,1) distribution, using 
# rnorm(N)*0.2 we set the standard deviation to 02
# HELP: runif draws from a uniform, so by doing runif(N)<0.5, we generate a coin flip
# HELP: data.table(X1,X2,X3) creates a table with columns X1,X2,X3

# compute a dependent variable using our data, and parameters
data[, h:=  p$a1*w + p$a2*x + mu]

# HELP: here we use the special command of data.table that allows to create 
# a new column from other columns. The first argument is left empty, it 
# is used for selection, we will see this later the second argument 
# defines h using the ":=" operator which creates the column and assigns 
# it the left hand side

# ------ RUNNING A REGRESSION -------
# we simulated without any relationship between the error term mu and (x,w) 
# this means it will be true that E(mu|x,w)=0, we can run a regression
fit1 = lm( h ~ w +x , data)

# HELP: the command "lm" stands for linear model. It will fit the formula 
# you give it using the operator "~" and using the data passed as an argument. 
# Here we want to fit the linear regression of h on w and x. By default this
# will include a constant term

summary(fit1) 
# HELP: this command gives the summary statistics for the regression. 
# Note that the coefficient are indeed well recovered and significant, 
# they should be equal to p$a1 and p$a2

# ----- EXTRACT PREDICTED VALUE AND PLOT THEM -------
data[,hpred := predict(fit1)]
ggplot(data,aes(x=h,y=hpred)) + geom_point() 

# HELP: again here, we used the ":=" command with our data[,] to 
# assign a new colum. We also used the predict() function with computes 
# the predicted value of the linear model.
# HELP: next we used the ggplot command. We start by specifying the data 
# source in the first parameter. The second parameter aes, controls the 
# mapping of data columns into plotting "Aestetics". Here we want the
# x coordinate to be h, and the y-coordinate to be hpred, lastly we tell 
# ggplot that we would like to use the aesthetic mapping to display points.

# check this out, we can easily color by the coin flip using the color aestetic
ggplot(data,aes(x=h,y=hpred,color=coinflip)) + geom_point()
  
# we can aslo easily split by coin flip using the facet_wrap command
ggplot(data,aes(x=h,y=hpred)) + geom_point() + facet_wrap(~coinflip)

# ------ RUNNING A BINARY REGRESSION -------

# we first generate a binary decsiion
data[, d:= w+p$a3*z + 0.2*nu> 0 ]

# let's plot this with colors in the (w,z) space
ggplot(data,aes(x=w,y=z,color=d)) + geom_point()
# you should see a noisy separation with slopde approximatively -p$a3 !

# we can estimate this equation using the glm command
fit2 = glm(d~w+z,family = binomial('probit'),data)

# HELP: here we use the glm command, similar to the lm command with one 
# additional parameter which defines the "linkage" equation. Here we 
# tell it to use the probit because we know are error is generated from a 
# normal distribution. (For exponenial family, use logit)

summary(fit2)

# this gives the summary stastics. Note that we did not recover (1,a3) 
# for the coffecients? Actually this is because the scale is not 
# indentified when running a glm regression. But note that:
coef(fit2)[3]/coef(fit2)[2]
# does give us correclty our p$a3 parameter!

# we can recover the predicted values and plot them:
data[, dpred:= predict(fit2)]
ggplot(data,aes(x=w+p$a3*z,y=dpred)) + geom_point()

# they should land very nicely on the 45 degree line! Note that here we 
# plot the predicted w+p$a3*z, this is the default output of predict 
# for glm. If we wanted the probability we would need to do use predict()
data[,dpred_pr := predict(fit2,type="response")]
ggplot(data,aes(x=w+p$a3*z,y=dpred_pr)) + geom_point()

# we can also do that with color for z
ggplot(data,aes(x=w,y=dpred_pr,color=z)) + geom_point()

# ------- COIMPUTING A MILLS RATIO --------

# the 2 functions you need for that are dnorm (normal PDF) and pnorm (normal CDF)
# so for instance to compute a mills ratio from our previous binary decision we compute:

data[, lambda := dnorm(dpred)/pnorm(dpred)]
ggplot(data,aes(x=w+p$a3*z,y=lambda)) + geom_point()





