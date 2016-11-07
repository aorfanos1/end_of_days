#Assignment 4
#Alex Orfanos
install.packages('tseries')
library(tseries)

#A few different series of vectors that all theoretically should have a normal distribution

#Fuction takes an input of a sample and returns a vector of a specified length y of 
#the means boostrapped with replacement

sample_the_mean <- function(x,y) {
  Tboot <- c()
  xx1 <- c()
  for(i in 1:y){
    xx1 <- sample(x, size=length(x), replace=TRUE)
    Tboot[i] <- mean(xx1)
  }
  return(Tboot)
}

sample_the_mean_dif <- function(x,y) {
  for(i in 1:y){
    xx1 <- sample(x, size=30, replace = FALSE)
    Tboot[i] <- mean(xx1)
  }
  return(Tboot)
}

hist(sample_the_mean_dif(runif(1000), 1000))

#Fairly large uniform distribution mean 
unif_large <- sample_the_mean(runif(100),1000)
mean(unif_large)
qqnorm(unif_large)

#Smaller uniform distibution mean
unif_small <- sample_the_mean(runif(10), 100)
mean(unif_small)
qqnorm(unif_small)
#Maybe make a shiny plot that takes a bootstapped mean case and you can use sliders to change
#how many times you recalculate the mean 
#Exponential random variable testing 
exp_large <- sample_the_mean(rexp(20), 1500)
qqnorm(exp_large)

#Binomial random variable
binom_large <- sample_the_mean(rbinom(100,1,0.5), 10000)
qqnorm(binom_large)
hist(binom_large)

#Exponential small
exp_small <- sample_the_mean(rexp(10), 5)
qqnorm(exp_small)
ks.test(exp_small, rnorm(1000))

#Exponential large
exp_large <- sample_the_mean(rexp(10),1000)
qqnorm(exp_large)
ks.test(exp_large, rnorm(1000))



####Question 2
#If x is uniform the distribution of x looks like
hist(runif(10), probability = TRUE)
hist(runif(100),probability = TRUE)
hist(runif(10000), probability = TRUE)


qqnorm(runif(1000))
#If we are sampling the mean then the distribution looks like this
data <- sample_the_mean(runif(10000),1000)
hist(data)
#This data looks suspiciously normal
qqnorm(data)
#Now if y = 1/x the distribution of x would look like 
g_x <- 1/runif(1000, min = 1 , max = 2) 
qplot(g_x, geom = 'histogram', xlim = c(0.5,1), bins = 50)
qqnorm(g_x)
#We can also see that Y bar follows a normal distribution
y_bar <- sample_the_mean(g_x, 1000)
qplot(y_bar, geom = 'histogram', xlim = c(0.66,0.75), bins = 50)
qqnorm(y_bar)


####Question Number 3 


random_t <- as.data.frame(rt(1000, df = 1))
names(random_t)[1] <- 'values'
#One DF
#little
qqnorm(r(20,1))
#Big
qqnorm(r(1000,1))

#Three DF
qqnorm(r(20,3))
qqnorm(r(1000,3))

#Five DF
qqnorm(r(20,5))
qqnorm(r(1000,5))

#Seven DF
qqnorm(r(20,7))
qqnorm(r(1000,7))

ggplot(data = random_t, aes(x = 'values')) + geom_histogram(aes())
names(random_t)[1] <- 'values'


ui <- fluidPage(
  numericInput( inputId = 
    'samplesize', 'Sample Size', value = 100
  ),
  numericInput( inputId = 'df', 'Degrees of Freedom', value = 1),
  plotOutput(outputId="hist"),
  plotOutput(outputId = 'plot')
)
server1 <- function(input, output) {
  output$hist <- renderPlot({
    hist(rt(input$samplesize,input$df), breaks = 20, probability = TRUE)
  })
}
shinyApp(ui = ui, server = server1)




#####Question 4
set.seed(10)
product <- read.delim('productsales.dat', header = F, sep = '\t')
product_vec <- unlist(product, use.names= F)
product_vec
median(product_vec)
sample_the_median <- function(x,y) {
  Tboot = c()
  yy1 = c()
  for(i in 1:y){
    yy1 <- sample(x, size=length(x), replace=TRUE)
    Tboot[i] <- median(yy1)
  }
  return(Tboot)
}

bootstrap_med <- sample_the_median(product_vec, 1000)
true_med <- median((bootstrap_med))
#The bootstrapped median is
true_med

#The MSE using the median is 
(sum((product_vec-true_med)^2))/length(product_vec)



