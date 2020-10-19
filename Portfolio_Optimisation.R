#Library Installation 
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(fPortfolio)
library(portfolio.optimization)
library( ggplot2)
library( scales )
#######################################

#######Additon of Stocks( 18 stocks)##########

myStocks <- c("DB", "RBS", "JPM", "MS", "C", "GS", "AXP", "BAC", "BLK", "AAPL", "AMZN",
              "GOOGL", "FB", "GOLD", "OIL", "NFLX", "DIS", "MSFT")
#####################################

##########building the portfolio and gathering the weekly returns ################
portfolio1 <- getSymbols( myStocks[1],
                          auto.assign = FALSE,
                          from = "2018-01-01", to = "2019-01-01")
#Grabbed close price
portfolio2 <- portfolio1[ ,4 ]
#Weekly Returns
portfolio3 <- weeklyReturn(portfolio2)

for (i in 2:length(myStocks) ) {
  #intiziling the portfolio
  portfolio1 <- getSymbols( myStocks[i],
                            auto.assign = FALSE,
                            from = "2018-01-01", to = "2019-01-01")
  #Grabbed close price
  portfolio2 <- portfolio1[ ,4 ]
  #Weekly Returns
  portfolio3a <- weeklyReturn(portfolio2)
  #Glues together
  portfolio3 <- cbind( portfolio3, portfolio3a )
}

names( portfolio3 )<- myStocks
#########################################################
#Portfolio returns function of the weights:find an optimal portfolio changing the weights values.
#We can define portfolio returns time series in function of the weights by multiplying each asset return by the weight related to it
portfolio_returns = function(x) {
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + portfolio3[,i] * x[i]
  }
  
  return (port.returns)
}
#####################################################
####### Sharpe Function #########
sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)/sqrt(var(port.returns)))
  
}
#######################
##### Constraint function greater than 0 less than 1 #########
constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr +
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}
#################################
####### objective function for optimization#####
obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  
  return (-sharpe(x)+100*constraint(x))
}
###########################
### Generic Algorithm including stopping iteration when max fitness found######
library("GA")
ga_res = ga(
  # Tell the genetic algorithm that the
  # weights are real variables
  type="real-valued",
  
  # "ga" function performs maximization, so we must
  # multiply the objective function by -1
  function(x){-obj(x)},
  
  # x_i >= 0
  lower = rep(0,ncol(portfolio3)),
  
  # x_i <= 1
  upper = rep(1,ncol(portfolio3)),
  
  # Maximum number of iterations
  maxiter = 1000,
  
  # If the maximum fitness remains the same for 50
  # consecutive transactions, stop the algorithm
  run=50,
  
  # We want to see the partial results of the process
  # while it performs
  monitor=TRUE,
  
  # Seed useful for replicating the results
  seed=1
)
fitness <- function(x) 
  
  #####################


#######resulting weights in a vector###
sol = as.vector(summary(ga_res)$solution)
sol
####################

######## portfolio illustration with the bold black weighted portfolio line########

MyPortfolio = portfolio_returns(sol)
plot(cumsum(MyPortfolio),type="l",lwd=5,
     main= "MyPortfolio")
lines(cumsum(portfolio3[,1]),col="blue")
lines(cumsum(portfolio3[,2]),col="red")
lines(cumsum(portfolio3[,3]),col="green")
lines(cumsum(portfolio3[,4]),col="violet")
lines(cumsum(portfolio3[,5]),col="peru")
lines(cumsum(portfolio3[,6]),col="pink")
lines(cumsum(portfolio3[,7]),col="purple")
lines(cumsum(portfolio3[,8]),col="yellow")
lines(cumsum(portfolio3[,9]),col="orange")
lines(cumsum(portfolio3[,10]),col="cyan")
lines(cumsum(portfolio3[,11]),col="magenta")
lines(cumsum(portfolio3[,12]),col="lavender")
lines(cumsum(portfolio3[,13]),col="brown")
lines(cumsum(portfolio3[,14]),col="grey")
lines(cumsum(portfolio3[,15]),col="aquamarine")
lines(cumsum(portfolio3[,16]),col="gold")
lines(cumsum(portfolio3[,17]),col="navy")
lines(cumsum(portfolio3[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","khaki3" ),
       lty=1, cex = 0.45)


######### PART 2########


myStocks <- c("DB", "RBS", "JPM", "MS", "C", "GS", "AXP", "BAC", "BLK", "AAPL", "AMZN",
              "GOOGL", "FB", "GOLD", "OIL", "NFLX", "DIS", "MSFT")
#####################################

##########building the portfolio and gathering the weekly returns ################
portfolio1_2019 <- getSymbols( myStocks[1],
                               auto.assign = FALSE,
                               from = "2019-01-01", to = "2020-01-01")
#Grabbed close price
portfolio2_2019 <- portfolio1_2019[ ,4 ]
#Weekly Returns
portfolio3_2019 <- weeklyReturn(portfolio2_2019)

for (i in 2:length(myStocks) ) {
  #intiziling the portfolio
  portfolio1_2019 <- getSymbols( myStocks[i],
                                 auto.assign = FALSE,
                                 from = "2019-01-01", to = "2020-01-01")
  #Grabbed close price
  portfolio2_2019 <- portfolio1_2019[ ,4 ]
  #Weekly Returns
  portfolio3a_2019 <- weeklyReturn(portfolio2_2019)
  #Glues together
  portfolio3_2019 <- cbind( portfolio3_2019, portfolio3a_2019 )
}

portfolio_returns_2019 = function(x) {
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + portfolio3_2019[,i] * x[i]
  }
  
  return (port.returns)
}

optimal_returns_2019 = portfolio_returns_2019(sol)

plot(cumsum(optimal_returns_2019),type="l",lwd=5,
     main= "Optimal_2019")
lines(cumsum(portfolio3_2019[,1]),col="blue")
lines(cumsum(portfolio3_2019[,2]),col="red")
lines(cumsum(portfolio3_2019[,3]),col="green")
lines(cumsum(portfolio3_2019[,4]),col="violet")
lines(cumsum(portfolio3_2019[,5]),col="peru")
lines(cumsum(portfolio3_2019[,6]),col="pink")
lines(cumsum(portfolio3_2019[,7]),col="purple")
lines(cumsum(portfolio3_2019[,8]),col="yellow")
lines(cumsum(portfolio3_2019[,9]),col="orange")
lines(cumsum(portfolio3_2019[,10]),col="cyan")
lines(cumsum(portfolio3_2019[,11]),col="magenta")
lines(cumsum(portfolio3_2019[,12]),col="lavender")
lines(cumsum(portfolio3_2019[,13]),col="brown")
lines(cumsum(portfolio3_2019[,14]),col="grey")
lines(cumsum(portfolio3_2019[,15]),col="aquamarine")
lines(cumsum(portfolio3_2019[,16]),col="gold")
lines(cumsum(portfolio3_2019[,17]),col="navy")
lines(cumsum(portfolio3_2019[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","kh"), lty = 1)
####################

######### PART 3 ########

#Balanced Portfolio 2018 - 2019

balanced_weights <- as.vector(rep(0.05555556, length(myStocks)))
balanced_weights

balanced_portfolio_returns = portfolio_returns(balanced_weights)
print(balanced_portfolio_returns)

plot(cumsum(balanced_portfolio_returns),type="l",lwd=5,
     main= "Part 3_Balanced_2018 - 2019")
lines(cumsum(portfolio3[,1]),col="blue")
lines(cumsum(portfolio3[,2]),col="red")
lines(cumsum(portfolio3[,3]),col="green")
lines(cumsum(portfolio3[,4]),col="violet")
lines(cumsum(portfolio3[,5]),col="peru")
lines(cumsum(portfolio3[,6]),col="pink")
lines(cumsum(portfolio3[,7]),col="purple")
lines(cumsum(portfolio3[,8]),col="yellow")
lines(cumsum(portfolio3[,9]),col="orange")
lines(cumsum(portfolio3[,10]),col="cyan")
lines(cumsum(portfolio3[,11]),col="magenta")
lines(cumsum(portfolio3[,12]),col="lavender")
lines(cumsum(portfolio3[,13]),col="brown")
lines(cumsum(portfolio3[,14]),col="grey")
lines(cumsum(portfolio3[,15]),col="aquamarine")
lines(cumsum(portfolio3[,16]),col="gold")
lines(cumsum(portfolio3[,17]),col="navy")
lines(cumsum(portfolio3[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","kh"), lty = 1)
####################

#Random portfolio 2018 - 2019

get_random_weights <- function(){
  loop_length = 5
  x < NULL
  for (i in 1:loop_length){
    x <-x + runif(length(myStocks))
  }
  x<- x/(sum(x))
  return(x)
  
} 

random_weights <- as.vector(get_random_weights())

random_portfolio_returns_2018 = portfolio_returns(random_weights)
print(random_portfolio_returns_2018)

plot(cumsum(random_portfolio_returns_2018),type="l",lwd=5,
     main= "Part 3_Random_2018 - 2019")
lines(cumsum(portfolio3[,1]),col="blue")
lines(cumsum(portfolio3[,2]),col="red")
lines(cumsum(portfolio3[,3]),col="green")
lines(cumsum(portfolio3[,4]),col="violet")
lines(cumsum(portfolio3[,5]),col="peru")
lines(cumsum(portfolio3[,6]),col="pink")
lines(cumsum(portfolio3[,7]),col="purple")
lines(cumsum(portfolio3[,8]),col="yellow")
lines(cumsum(portfolio3[,9]),col="orange")
lines(cumsum(portfolio3[,10]),col="cyan")
lines(cumsum(portfolio3[,11]),col="magenta")
lines(cumsum(portfolio3[,12]),col="lavender")
lines(cumsum(portfolio3[,13]),col="brown")
lines(cumsum(portfolio3[,14]),col="grey")
lines(cumsum(portfolio3[,15]),col="aquamarine")
lines(cumsum(portfolio3[,16]),col="gold")
lines(cumsum(portfolio3[,17]),col="navy")
lines(cumsum(portfolio3[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","kh"), lty = 1)
####################

#Balanced Portfolio 2019 - 2020

balanced_portfolio_returns_2019 = portfolio_returns_2019(balanced_weights)
print(balanced_portfolio_returns_2019)


plot(cumsum(balanced_portfolio_returns_2019),type="l",lwd=5,
     main= "Part 3_Balanced_2019 - 2020")
lines(cumsum(portfolio3_2019[,1]),col="blue")
lines(cumsum(portfolio3_2019[,2]),col="red")
lines(cumsum(portfolio3_2019[,3]),col="green")
lines(cumsum(portfolio3_2019[,4]),col="violet")
lines(cumsum(portfolio3_2019[,5]),col="peru")
lines(cumsum(portfolio3_2019[,6]),col="pink")
lines(cumsum(portfolio3_2019[,7]),col="purple")
lines(cumsum(portfolio3_2019[,8]),col="yellow")
lines(cumsum(portfolio3_2019[,9]),col="orange")
lines(cumsum(portfolio3_2019[,10]),col="cyan")
lines(cumsum(portfolio3_2019[,11]),col="magenta")
lines(cumsum(portfolio3_2019[,12]),col="lavender")
lines(cumsum(portfolio3_2019[,13]),col="brown")
lines(cumsum(portfolio3_2019[,14]),col="grey")
lines(cumsum(portfolio3_2019[,15]),col="aquamarine")
lines(cumsum(portfolio3_2019[,16]),col="gold")
lines(cumsum(portfolio3_2019[,17]),col="navy")
lines(cumsum(portfolio3_2019[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","kh"), lty = 1)
####################

#Random portfolio 2019 - 2020

random_portfolio_returns_2019 = portfolio_returns_2019(random_weights)
print(random_portfolio_returns_2019)

plot(cumsum(random_portfolio_returns_2019),type="l",lwd=5,
     main= "Part 3_Random_2019 - 2020")
lines(cumsum(portfolio3_2019[,1]),col="blue")
lines(cumsum(portfolio3_2019[,2]),col="red")
lines(cumsum(portfolio3_2019[,3]),col="green")
lines(cumsum(portfolio3_2019[,4]),col="violet")
lines(cumsum(portfolio3_2019[,5]),col="peru")
lines(cumsum(portfolio3_2019[,6]),col="pink")
lines(cumsum(portfolio3_2019[,7]),col="purple")
lines(cumsum(portfolio3_2019[,8]),col="yellow")
lines(cumsum(portfolio3_2019[,9]),col="orange")
lines(cumsum(portfolio3_2019[,10]),col="cyan")
lines(cumsum(portfolio3_2019[,11]),col="magenta")
lines(cumsum(portfolio3_2019[,12]),col="lavender")
lines(cumsum(portfolio3_2019[,13]),col="brown")
lines(cumsum(portfolio3_2019[,14]),col="grey")
lines(cumsum(portfolio3_2019[,15]),col="aquamarine")
lines(cumsum(portfolio3_2019[,16]),col="gold")
lines(cumsum(portfolio3_2019[,17]),col="navy")
lines(cumsum(portfolio3_2019[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","kh"), lty = 1)
####################

#2018 Comparison between Optimal/Balanced

plot(cumsum(MyPortfolio),type="l",lwd=5, xlim = c(0, 65),
     main= "2018 Optimal Returns VS 2018 Balanced")
lines(cumsum(balanced_portfolio_returns[,1]),col="blue", lwd=5)
legend('topright',legend=c("Weighted portfolio 2018",
                           "Balanced Weights 2018"), col = c("black","blue","red"), lty = 1, cex = 0.75)

#2018 Comparison between Optimal/Random

plot(cumsum(MyPortfolio),type="l",lwd=5, xlim = c(0, 65),
     main= "2018 Optimal Returns VS 2018 Random")
lines(cumsum(random_portfolio_returns_2018[,1]),col="red", lwd=5)
legend('topright',legend=c("Weighted portfolio 2018",
                           "Random Weights 2018"), col = c("black","red"), lty = 1, cex = 0.75)

#2019 Comparison between Optimal/Balanced

plot(cumsum(optimal_returns_2019),type="l",lwd=5, xlim = c(0, 65),
     main= "2019 Optimal Returns VS 2019 Balanced")
lines(cumsum(balanced_portfolio_returns_2019[,1]),col="blue", lwd=5)
legend('topright',legend=c("Weighted portfolio 2019",
                           "Balanced Weights 2019"), col = c("black","blue"), lty = 1, cex = 0.75)

#2019 Comparison between Optimal/Random

plot(cumsum(optimal_returns_2019),type="l",lwd=5, xlim = c(0, 65),
     main= "2019 Optimal Returns VS 2019 Balanced")
lines(cumsum(random_portfolio_returns_2019[,1]),col="red", lwd=5)
legend('topright',legend=c("Weighted portfolio 2019",
                           "Random Weights 2019"), col = c("black","red"), lty = 1, cex = 0.75)

#PART 4

#Min Variance portfolio calculated using built in functions
#This will build a well-diversified portfolio that consists of individually risky assets,
#which are hedged when traded together, resulting in the lowest possible risk for the rate of expected return.
#Can be used as a good measure to see the differences between portfolios with different goals

minvp1 <- minvariancePortfolio( as.timeSeries( portfolio3 ), #converting into a timeseries
                                spec = portfolioSpec(), #find the efficent frontier for specific portfolio
                                constraints = "LongOnly")

minvp1 #summary of risk and return and portfolio weights (if your trying to minimize the variance)

#Output of portfolio weights by % if your trying minimze the variance i.e 18.3% in JP morgan
# some null weight stocks

weights2 <- c(0.0000, 0.0000, 0.1915, 0.0000, 0.0000, 0.0000, 0.1963,
              0.0000, 0.0945, 0.0572, 0.1438, 0.1023,0.0000, 0.3066,
              0.0564, 0.0000, 0.2142, 0.0796 )

MinVariance_portfolio_2018 = portfolio_returns(weights2)
print(MinVariance_portfolio_2018)

plot(cumsum(MinVariance_portfolio_2018),type="l",lwd=5,
     main= "MinVariance_portfolio_2018 - 2019")
lines(cumsum(portfolio3[,1]),col="blue")
lines(cumsum(portfolio3[,2]),col="red")
lines(cumsum(portfolio3[,3]),col="green")
lines(cumsum(portfolio3[,4]),col="violet")
lines(cumsum(portfolio3[,5]),col="peru")
lines(cumsum(portfolio3[,6]),col="pink")
lines(cumsum(portfolio3[,7]),col="purple")
lines(cumsum(portfolio3[,8]),col="yellow")
lines(cumsum(portfolio3[,9]),col="orange")
lines(cumsum(portfolio3[,10]),col="cyan")
lines(cumsum(portfolio3[,11]),col="magenta")
lines(cumsum(portfolio3[,12]),col="lavender")
lines(cumsum(portfolio3[,13]),col="brown")
lines(cumsum(portfolio3[,14]),col="grey")
lines(cumsum(portfolio3[,15]),col="aquamarine")
lines(cumsum(portfolio3[,16]),col="gold")
lines(cumsum(portfolio3[,17]),col="navy")
lines(cumsum(portfolio3[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","kh"), lty = 1)
####################

plot(cumsum(MyPortfolio),type="l",lwd=5,
     main= "Optimal Portfolio 2018 VS Min Variance Portfolio 2018")
lines(cumsum(MinVariance_portfolio_2018[,1]),col="gold", lwd = 5)
legend('bottomleft',legend=c("Optimal portfolio 2018",
                             "Min Variance 2018"), col = c("black","gold"), lty = 1, cex = 0.75)

################## Max Return Portfolio #############################

####### Sharpe Function #########
sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)) #Takes away risk
  
}
#######################
##### Constraint function greater than 0 less than 1 #########
constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr +
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}
#################################
####### objective function for optimization#####
obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  
  return (-sharpe(x)+100*constraint(x))
}
###########################
### Generic Algorithm including stopping iteration when max fitness found######
library("GA")
ga_res = ga(
  # Tell the genetic algorithm that the
  # weights are real variables
  type="real-valued",
  
  # "ga" function performs maximization, so we must
  # multiply the objective function by -1
  function(x){-obj(x)},
  
  # x_i >= 0
  lower = rep(0,ncol(portfolio3)),
  
  # x_i <= 1
  upper = rep(1,ncol(portfolio3)),
  
  # Maximum number of iterations
  maxiter = 1000,
  
  # If the maximum fitness remains the same for 50
  # consecutive transactions, stop the algorithm
  run=50,
  
  # We want to see the partial results of the process
  # while it performs
  monitor=TRUE,
  
  # Seed useful for replicating the results
  seed=1
)
fitness <- function(x) 
  
  #####################


#######resulting weights in a vector###
MaxReturn = as.vector(summary(ga_res)$solution)
MaxReturn
####################

######## portfolio illustration with the bold black weighted portfolio line########

MaxReturnPortfolio = portfolio_returns(MaxReturn)
plot(cumsum(MaxReturnPortfolio),type="l",lwd=5,
     main= "Max_Return_Portfolio_2018")
lines(cumsum(portfolio3[,1]),col="blue")
lines(cumsum(portfolio3[,2]),col="red")
lines(cumsum(portfolio3[,3]),col="green")
lines(cumsum(portfolio3[,4]),col="violet")
lines(cumsum(portfolio3[,5]),col="peru")
lines(cumsum(portfolio3[,6]),col="pink")
lines(cumsum(portfolio3[,7]),col="purple")
lines(cumsum(portfolio3[,8]),col="yellow")
lines(cumsum(portfolio3[,9]),col="orange")
lines(cumsum(portfolio3[,10]),col="cyan")
lines(cumsum(portfolio3[,11]),col="magenta")
lines(cumsum(portfolio3[,12]),col="lavender")
lines(cumsum(portfolio3[,13]),col="brown")
lines(cumsum(portfolio3[,14]),col="grey")
lines(cumsum(portfolio3[,15]),col="aquamarine")
lines(cumsum(portfolio3[,16]),col="gold")
lines(cumsum(portfolio3[,17]),col="navy")
lines(cumsum(portfolio3[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","khaki3" ),
       lty=1, cex = 0.45)

plot(cumsum(MyPortfolio),type="l",lwd=5,
     main= "Optimal Portfolio 2018 VS Max Return Portfolio 2018")
lines(cumsum(MaxReturnPortfolio[,1]),col="blue", lwd = 5)
legend('topright',legend=c("Optimal portfolio 2018",
                           "Max Return 2018"), col = c("black","blue"), lty = 1, cex = 0.75)

plot(cumsum(MaxReturnPortfolio),type="l",lwd=5,
     main= "Max Return Portfolio 2018 VS Min Variance Portfolio 2018")
lines(cumsum(MinVariance_portfolio_2018[,1]),col="gold", lwd = 5)
legend('topright',legend=c("Max Return Portfolio 2018",
                           "Min Variance 2018"), col = c("blue","gold"), lty = 1, cex = 0.75)

#MINIMUM RISK PORTFOLIO

#Sharpe Function
sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (1/sqrt(var(port.returns))) #Minimises Risk
  
}
#######################
##### Constraint function greater than 0 less than 1 #########
constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr +
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}
#################################
####### objective function for optimization#####
obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  
  return (-sharpe(x)+100*constraint(x))
}
###########################
### Generic Algorithm including stopping iteration when max fitness found######
library("GA")
ga_res = ga(
  # Tell the genetic algorithm that the
  # weights are real variables
  type="real-valued",
  
  # "ga" function performs maximization, so we must
  # multiply the objective function by -1
  function(x){-obj(x)},
  
  # x_i >= 0
  lower = rep(0,ncol(portfolio3)),
  
  # x_i <= 1
  upper = rep(1,ncol(portfolio3)),
  
  # Maximum number of iterations
  maxiter = 1000,
  
  # If the maximum fitness remains the same for 50
  # consecutive transactions, stop the algorithm
  run=50,
  
  # We want to see the partial results of the process
  # while it performs
  monitor=TRUE,
  
  # Seed useful for replicating the results
  seed=1
)
fitness <- function(x) 
  
  #####################


#######resulting weights in a vector###
MinRisk = as.vector(summary(ga_res)$solution)
MinRisk
####################

######## portfolio illustration with the bold black weighted portfolio line########

MinRiskPortfolio = portfolio_returns(MinRisk)
plot(cumsum(MinRiskPortfolio),type="l",lwd=5,
     main= "Min_Risk_Portfolio_2018")
lines(cumsum(portfolio3[,1]),col="blue")
lines(cumsum(portfolio3[,2]),col="red")
lines(cumsum(portfolio3[,3]),col="green")
lines(cumsum(portfolio3[,4]),col="violet")
lines(cumsum(portfolio3[,5]),col="peru")
lines(cumsum(portfolio3[,6]),col="pink")
lines(cumsum(portfolio3[,7]),col="purple")
lines(cumsum(portfolio3[,8]),col="yellow")
lines(cumsum(portfolio3[,9]),col="orange")
lines(cumsum(portfolio3[,10]),col="cyan")
lines(cumsum(portfolio3[,11]),col="magenta")
lines(cumsum(portfolio3[,12]),col="lavender")
lines(cumsum(portfolio3[,13]),col="brown")
lines(cumsum(portfolio3[,14]),col="grey")
lines(cumsum(portfolio3[,15]),col="aquamarine")
lines(cumsum(portfolio3[,16]),col="gold")
lines(cumsum(portfolio3[,17]),col="navy")
lines(cumsum(portfolio3[,18]),col="khaki3")
legend(0,1.100,legend=c("Weighted portfolio",names(myStocks)),
       col = c("black","blue","red","green","violet","peru","pink","purple","yellow","orange",
               "cyan","magenta","lavender", "brown", "grey", "aquamarine","gold","navy","khaki3" ),
       lty=1, cex = 0.45)

plot(cumsum(MyPortfolio),type="l",lwd=5,
     main= "Optimal Portfolio 2018 VS Min Risk Portfolio 2018")
lines(cumsum(MinRiskPortfolio[,1]),col="red", lwd = 5)
legend('topright',legend=c("Optimal portfolio 2018",
                           "Min Risk 2018"), col = c("black","red"), lty = 1, cex = 0.75)

plot(cumsum(MinRiskPortfolio),type="l",lwd=5,
     main= "Min Risk Portfolio 2018 VS Min Variance Portfolio 2018")
lines(cumsum(MinVariance_portfolio_2018[,1]),col="gold", lwd = 5)
legend('topright',legend=c("Min Risk Portfolio 2018",
                           "Min Variance 2018"), col = c("red","gold"), lty = 1, cex = 0.75)

#GRAPH COMPARISONS BETWEEN OPTIMAL/MIN VARIANCE/MAX RETURN/MIN RISK

plot(cumsum(MyPortfolio),type="l",lwd=3,
     main= "My Portfolio 2018 VS Min Variance Portfolio 2018
     VS Max Return 2018 VS Min Risk 2018")
lines(cumsum(MinVariance_portfolio_2018[,1]),col="gold", lwd = 3)
lines(cumsum(MaxReturnPortfolio[,1]),col="blue", lwd = 3)
lines(cumsum(MinRiskPortfolio[,1]),col="red", lwd = 3)
legend('topright',legend=c("My Portfolio 2018",
                           "Min Variance 2018",
                           "Max Return 2018",
                           "Min Risk"), col = c("black", "gold", "blue", "red"), lty = 1, cex = 0.75)

######### MATH calculations ##############

apply( portfolio3, 2, mean ) # Avergae weekly returns for the year
apply( portfolio3, 2, median ) # Median for each stock over the year
cov( portfolio3 ) # Covariance 
cor( portfolio3 ) # Correlation 
SharpeRatio(portfolio3) #Std Dev and Var/Er Sharpe


#optimzimizing portfolio: weights, %, risk etc..

#####  weights equal to 1( 1/18), values between 0 - 1 manual input ####

weights1 <- c(0.055555555555556,0.055555555555556,0.055555555555556,
              0.055555555555556,0.055555555555556,0.055555555555556,
              0.055555555555556,0.055555555555556,0.055555555555556,
              0.055555555555556,0.055555555555556,0.055555555555556,
              0.055555555555556,0.055555555555556,0.055555555555556,
              0.055555555555556,0.055555555555556,0.055555555555556)
unweightmean <- apply( portfolio3, 2, mean ) 


#expected return under the above weights/ weighted peformance of portfolio
weightmean1 <- unweightmean%*%weights1
weightmean1
#Weighted Risk
weightrisk1 <- t( weights1 )%*%cov( portfolio3 )%*%weights1
weightrisk1 # The risk associated with portfolio 3 with the equal weights 


### Effiencent frontier portfolio allocation 
#####minimum variance portfolio (smallest variant i can get out of portfolio3) #####

minvp1 <- minvariancePortfolio( as.timeSeries( portfolio3 ), #converting into a timeseries
                                spec = portfolioSpec(), #find the efficent frontier for specific portfolio
                                constraints = "LongOnly")

minvp1 #summary of risk and return and portfolio weights (if your trying to minimize the variance)

#Output of portfolio weights by % if your trying minimze the variance i.e 18.3% in JP morgan
# some null weight stocks

weights2 <- c(0.0000, 0.0000, 0.1803, 0.0000, 0.0000, 0.0000, 0.1963,
              0.0000, 0.0000, 0.0008, 0.1438, 0.1023,0.0000, 0.2014,
              0.0000, 0.0000, 0.1092, 0.0659 )

#calculation of weighted mean
weightmean2 <- unweightmean%*%weights2
#first portfolio equal weights
weightmean1
#optimized weights via output
weightmean2
#calculation of weighted risk
weightrisk2 <- t( weights2 )%*%cov( portfolio3 )%*%weights2
#comparing equal weight portfolio to minimze variance portfolio
#risk level first portfolio- much higher than portfolio 2
weightrisk1
#portfolio 2 risk level- much less than original
weightrisk2