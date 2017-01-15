## Creates a clean canvas
rm(list=ls())
graphics.off()

## Defines helper function to download and/or load the necessary libraries
load.fun <- function(x) {
  x <- as.character(substitute(x))
  if(isTRUE(x %in% .packages(all.available=TRUE))) {
    eval(parse(text=paste("library(", x, ")", sep="")))
  } else {
    #update.packages()  ## good idea, but may take some time. can
    ## usually be safely skipped
    eval(parse(text=paste("install.packages('", x, "')", sep="")))
    eval(parse(text=paste("library(", x, ")", sep="")))
  }
}

## Loads necessary libraries by calling the helper function with library names given as arguments
load.fun(ggplot2)
load.fun(reshape)

## Defines helper functions to avoid repetitions in code

## Function to generate an arbitrary number of random numbers 
## from an arbitrary distribution specified in the arguments as
## size and distribution. Takes an optional argument, parameters,
## to specify distribution parameters such as mean and sd in the
# case of normal distribution
rdist <- function(size, distribution, parameters) {
  if (distribution == "normal") {
    mean <- parameters[1]
    sd <- parameters[2]
    
    rnorm(size, mean=mean, sd=sd)
  }
  
  else if (distribution == "uniform") {
    runif(size)
  }
}

## Function to generate data from a given distribution
## Returns a list containing two dataframes consisting of
## sample means and sample variances
## for all N sample sizes in long format. 
## Takes as arguments: N = different sample sizes, 
## simulations = number of simulations,
## distribution = type of distribution,
## distribution_parameters = distribution parameters; in the case of
## a normal distribution this is an array containing mean and sd
genData <- function(N, simulations, distribution, distribution_parameters) {
  
  ## Defines datasets
  means <- matrix(NA, nrow = simulations, ncol = length(N))
  vars <- means
  
  ## Populates data sets
  for(i in 1:length(N)) {
    n <- N[i]
    dat <- matrix(rdist(n*simulations, distribution, distribution_parameters), nrow = simulations, ncol = n)
    
    means[,i] <- apply(dat, 1, mean)
    vars[,i] <- apply(dat, 1, var)
  }
  
  return(list(means=means,vars=vars))
}

## Function to format generated data, namely to group
## values by sample size and to reshape it into long format
longify <- function(data) {
  df <- data.frame(data)
  names(df) <- sprintf("N%d",N)
  df <- melt(df,variable_name="sampleSize")
  
  return(df)
}

genPlots <- function(data, title) {
  data <- longify(data)
  
  cltPlot <- ggplot(data=data, aes(x=value, fill=sampleSize)) +
    geom_histogram(alpha=0.8, position="identity", bins=100) +
    scale_x_continuous(name="sample mean") +
    scale_fill_brewer(type="div",palette="RdYlGn",
                      name="N",label=N) +
    labs(title=title) +
    facet_grid(sampleSize ~ . ) + theme_minimal()
  
  return(cltPlot)
}

doAll <- function(N, simulations, distribution, distribution_parameters) {
  
  if(distribution == "normal"){
    titleMeans = "Histogram of sample means from normal distribution"
    titleVars = "Histogram of sample variances from normal distribution"
  } else if(distribution == "uniform") {
    titleMeans = "Histogram of sample means from uniform distribution"
    titleVars = "Histogram of sample variances from uniform distribution"
  }
  
  ## Generates data from specified distribution
  data <- genData(N, simulations, distribution, distribution_parameters)
  means <- data["means"]
  vars <- data["vars"]
  
  ## Generates Histograms of sample means and 
  ## variances of samples from specified distribution
  meansHistogram <- genPlots(means, titleMeans)
  varsHistogram <- genPlots(vars, titleVars)
  
  ## Returns the generated histograms
  return(list(means=meansHistogram, vars=varsHistogram))
}

## Defines different sample sizes
## and the number of simulations
N <- c(8,32,128)
simulations <- 5000

## Generate graphs of means and variances of samples generated from
## normal distribution
plotsNormal <- doAll(N, simulations, "normal", c(0,1))
plotsNormal["means"]
plotsNormal["vars"]

## Plot graphs of means and variances of samples generated from
## uniform distribution
plotsUniform <- doAll(N, simulations, "uniform")
plotsUniform["means"]
plotsUniform["vars"]