## Defines helper function to download and/or load
## the necessary libraries
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

load.fun("ggplot2")

## Loads inflation data from the two metrics into two data frames for both the US and Canada
cpi_data <- read.table("http://127.0.0.1/CANCPIALLQINMEI.csv", header = TRUE, sep = ",")
gdpdef_data <- read.table("http://127.0.0.1/CANGDPDEFQISMEI.csv", header = TRUE, sep = ",")

## Makes column names more readable
names(cpi_data) <- c("DATE", "CPI")
names(gdpdef_data) <- c("DATE", "GDPDEF")

inflation_rates_cpi <- data.frame()
inflation_rates_gdpdef <- data.frame()

## Calculates interest rates based on cpi
for(i in 2:length(cpi_data$CPI)) {
  period <- paste(as.character(cpi_data$DATE[i-1]), as.character(cpi_data$DATE[i]), sep=" to ")
  pi <- ((cpi_data$CPI[i]/cpi_data$CPI[i-1])-1)*100
  
  inflation_rates_cpi[i-1, 1] <- period
  inflation_rates_cpi[i-1, 2] <- pi
}

## Gives names to columns
names(inflation_rates_cpi) <- c("PERIOD","INFLATION")

## Calculates interest rates based on gdp deflator
for(i in 2:length(gdpdef_data$GDPDEF)) {
  period <- paste(as.character(gdpdef_data$DATE[i-1]), as.character(gdpdef_data$DATE[i]), sep=" to ")
  pi <- ((gdpdef_data$GDPDEF[i]/gdpdef_data$GDPDEF[i-1])-1)*100
  
  inflation_rates_gdpdef[i-1, 1] <- period
  inflation_rates_gdpdef[i-1, 2] <- pi
}

## Gives names to columns
names(inflation_rates_gdpdef) <- c("PERIOD","INFLATION")

## Generates timeseries graph of inflation as measured by the CPI
ggplot(inflation_rates_cpi, aes(PERIOD, INFLATION, group = 1)) + 
  geom_line() +
  xlab("Time Period") + 
  ylab("Rate of Inflation (%)") +
  labs(title="Inflation as Measured by the CPI for Canada")

## Generates timeseries graph of inflation as measured by the GDP deflator
ggplot(inflation_rates_gdpdef, aes(PERIOD, INFLATION, group = 1)) + 
  geom_line() +
  xlab("Time Period") + 
  ylab("Rate of Inflation (%)") +
  labs(title="Inflation as Measured by the GDP Deflator for Canada")

## Computes the correlation coefficient between the two indicators of inflation
cor(inflation_rates_cpi$INFLATION, inflation_rates_gdpdef$INFLATION)