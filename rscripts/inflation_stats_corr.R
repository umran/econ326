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
## US
us_cpi_data <- read.table("http://127.0.0.1/CPALTT01USQ661S.csv", header = TRUE, sep = ",")
us_gdp_def_data <- read.table("http://127.0.0.1/GDPDEF.csv", header = TRUE, sep = ",")
## Canada
can_cpi_data <- read.table("http://127.0.0.1/CANCPIALLQINMEI.csv", header = TRUE, sep = ",")
can_gdp_def_data <- read.table("http://127.0.0.1/CANGDPDEFQISMEI.csv", header = TRUE, sep = ",")

## Merges data horizontally by date
## US
us_merged_data <- merge(us_cpi_data, us_gdp_def_data, by = "DATE")
names(us_merged_data) <- c("DATE", "CPI", "GDPDEF")
## Canada
can_merged_data <- merge(can_cpi_data, can_gdp_def_data, by = "DATE")
names(can_merged_data) <- c("DATE", "CPI", "GDPDEF")

## Computes correlation coefficient between CPI and GDP
## US
us_cpi <- us_merged_data$CPI
us_gdp_def <- us_merged_data$GDPDEF
us_corr_cpi_gdp_def <- cor(us_cpi, us_gdp_def)
us_corr_cpi_gdp_def
## Canada
can_cpi <- can_merged_data$CPI
can_gdp_def <- can_merged_data$GDPDEF
can_corr_cpi_gdp_def <- cor(can_cpi, can_gdp_def)
can_corr_cpi_gdp_def

## Generates scatter of CPI vs GDP Deflator
## US
us_scatter <- ggplot(us_merged_data, aes(x=CPI, y=GDPDEF)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=FALSE) +
  labs(title="United States")
us_scatter
## Canada
can_scatter <- ggplot(can_merged_data, aes(x=CPI, y=GDPDEF)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=FALSE) +
  labs(title="Canada")
can_scatter