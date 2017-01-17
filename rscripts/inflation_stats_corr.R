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

## Loads inflation data from the two metrics into two data frames
cpi_data <- read.table("http://127.0.0.1/CPALTT01USQ661S.csv", header = TRUE, sep = ",")
gdp_def_data <- read.table("http://127.0.0.1/GDPDEF.csv", header = TRUE, sep = ",")

## Asserts equivalency of size between the two datasets
length(cpi_data) == length(gdp_def_data)

## Merges data horizontally by date
merged_data <- merge(cpi_data, gdp_def_data, by = "DATE")

## Computes correlation coefficient between CPI and GDP
cpi <- merged_data$CPALTT01USQ661S_NBD20090101
gdp_def <- merged_data$GDPDEF_NBD20090101

corr_cpi_gdp_def <- cor(cpi, gdp_def)

corr_cpi_gdp_def

## Generates scatter of CPI vs GDP Deflator
scatter <- ggplot(merged_data, aes(x=CPALTT01USQ661S_NBD20090101, y=GDPDEF_NBD20090101)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, se=FALSE)

scatter