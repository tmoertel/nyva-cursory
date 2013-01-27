library(ggplot2)
library(reshape2)
library(scales)
library(plyr)

## Data source:  2008-2010 NYC Teacher Performance Data
## http://www.ny1.com/content/top_stories/156599/now-available--2007-2010-nyc-teacher-performance-data#doereports


## helper function to load data set and remove hard-to-interpret rows
read_va_data <- function(file_name) {
  ds <- read.csv(file_name, na.strings="")
  ds <- subset(ds, exp_group != "Unknown" & exp_group != "Co-Teaching")
  ds
}

## these variables identify comparable teacher observations
id_vars <- c("subject", "grade", "dbn",
             "teacher_name_first_1", "teacher_name_last_1")

## merge 2008-09 and 2009-10 data sets on comparables
ds09 <- read_va_data("data/TDI_20082009_FOIL_Press.csv")
ds10 <- read_va_data("data/TDI_20092010_FOIL_Press.csv")
ds_cmp <- merge(ds09, ds10, by=id_vars)


## prepare a scatterplot of one year's value-added vs. the next's
p <- ggplot(ds_cmp, aes(x=va_0809, y=va_0910))
p <- p + geom_jitter(shape=".") + geom_smooth()
ggsave("nyva_scatterplot.png", plot=p)


## plot histogram of the change in value-added from one year to next
p <- qplot(va_0910 - va_0809, data=ds_cmp, binwidth=0.05)
ggsave("nyva_yearly_va_change_histogram.png", p)


## simple linear model:  this year's value-added predicts next year's
m1 <- lm(va_0910 ~ va_0809, data=ds_cmp)
summary(m1)
