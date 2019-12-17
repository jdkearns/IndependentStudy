# Creating the combined player participation file
# goal: create script that can take the xml from link given 
#       by sportradar and put into the three years combined, then one full dataset
# John Kearns
# 10/18/19

# input: unlisted xml data, NFLmerge function
# output: datasets compiling plays from every season

# install.packages('XML')
# install.packages('gtools')
# install.packages('XML')
# install.packages('gtools')
library(XML)
library(dplyr)
library(tidyverse)
library(gtools)
'%!in%' <- function(x,y)!('%in%'(x,y))
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

# bring in NFLmerge
# source("~/Year 4 - William and Mary/Semester 1/Indep Study/function.R")

# run for the 2017 season (already did, saved as playerpart_2017)

# run for the 2018 season
playerpart_2018 = NFLmerge('2018')

# run for the 2016 season
playerpart_2016 = NFLmerge('2016')

# merge all into one data frame
playerpart_a = merge(playerpart_2017,playerpart_2016,all=TRUE,by=intersect(names(playerpart_2016),names(playerpart_2017)))
playerpart_all = merge(playerpart_a,playerpart_2018,all=TRUE,by=intersect(names(playerpart_a),names(playerpart_2018)))
playerpart_all[is.na(playerpart_all)] = 0