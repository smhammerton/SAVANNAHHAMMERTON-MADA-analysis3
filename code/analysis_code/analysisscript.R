###############################
# analysis script
#

#load needed packages. make sure they are installed.
library(tidyverse) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels) #for model fitting 

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data and set to object 
mydata <- readRDS(data_location)





#save figure
figure_file = here("results","resultfigure.png")
ggsave(filename = figure_file, plot=p1) 

######################################
#Data fitting/statistical analysis
######################################

# fit linear model
lmfit <- lm(Weight ~ Height, mydata)  

# place results from fit into a data frame with the tidy function
lmtable <- broom::tidy(lmfit)

#look at fit results
print(lmtable)

# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(lmtable, file = table_file)

  