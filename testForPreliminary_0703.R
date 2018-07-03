###############################################################################
######################Test for forum scraping##################################
###################Clean, weight and topic model###############################
###############################################################################

install.packages("rjson")
library("rjson")
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

###############################################################################
#read in test file
json_file <- "C:/Users/jchen/Downloads/testF25.json"
 
json_data <- fromJSON(file=json_file)

training_folder = "Z:/2018/NLP/training0703/"

#define a function to read all files from a folder into a data frame
read_folder = function(infolder){
  data_frame(file = dir(infolder, full.names=T)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

#use unnest() and map() to apply read_folder to each subfolder
raw_text = data_frame(folder = dir(training_folder, full.names=T)) %>%
  unnest(map(folder, read_folder)) %>%
  transmute(newgroup = basename(folder), id, text)
  

raw_text = json_data

#need to be more anchived
raw_text2 =   data.frame(matrix(unlist(raw_text), nrow=132, byrow=T))

df$text <- gsub('(?=\\b\\pL{6,}).{5}\\K\\pL*', '', df$text, perl=T)
df



################
#preprocessing text

library(stringr)

#must occur after the first occurrence of an empty lines and before the first 
#occurrence of a line starting with --

cleaned_text <- rawtext %>%
  group_by()





  



