#Coursera Data Science Course
#Course 2 R programming

#by Alimohammad Pourkhesalian
#
#function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.
#



#week 2 assignment part1 , pollutantmean function  

rm(list = ls())


#directory <- "C:/Users/pou046/Dropbox/DataScienceCourse/RProgramming/week2 assignment/datasciencecoursera/specdata/"      
#pollutant <- 'nitrate' #or nitrate
#id<- c(1, 10, 85, 124, 332)

#id<- 23

pollutantmean <- function(directory, pollutant, id= 1:332){
  
  file_names<- paste( 
    c(paste
      ('00', 1:9, sep=''),paste(
        '0', 10:99, sep=''),paste(
          100:332, sep='')),'.csv', sep = '') #making a list of all file names to be able to use read.csv
  file_names_dir_list <- paste(
    directory,'/', file_names[id], sep = '') # making a list of files to be read by read.csv
  pollutatn.df <- read.csv(
    file_names_dir_list[1]) # making the dataframe with the first file in it
  if( length(id)>1){
    for (i in 2: length(id)){
      pollutatn.df<- rbind(
        pollutatn.df, read.csv(
          file_names_dir_list[i])) #adding rest of the file list to the dataframe
    }      
  }
  mean(
    pollutatn.df[[pollutant]][!is.na(pollutatn.df[[pollutant]])]) #ignoring NAs and taking mean
}

pollutantmean(directory = 'specdata', pollutant = 'sulfate', id=1:10)
pollutantmean(directory = 'specdata', pollutant = 'nitrate', id=70:72)
pollutantmean(directory = 'specdata', pollutant = 'nitrate', id=23)
