
#week 2 assignment part2 , 
#a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. 
#by Alimohammad Pourkhesalian


rm(list = ls()) #let's clear the memory

complete <- function(directory, id= 1:332){
  file_names<- paste( 
    c(paste
      ('00', 1:9, sep=''),paste(
        '0', 10:99, sep=''),paste(
          100:332, sep='')),'.csv', sep = '') #making a list of all file names to be able to use read.csv
  comp.cases <- data.frame(id= rep(NA, length(id)), nobs= rep(NA, length(id)))
  for (i in 1 : length(id)) {    
    comp.cases$id[i] <- id[i]
    comp.cases$nobs[i] <- sum(complete.cases( read.csv(paste(directory,'/',file_names[id[i]], sep=''))))
  }
  return(comp.cases)
}



complete('specdata', 1)
complete('specdata', c(2,4,8,10,12))
complete('specdata', 30:25)
complete('specdata', 3)


set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
