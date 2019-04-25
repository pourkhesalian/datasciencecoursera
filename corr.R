corr<- function(directory, threshold= 0){        
    #making a list of all file names to be able to use read.csv
  file_names<- paste( 
    c(paste
      ('00', 1:9, sep=''),paste(
        '0', 10:99, sep=''),paste(
          100:332, sep='')),'.csv', sep = '') 
  comp.cases <- data.frame(id= rep(NA, 332), nobs= rep(NA, 332)) #a dataframe showing the number of CC
  for (i in 1 : 332) {    
    comp.cases$id[i] <- i
    comp.cases$nobs[i] <- sum(complete.cases( read.csv(paste(directory,'/',file_names[i], sep=''))))
  }
  comp.cases.df <- comp.cases
  if (any(comp.cases.df$nobs>threshold)){
          
          
          comp.files.list <- paste(
            directory,'/',file_names[comp.cases.df$id[comp.cases.df$nobs>threshold]], sep = '')
          corr.vec<- rep(NA, length(comp.files.list))
        
                    for (i in 1 : length(comp.files.list)) {
                      comp.file.df<- read.csv(comp.files.list[i])
                      corr.vec[i]<- cor(
                        comp.file.df$sulfate[complete.cases(comp.file.df)],
                        comp.file.df$nitrate[complete.cases(comp.file.df)])
                      
                    }
   } else { corr.vec<- comp.cases.df$id[comp.cases.df$nobs>threshold]}
  return(corr.vec)
}

cr<-corr('specdata', 150)

summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <-corr('specdata')
summary(cr)
length(cr)
