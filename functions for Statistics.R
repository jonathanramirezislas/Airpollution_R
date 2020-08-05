pollutantmean <- function(directory, pollutant, id=1:332){
  
  
  # Aux variables
  file_list <- list.files(directory, pattern = ".csv", full.names=TRUE)
  values <- numeric()
  #For each id passed as parameter:
  for (i in id) {
    data <- read.csv(file_list[i])
    
    values <- c(values, data[[pollutant]])
  }
  # Calculate the mean and return it
  mean(values, na.rm = TRUE)
}


complete <- function(directory, id=1:332){
  
  fileList <- list.files(directory, pattern = ".csv", full.names=TRUE)
  nobs <- numeric()
  
  # id  servira para recorrer cada archivo csv
  for (i in id) {
    #leemos el archivo csv
    data <- read.csv(fileList[i])
    #comple.cases verifica si la row esta completa, ejemplo de salida 1 0 1 0 1 por cada row
    nobs <- c(nobs, sum(complete.cases(data)))
    
  }
  data.frame(id, nobs)
  
}
                  #threshol as defualt 0
corr <- function(directory, threshold = 0) {
  filesList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  cors <- numeric()
  for (i in 1:332) {
    data <- read.csv(filesList[i])
    if (sum(complete.cases(data)) > threshold) {

                      # cor function es la relacion reciproca entre dos o mas funciones o fenomenos, complete.obs omitira valores na
      cors <- c(cors, cor(data[["sulfate"]], data[["nitrate"]], use = "complete.obs"))
    }
  }
  cors
}


seedata <- function(directory, threshold = 0) {
    filesList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  for (i in 1:332) {
    data <- read.csv(filesList[i])
    complecases <- sum(complete.cases(data))
    
  }
  data
  
}
#pollutantmean("C:/Users/jonaathan/Desktop/R/specdata", "sulfate")

#complete("C:/Users/jonaathan/Desktop/R/specdata", c(1, 5)) 