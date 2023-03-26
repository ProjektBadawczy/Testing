library(ggplot2)

#tutaj metodologia liczenia ktora omawialem na spotkaniu - "liczymy responce, ktore przyszly"
getBenchmarkDF <- function(df, delta=delta, total_time=total_time) {
  df$responce_end_timeStamp <- df$timeStamp + df$elapsed # when the last chunk of the response has been received
  estimated_benchmark_start <- min(df$timeStamp) + delta*1000
  tmp <- df[df$responce_end_timeStamp>estimated_benchmark_start, 'responce_end_timeStamp']
  benchmark_start <- min(tmp)
  benchmark_end <- benchmark_start + total_time*1000
  return( df[df$responce_end_timeStamp>=benchmark_start & df$responce_end_timeStamp<=benchmark_end,] )
}

#tutaj metodologia liczenia zgodna z dokumentacja Jmeter
getBenchmarkJmeterDF <- function(df, delta, total_time) {
  df$responce_end_timeStamp <- df$timeStamp + df$elapsed # when the last chunk of the response has been received
  estimated_benchmark_start <- min(df$timeStamp) + delta*1000
  tmp <- df[df$timeStamp>estimated_benchmark_start, 'timeStamp']
  benchmark_start <- min(tmp)
  benchmark_end <- benchmark_start + total_time*1000
  return( df[df$timeStamp>=benchmark_start & df$responce_end_timeStamp<=benchmark_end,] )
}

getStat <- function(df, total_time, max_latency=200, drop_unsuccessful=TRUE) {
  all_requests <- nrow(df)
  if(drop_unsuccessful) df <- df[df$responseCode==200,]
  successful_requests <- nrow(df)
  error_rates <- (all_requests-successful_requests) / all_requests
  median_latency <- median(df$Latency)
  q90_latency <- quantile(df$Latency, probs=c(0.9))
  if(max_latency>0) df <- df[df$Latency<=max_latency,]
  throughput <- nrow(df) / total_time
  result <- list(Throughput=throughput, Median_latency=median_latency, Q90_latency=q90_latency, Error_rates=error_rates)
  return(result)
}

#Tej funkcji nie uzywam, ale latwiejsza do zrozumienia niz getStat
getThroughput <- function(df, total_time, max_latency=200, drop_unsuccessful=TRUE) {
  if(drop_unsuccessful) df <- df[df$responseCode==200,]
  if(max_latency>0) df <- df[df$Latency<=max_latency,]
  throughput <- nrow(df) / total_time
  return(throughput)
}


# W Jmeter duration ustawiamy na 14
delta <- 7
total_time <- 5 

#W tym katalogu musza byc podkatalogi z iteracjami wygenerowane przez run_Jmeter.bat
baseDir <- "D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Testing"

columns = c("Iteration","Users","Throughput","Median_latency", "Q90_latency", "Error_rates") 
statsDF = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(statsDF) = columns

iterationDirectories <- list.dirs(baseDir, full.names=FALSE)[-1]
for(iteration in iterationDirectories) {
  csvFiles <- list.files(file.path(baseDir, iteration), pattern="*.csv", full.names=FALSE)
  for(fileName in csvFiles) {
    # We assume that each files starts with "users_"
    users <- strtoi( substr(fileName, 7, nchar(fileName)-4) )
    path <- file.path(baseDir, iteration, fileName)
    data <- read.csv(path)
    min_timeStamp <- min(data$timeStamp)
    data$timeStamp <- data$timeStamp-min_timeStamp
    df <- getBenchmarkDF(data, delta, total_time)
    # df <- getBenchmarkJmeterDF(data, delta, total_time)
    # throughput <- getThroughput(df, total_time)
    # print(eval(sprintf("iteration: %s, #users: %d throughput: %.1f", iteration, users, round(throughput,1))))
    
    #dodajemy wiersz na koncu DF
    statsDF[nrow(statsDF) + 1,] <- c(iteration, users, getStat(df, total_time))
  }
}

#Najpierw patrzymy jak wyglada kazda iteracja i tam gdzie dla konkretnej liczby userow pomiar odstaje to ten pojedynczy pomiar powtarzamy i podmieniamy odpowiednia csv'ke
ggplot(statsDF, aes(x=Users, y=Throughput)) + geom_point() + facet_wrap(~Iteration)

df <- aggregate(cbind(Throughput, Median_latency, Q90_latency, Error_rates) ~ Users, statsDF, FUN = median)

ggplot(df, aes(x=Users, y=Throughput)) + geom_point()

