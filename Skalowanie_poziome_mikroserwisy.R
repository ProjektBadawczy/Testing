#######################################
#           CLEAN WORKSPACE           #
#######################################
rm(list = ls())
cat("\014")
# dev.off(dev.list()["RStudioGD"])


#######################################
#               IMPORTS               #
#######################################
library(ggplot2)
library(dplyr)


#######################################
#              FUNCTIONS              #
#######################################
getBenchmarkDF <- function(df, warmup, measurement_time) {
  df$responceTimeStamp <- df$timeStamp + df$elapsed # when the last chunk of the response has been received
  estimated_benchmark_start <- min(df$timeStamp) + warmup * 1000
  tmp <- df[df$responceTimeStamp>estimated_benchmark_start, 'responceTimeStamp']
  benchmark_start <- min(tmp)
  benchmark_end <- benchmark_start + measurement_time * 1000
  return( df[df$responceTimeStamp>=benchmark_start & df$responceTimeStamp<=benchmark_end,] )
}

getStat <- function(df, total_time, max_latency=200, drop_unsuccessful=TRUE) {
  all_requests <- nrow(df)
  if(drop_unsuccessful) df <- df[df$responseCode==200,]
  successful_requests <- nrow(df)
  error_rates <- (all_requests-successful_requests) / all_requests
  median_latency <- median(df$Latency)
  q90_latency <- quantile(df$Latency, probs=c(0.9))
  # if(max_latency>0) df <- df[df$Latency<=max_latency,]
  throughput <- nrow(df) / total_time
  result <- list(Throughput=throughput, Median_latency=median_latency, Q90_latency=q90_latency, Error_rates=error_rates)
  return(result)
}

collectData <- function(baseDir) {
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
      
      statsDF[nrow(statsDF) + 1,] <- c(iteration, users, getStat(df, total_time))
    }
  }
  df <- aggregate(cbind(Throughput, Median_latency, Q90_latency, Error_rates) ~ Users, statsDF, FUN = median)
  return(df)
}


#######################################
#           SETUP & INPUTS            #
#######################################
delta <- 7
total_time <- 5 
max_response_time <- 200  # [ms]


#######################################
#            COLLECT DATA             #
#######################################
label_title <- "Liczba instancji"

df1 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.large-edmonds-karp/")
df2 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.large-2-instances-edmonds-karp/")
df20 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.large-4-instances-edmonds-karp/")

df_combined_1 <- rbind(
  transform(df1, Instancja = "1", Technologia = "Java"),
  transform(df2, Instancja = "2", Technologia = "Java"),
  transform(df20, Instancja = "4", Technologia = "Java")
)

df3 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.large-edmonds-karp/")
df4 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.large-2-instances-edmonds-karp/")
df40 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.large-4-instances-edmonds-karp/")

df_combined_2 <- rbind(
  transform(df3, Instancja = "1", Technologia = "NET"),
  transform(df4, Instancja = "2", Technologia = "NET"),
  transform(df40, Instancja = "4", Technologia = "NET")
)

df6 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.large-edmonds-karp/")
df7 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.large-2-instances-edmonds-karp/")
df70 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.large-4-instances-edmonds-karp/")

df_combined_3 <- rbind(
  transform(df6, Instancja = "1", Technologia = "NodeJS"),
  transform(df7, Instancja = "2", Technologia = "NodeJS"),
  transform(df70, Instancja = "4", Technologia = "NodeJS")
)

df_combined <- rbind(df_combined_1, df_combined_2, df_combined_3)


max_throughput <- df_combined %>%
  group_by(Instancja, Technologia) %>%
  filter(Throughput == max(Throughput)) %>%
  slice(1)


#######################################
#                 PLOT                #
#######################################

# p1 <- ggplot(df_combined, aes(x = Users, y = Throughput, color = Instancja, shape = Instancja, fill = Instancja)) +
ggplot(df_combined, aes(x = Users, y = Throughput, color = Instancja, fill = Instancja)) +
  geom_point(size = 2) + 
  geom_line() +
  geom_hline(data = max_throughput, linetype = "dashed", color = "gray", aes(yintercept = Throughput)) +
  geom_text(data = df_combined %>% group_by(Instancja, Technologia) %>% filter(Throughput == max(Throughput)) %>% slice(1),
            aes(x = Users, y = Throughput, label = round(Throughput, 1.2), vjust = -0.6), show.legend = FALSE, check_overlap = TRUE) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_color_manual(values = c("#0DBBF7", "#FF5B33", "#D426FF")) +
  scale_fill_manual(values =  c("#0DBBF7",  "#FF5B33","#D426FF")) +
  labs(x = "Liczba równoległych użytkowników", y = "Przepustowość [liczb. zap. na sekundę]", color = label_title, shape = label_title, fill = label_title) +
  theme(legend.position = "bottom") +
  facet_wrap(~factor(Technologia, c("Java", "NET", "NodeJS")), ncol = 1, scales = "free_y", strip.position = "top") +
  scale_y_continuous(limits = c(0, 700))

