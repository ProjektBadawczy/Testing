#######################################
#           CLEAN WORKSPACE           #
#######################################
rm(list = ls())
cat("\014")


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
label_title <- "Technologia i rodzaj algorytmu"

df1 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/monolit/m4.large-edmonds-karp/")
df2 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/monolit/m4.large-push-relabel/")
df3 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/monolit/m4.large-edmonds-karp/")
df4 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/monolit/m4.large-push-relabel/")
df5 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/monolit/m4.large-edmonds-karp/")
df6 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/monolit/m4.large-push-relabel/")

df_combined_1 <- rbind(
  transform(df1, Instancja = "Java - Edmonds-Karp"),
  transform(df2, Instancja = "Java - Push-relabel"),
  transform(df3, Instancja = "NET - Edmonds-Karp"),
  transform(df4, Instancja = "NET - Push-relabel"),
  transform(df5, Instancja = "NodeJS - Edmonds-Karp"),
  transform(df6, Instancja = "NodeJS - Push-relabel")
)

df7 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.large-edmonds-karp/")
df8 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.large-push-relabel/")
df9 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.large-edmonds-karp/")
df10 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.large-push-relabel/")
df11 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.large-edmonds-karp/")
df12 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.large-push-relabel/")

df_combined_2 <- rbind(
  transform(df7, Instancja = "Java - Edmonds-Karp"),
  transform(df8, Instancja = "Java - Push-relabel"),
  transform(df9, Instancja = "NET - Edmonds-Karp"),
  transform(df10, Instancja = "NET - Push-relabel"),
  transform(df11, Instancja = "NodeJS - Edmonds-Karp"),
  transform(df12, Instancja = "NodeJS - Push-relabel")
)

df_combined_1$Grupa <- "Monolit"
df_combined_2$Grupa <- "Mikroserwisy"
df_combined <- rbind(df_combined_1, df_combined_2)


max_throughput <- df_combined %>%
  group_by(Instancja, Grupa) %>%
  filter(Throughput == max(Throughput)) %>%
  slice(1)



#######################################
#                 PLOT                #
#######################################
ggplot(df_combined, aes(x = Users, y = Throughput, color = Instancja, shape = Instancja, fill = Instancja)) +
  geom_point(size = 2) + 
  geom_line() +
  geom_hline(data = max_throughput, linetype = "dashed", color = "gray", aes(yintercept = Throughput)) +
  geom_text(data = df_combined %>% group_by(Instancja, Grupa) %>% filter(Throughput == max(Throughput)) %>% slice(1),
            aes(x = Users, y = Throughput, label = round(Throughput, 1.2), vjust = -0.7), show.legend = FALSE, check_overlap = TRUE) +
  scale_shape_manual(values = c(22, 22, 21, 21, 25, 25)) +
  scale_color_manual(values = c("#0D4F91", "#1F77B4", "#821818", "#D62728", "#1E6721", "#2CA02C")) +
  scale_fill_manual(values =  c("#0D4F91", "white",   "#821818", "white",   "#1E6721", "white")) +
  labs(x = "Liczba równoległych userów", y = "Przepustowość [zap. na sekundę]", color = label_title, shape = label_title, fill = label_title) +
  theme(legend.position = "bottom") +
  facet_grid(.~Grupa)


geom_text_data <- df_combined_1 %>% group_by(Instancja, Grupa) %>% filter(Throughput == max(Throughput)) %>% slice(1)
geom_hline_data <- max_throughput %>% filter(Grupa == "Monolit")

ggplot(df_combined_1, aes(x = Users, y = Throughput, color = Instancja, shape = Instancja, fill = Instancja)) +
  geom_point(size = 2) + 
  geom_line() +
  geom_hline(data = geom_hline_data, linetype = "dashed", color = "gray", aes(yintercept = Throughput)) +
  geom_text(data = geom_text_data,
            aes(x = Users, y = Throughput, label = round(Throughput, 1.2), vjust = -1), show.legend = FALSE, check_overlap = TRUE) +
  scale_shape_manual(values = c(22, 22, 21, 21, 25, 25)) +
  scale_color_manual(values = c("#0D4F91", "#1F77B4", "#821818", "#D62728", "#1E6721", "#2CA02C")) +
  scale_fill_manual(values =  c("#0D4F91", "white",   "#821818", "white",   "#1E6721", "white")) +
  labs(x = "Liczba równoległych userów", y = "Przepustowość [zap. na sekundę]", color = label_title, shape = label_title, fill = label_title)


geom_text_data <- df_combined_2 %>% group_by(Instancja, Grupa) %>% filter(Throughput == max(Throughput)) %>% slice(1)
geom_hline_data <- max_throughput %>% filter(Grupa == "Mikroserwisy")

ggplot(df_combined_2, aes(x = Users, y = Throughput, color = Instancja, shape = Instancja, fill = Instancja)) +
  geom_point(size = 2) + 
  geom_line() +
  geom_hline(data = geom_hline_data, linetype = "dashed", color = "gray", aes(yintercept = Throughput)) +
  geom_text(data = geom_text_data,
            aes(x = Users, y = Throughput, label = round(Throughput, 1.2), vjust = -1), show.legend = FALSE, check_overlap = TRUE) +
  scale_shape_manual(values = c(22, 22, 21, 21, 25, 25)) +
  scale_color_manual(values = c("#0D4F91", "#1F77B4", "#821818", "#D62728", "#1E6721", "#2CA02C")) +
  scale_fill_manual(values =  c("#0D4F91", "white",   "#821818", "white",   "#1E6721", "white")) +
  labs(x = "Liczba równoległych userów", y = "Przepustowość [zap. na sekundę]", color = label_title, shape = label_title, fill = label_title)


