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
# WERTYKALNE
label_title <- "Nazwa instancji"

df1 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/monolit/m4.large-edmonds-karp/")
df2 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/monolit/m4.xlarge-edmonds-karp/")
df21 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/monolit/m4.2xlarge-edmonds-karp/")

df_combined_1 <- rbind(
  transform(df1, Technologia = "Java", Architektura = "Monolit", Instancja = "m4.large"),
  transform(df2, Technologia = "Java", Architektura = "Monolit", Instancja = "m4.xlarge"),
  transform(df21, Technologia = "Java", Architektura = "Monolit", Instancja = "m4.2xlarge")
)

df3 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.large-edmonds-karp/")
df9 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.xlarge-edmonds-karp/")
df19 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/Java/mikroserwisy/m4.2xlarge-edmonds-karp/")

df_combined_2 <- rbind(
  transform(df3, Technologia = "Java", Architektura = "Mikroserwisy", Instancja = "m4.large"),
  transform(df9, Technologia = "Java", Architektura = "Mikroserwisy", Instancja = "m4.xlarge"),
  transform(df19, Technologia = "Java", Architektura = "Mikroserwisy", Instancja = "m4.2xlarge")
)

df5 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/monolit/m4.large-edmonds-karp/")
df6 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/monolit/m4.xlarge-edmonds-karp/")
df7 <- collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/monolit/m4.2xlarge-edmonds-karp/")

df_combined_3 <- rbind(
  transform(df5, Technologia = "NET", Architektura = "Monolit", Instancja = "m4.large"),
  transform(df6, Technologia = "NET", Architektura = "Monolit", Instancja = "m4.xlarge"),
  transform(df7, Technologia = "NET", Architektura = "Monolit", Instancja = "m4.2xlarge")
)

df10 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.large-edmonds-karp/")
df20 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.xlarge-edmonds-karp/")
df21 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NET/mikroserwisy/m4.2xlarge-edmonds-karp/")

df_combined_4 <- rbind(
  transform(df10, Technologia = "NET", Architektura = "Mikroserwisy", Instancja = "m4.large"),
  transform(df20, Technologia = "NET", Architektura = "Mikroserwisy", Instancja = "m4.xlarge"),
  transform(df21, Technologia = "NET", Architektura = "Mikroserwisy", Instancja = "m4.2xlarge")
)

df11 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/monolit/m4.large-edmonds-karp/")
df12 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/monolit/m4.xlarge-edmonds-karp/")
df13 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/monolit/m4.2xlarge-edmonds-karp/")

df_combined_5 <- rbind(
  transform(df11, Technologia = "NodeJS", Architektura = "Monolit", Instancja = "m4.large"),
  transform(df12, Technologia = "NodeJS", Architektura = "Monolit", Instancja = "m4.xlarge"),
  transform(df13, Technologia = "NodeJS", Architektura = "Monolit", Instancja = "m4.2xlarge")
)

df16 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.large-edmonds-karp/")
df17 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.xlarge-edmonds-karp/")
df18 <-  collectData("D:/Studia- informatyka/II-stopień/semestr 1/Projekt badawczy/Gotowe_dane/NodeJS/mikroserwisy/m4.2xlarge-edmonds-karp/")

df_combined_6 <- rbind(
  transform(df16, Technologia = "NodeJS", Architektura = "Mikroserwisy", Instancja = "m4.large"),
  transform(df17, Technologia = "NodeJS", Architektura = "Mikroserwisy", Instancja = "m4.xlarge"),
  transform(df18, Technologia = "NodeJS", Architektura = "Mikroserwisy", Instancja = "m4.2xlarge")
)

df_combined <- rbind(df_combined_1, df_combined_2, df_combined_3, df_combined_4, df_combined_5, df_combined_6)


#######################################
#                 PLOT                #
#######################################
max_y <- df_combined %>%
  group_by(Technologia, Instancja, Architektura) %>%
  summarise(max_y = max(Throughput))


geom_text_data <- df_combined %>% group_by(Architektura, Technologia, Instancja) %>% filter(Throughput == max(Throughput)) %>% slice(1)

ggplot(df_combined, aes(x = Users, y = Throughput, color = Instancja)) +
  # ggplot(df_combined, aes(x = Users, y = Throughput, color = Instancja)) +
  geom_point() + 
  geom_line() +
  geom_hline(data = max_y, linetype = "dashed", color = "gray", aes(yintercept = max_y)) +
  geom_text(data = geom_text_data,
            aes(x = Users, y = Throughput, label = round(Throughput, 1.2), vjust = -0.8), show.legend = FALSE, check_overlap = TRUE) +
  labs(x = "Liczba równoległych userów", y = "Przepustowość [zap. na sekundę]", color = label_title) +
  facet_grid(factor(Technologia, c("Java", "NET", "NodeJS")) ~ Architektura) +
  # facet_wrap(~Architektura) +
  scale_color_discrete(limits=c("m4.large", "m4.xlarge", "m4.2xlarge")) #+
scale_y_continuous(limits = c(0, 6000)) #+
scale_y_log10(limits = c(4, 250)) 
# scale_y_continuous(trans = "log", limits = c(2, 260)) +
# scale_x_continuous(limits = c(1, 10)) +
# theme(legend.position = "bottom")
# coord_cartesian(ylim = c(220, 261))


