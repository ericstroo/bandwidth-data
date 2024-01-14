#Work file for exploring data and testing functions

#import libraries
library(tidyverse)
library(gridExtra)
library(readr)

#update working directory and import functions
setwd("./HomeNetworkAnalysis/")
source("DataParsing.R")
source("DataVisualization.R")
data_directory <- "./Data/"

#test process_logs function
log_data <- process_logs(data_directory)
str(log_data)

#visualize a daily overview
dailyISP = "COMCAST"
start = "2024-1-11"
end = "2024-01-15"

log_data %>%
  filter(between(time_data, as.Date(start), as.Date(end))) %>% 
  filter(isp == dailyISP) %>%
  visualization_daily_network()

test = log_data %>% filter(isp=="TMOBILE") %>% filter(!is.na(upload))
max(test$rtt_avg)
max(test$rtt_mdev)


#create boxplot comparing rtt_avg, download, upload, jitter, and packet_loss for all ISP levels
visualization_boxplot(log_data, log_data$isp, log_data$rtt_avg, "Ping (ms) by ISP")
visualization_boxplot(log_data, log_data$isp, log_data$download, "Download (Mbit/s) by ISP")
visualization_boxplot(log_data, log_data$isp, log_data$upload, "Upload (Mbit/s) by ISP")
visualization_boxplot(log_data, log_data$isp, log_data$rtt_mdev, "Jitter (ms) by ISP")
visualization_boxplot(log_data, log_data$isp, log_data$packet_loss, "Packet Loss (%) by ISP")


#perform t-tests for all ISPs to compare rtt_avg, download, upload, jitter, and packet_loss
t.test(log_data %>% filter(isp == "TMOBILE") %>% select(rtt_avg),
       log_data %>% filter(isp == "COMCAST") %>% select(rtt_avg))
t.test(log_data %>% filter(isp == "TMOBILE") %>% select(download),
       log_data %>% filter(isp == "COMCAST") %>% select(download))
t.test(log_data %>% filter(isp == "TMOBILE") %>% select(upload),
       log_data %>% filter(isp == "COMCAST") %>% select(upload))
t.test(log_data %>% filter(isp == "TMOBILE") %>% select(rtt_mdev),
       log_data %>% filter(isp == "COMCAST") %>% select(rtt_mdev))
t.test(log_data %>% filter(isp == "TMOBILE") %>% select(packet_loss),
       log_data %>% filter(isp == "COMCAST") %>% select(packet_loss))