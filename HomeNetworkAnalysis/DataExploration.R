#Work file for exploring data and testing functions

#import libraries
library(tidyverse)
library(readODS)

#update working directory and import functions
setwd("./HomeNetworkAnalysis/")
source("DataParsing.R")
source("DataVisualization.R")
data_directory <- "./Data/"

#extracts log data from directory. 
log_data <- process_logs(data_directory)
#ceptions <- read_ods("./ProjectPlanning/events_and_exceptions.ods", sheet=1)

#for loop through exceptions table
#filter out exceptions between exception date ranges from log_data
# for (i in 1:nrow(exceptions)){
#   log_data <- log_data %>% filter(!between(time_data, as.POSIXct(exceptions[i,]$Start), as.POSIXct(exceptions[i,]$End)))
#   print(i)
# }

#visualize a daily overview
dailyISP = "CenturyLink"
start = "2024-1-28"
end = "2024-02-02"
starttime = "00:00:00"
endtime = "23:59:59"
file_path = paste(".Visualizations/", dailyISP, "_", start, ".png", sep="")

#save visualization_daily_network as a png
log_data %>%
  filter(between(time_data, as.POSIXct(paste(start, starttime)), as.POSIXct(paste(end, endtime)))) %>%
  filter(isp == toupper(dailyISP)) %>%
  visualization_daily_network(plot_title = paste(dailyISP, ": ", start, " to ", end, sep=""))

# 
#create boxplot comparing rtt_avg, download, upload, jitter, and packet_loss for all ISP levels
# visualization_boxplot(log_data, log_data$isp, log_data$rtt_avg, "Ping (ms) by ISP")
# visualization_boxplot(log_data, log_data$isp, log_data$download, "Download (Mbit/s) by ISP")
# visualization_boxplot(log_data, log_data$isp, log_data$upload, "Upload (Mbit/s) by ISP")
# visualization_boxplot(log_data, log_data$isp, log_data$rtt_mdev, "Jitter (ms) by ISP")
# visualization_boxplot(log_data, log_data$isp, log_data$packet_loss, "Packet Loss (%) by ISP")
# 
# #average jitter for COMCAST
# log_data %>% filter(isp == "COMCAST") %>% summarize(mean(rtt_mdev, na.rm = TRUE))
# 
# #perform t-tests for all ISPs to compare rtt_avg, download, upload, jitter, and packet_loss
# t.test(log_data %>% filter(isp == "TMOBILE") %>% select(rtt_avg),
#        log_data %>% filter(isp == "COMCAST") %>% select(rtt_avg))
# t.test(log_data %>% filter(isp == "TMOBILE") %>% select(download),
#        log_data %>% filter(isp == "COMCAST") %>% select(download))
# t.test(log_data %>% filter(isp == "TMOBILE") %>% select(upload),
#        log_data %>% filter(isp == "COMCAST") %>% select(upload))
# t.test(log_data %>% filter(isp == "TMOBILE") %>% select(rtt_mdev),
#        log_data %>% filter(isp == "COMCAST") %>% select(rtt_mdev))
# t.test(log_data %>% filter(isp == "TMOBILE") %>% select(packet_loss),
#        log_data %>% filter(isp == "COMCAST") %>% select(packet_loss))
