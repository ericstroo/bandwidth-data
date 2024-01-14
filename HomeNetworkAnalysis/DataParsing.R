#Functions for parsing txt files, assembling into dataframe, and data cleaning

# Load necessary library
library(tidyverse)
library(gridExtra)
library(readr)

#crawl log file with grep() and assemble into dataframe
#NOTE works with data created in initial log file format in early experiments.
# This Will need to be modified to work with data produced by networkTest.sh script
parse_log_file <- function(file_path) {
  # Function to parse the log file
  getwd()
  lines <- readLines(file_path)
  
  # Extract header information
  ref <- str_extract(lines[1], "(?<=REF: ).*")
  isp <- str_extract(lines[2], "(?<=ISP: ).*")
  lan_hardware <- str_extract(lines[3], "(?<=LAN_HARDWARE: ).*")
  modem <- str_extract(lines[4], "(?<=MODEM: ).*")
  mtu <- as.numeric(str_extract(lines[5], "(?<=MTU: ).*"))
  temp_low <- as.numeric(str_extract(lines[6], "(?<=TEMP_LOW: ).*"))
  temp_high <- as.numeric(str_extract(lines[7], "(?<=TEMP_HIGH: ).*"))
  
  # Initialize empty vectors to store the data
  time_data <- numeric()
  packets_transmitted <- numeric()
  packets_received <- numeric()
  packet_loss <- numeric()
  rtt_min <- numeric()
  rtt_avg <- numeric()
  rtt_max <- numeric()
  rtt_mdev <- numeric()
  download <- numeric()
  upload <- numeric()
  parse_complete = TRUE
  
  # Iterate over the lines and extract data
  for (i in seq_along(lines)) {
    if (grepl("TIME:", lines[i])) {
      if (parse_complete == TRUE){
        parse_complete = FALSE
      }
      else {
        # in some instances, received a 403 FORBIDDEN error with speedtest.net
        # this logic makes sure to add NA to upload and download before starting the next line of data
        download <- c(download, NA) 
        upload <- c(upload, NA)
      }
      
      time_data <- c(time_data, as.numeric(str_extract(lines[i], "\\d+")))
      
    }
    if (grepl("packets transmitted", lines[i])) {
      transmitted_received <- str_extract_all(lines[i], "\\d+")[[1]]
      packets_transmitted <- c(packets_transmitted, as.numeric(transmitted_received[1]))
      packets_received <- c(packets_received, as.numeric(transmitted_received[2]))
      packet_loss <- c(packet_loss, as.numeric(str_extract(lines[i], "\\d+\\.\\d+")))
    }
    if (grepl("rtt min/avg/max/mdev", lines[i])) {
      rtt_values <- str_extract_all(lines[i], "\\d+\\.\\d+")[[1]]
      rtt_min <- c(rtt_min, as.numeric(rtt_values[1]))
      rtt_avg <- c(rtt_avg, as.numeric(rtt_values[2]))
      rtt_max <- c(rtt_max, as.numeric(rtt_values[3]))
      rtt_mdev <- c(rtt_mdev, as.numeric(rtt_values[4]))
    }
    if (grepl("Download:", lines[i])) {
      download <- c(download, as.numeric(str_extract(lines[i], "\\d+\\.\\d+")))
    }
    if (grepl("Upload:", lines[i])) {
      upload <- c(upload, as.numeric(str_extract(lines[i], "\\d+\\.\\d+")))
      parse_complete = TRUE
    }
  }
  

  # Create a dataframe
  data <- data.frame(
    ref = ref %>% as.character() %>% rep(length(time_data)),
    isp = isp %>% as.factor() %>% rep(length(time_data)),
    lan_hardware = lan_hardware %>% as.factor() %>% rep(length(time_data)),
    modem = modem %>% as.factor() %>% rep(length(time_data)),
    mtu = mtu %>% as.integer() %>% rep(length(time_data)),
    temp_low = temp_low %>% as.integer() %>% rep(length(time_data)),
    temp_high = temp_high %>% as.integer() %>% rep(length(time_data)),
    time_data = as.POSIXct(time_data, origin="1970-01-01", tz="America/Los_Angeles"),
    packets_transmitted = packets_transmitted %>% as.integer(),
    packets_received = packets_received %>% as.integer(),
    packet_loss = packet_loss %>% replace(is.na(.), 0),
    rtt_min,
    rtt_avg,
    rtt_max,
    rtt_mdev,
    download,
    upload
  )

  return(data)
  
}

#take log files in array "logset" and merge them into a dataframe
process_logs <- function(directory){
  
  #initialize empty dataframe
  assembledDataframe <- data.frame()
  
  #generate list of files with directory folder name appended to the beginning
  list <- paste(directory,list.files(directory),sep="")
  
  #for loop through each file in directory
  for (i in list){
    print(i)
    currentDataFrame = parse_log_file(i)
    assembledDataframe = assembledDataframe %>% rbind(currentDataFrame)
  }
  
  return(assembledDataframe)
}

