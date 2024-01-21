#Functions for parsing txt files, assembling into dataframe, and data cleaning

# Load necessary libraries
library(tidyverse)
library(gridExtra)
library(readr)
library(stringr)



parse_log_file2 <- function(file_path) {
  lines <- readLines(file_path)
  
  # Extract header information
  headers <- c("REF", "ISP", "LAN_HARDWARE", "MODEM", "MTU")
  header_values <- sapply(headers, function(header) {
    line <- grep(paste0(header, ":"), lines, value = TRUE)
    str_extract(line, "(?<=: ).*")
  })
  names(header_values) <- headers
  
  # Initialize empty vectors to store the data
  time_data <- integer()
  packets_transmitted <- numeric()
  packets_received <- numeric()
  packet_loss <- numeric()
  rtt_min <- numeric()
  rtt_avg <- numeric()
  rtt_max <- numeric()
  rtt_mdev <- numeric()
  download <- numeric()
  upload <- numeric()
  
  # Initialize control variables
  expected_step = ""
  ping_steps = c("TIME", "PACKETS", "RTT", "BANDWIDTH_CHECK")
  exceptions = c("bytes of data",
                 "ping statistics ---",
                 "Ping:",
                 "Download:",
                 "Upload:")
  ping_logged = FALSE
  #print(paste("Initial state of ping_logged:", ping_logged, sep=" "))
  # Iterate over the lines and extract data
  for (i in seq_along(lines)) {
    
    #Scan for new tests
    if(grepl("TEST: PING", lines[i])) {
      #print("-----new ping test found-----")
      # If expecting a bandwidth test, but encounter a PING test, log NA for bandwidth test
      # Once completing loging a ping test, log NA for a bandwidth test prior to logging next Ping test
      if(expected_step == "BANDWIDTH_CHECK"){
        #print("LOG NA BANDWIDTH")
        download <- c(download, NA)
        upload <- c(upload, NA)
      }
      
      #Adjust expected step to TIME
      expected_step <- ping_steps[1]
      next
    }
    
    #If a Speedtest is encountered, log Speedtest results
    else if(grepl("TEST: BANDWIDTH", lines[i])){
      #print("new bandwidth test found")
      expected_step <- ""
      
      # If the data exists, log it. Otherwise log NA.
      # There are cases where speedtest will write "TEST: BANDWIDTH" and then fail.
      # These events must be logged as NA to maintain 1:1 alignment with other data vectors
      if(grepl("Download:", lines[i+2])){
        #print("SUCCESSFUL BANDWIDTH TEST")
        download <- c(download, as.numeric(str_extract(lines[i+2], "\\d+")))
        upload <- c(upload, as.numeric(str_extract(lines[i+3], "\\d+")))
      }
      else {
        #print("LOG NA BANDWIDTH")
        download <- c(download, NA)
        upload <- c(upload, NA)
      }
      next
    }
    #Execute Ping Test data parsing when expected step is within Ping test bounds
    switch(expected_step,
           "TIME" = {
             #print("time data found")
             if(grepl("TIME:", lines[i])) {
               time_data <- c(time_data, as.numeric(str_extract(lines[i], "\\d+")))
             }
             else {
               # Expected TIME but not found, insert NA
               time_data <- c(time_data, NA)
             }
             expected_step <- ping_steps[2]
             next
           },
           "PACKETS" = {
             #print("--starting packet data scan")
             #print(paste("ping_logged value:", ping_logged, sep=" "))
             #If the first line is blank and ping logged hasn't been set to TRUE, it means the test failed. Log packet related data as NA, as well as RTT data, then reset and look for next test.
              if (lines[i]=="" & ping_logged == FALSE) {
                #print("packet data not found")
                packets_transmitted <- c(packets_transmitted, NA)
                packets_received <- c(packets_received, NA)
                packet_loss <- c(packet_loss, NA)
                # rtt_min <- c(rtt_min, NA)
                # rtt_avg <- c(rtt_avg, NA)
                # rtt_max <- c(rtt_max, NA)
                # rtt_mdev <- c(rtt_mdev, NA)
                expected_step <- ping_steps[3]
                next
              }
              else if(ping_logged == FALSE) {
                #print("line is not blank")
                ping_logged = TRUE
              }

              #If next line is one of the exceptions, skip it, and move on to the next line. If the following line is blank, skip and move onto the next line.
              if (lines[i] == "" || grepl(paste(exceptions, collapse = "|"), lines[i])) {
                #print("non data line of text")
                next
              }
              #If the line contains "packets transmitted", log the data.
              if(grepl("packets transmitted", lines[i])) {
                #print("packet data found")
                ping_logged = FALSE
                transmitted_received <- str_extract_all(lines[i], "\\d+")[[1]]
                packets_transmitted <- c(packets_transmitted, as.numeric(transmitted_received[1]))
                packets_received <- c(packets_received, as.numeric(transmitted_received[2]))
              }
              #After NA or the data is logged, set expected test to RTT and move on to the next line.
              expected_step <- ping_steps[3]
              next
             
           },
           "RTT" = {
             if(grepl("rtt min/avg/max/mdev", lines[i])) {
               rtt_values <- str_extract_all(lines[i], "\\d+\\.\\d+")[[1]]
               rtt_min <- c(rtt_min, as.numeric(rtt_values[1]))
               rtt_avg <- c(rtt_avg, as.numeric(rtt_values[2]))
               rtt_max <- c(rtt_max, as.numeric(rtt_values[3]))
               rtt_mdev <- c(rtt_mdev, as.numeric(rtt_values[4]))
             }
             else {
               rtt_min <- c(rtt_min, NA)
               rtt_avg <- c(rtt_avg, NA)
               rtt_max <- c(rtt_max, NA)
               rtt_mdev <- c(rtt_mdev, NA)
             }
             
             expected_step <- ping_steps[4]
             next
           }
           
    )
  }
  
  
  # Add NA to upload and download if its shorter than the other vectors
  # This only occurs when the test is interrupted or ends without a final speedtest.
  # Add NA to RTT variables. This occurs if final test ended with 100% packet loss.
  
  data_length = max(c(length(time_data),
                      length(packets_transmitted),
                      length(packets_received),
                      length(packet_loss),
                      length(rtt_min),
                      length(rtt_avg),
                      length(rtt_max),
                      length(rtt_mdev)))
  
  if(length(download) < data_length){
    download <- c(download, NA)
    upload <- c(upload, NA)
  }
  
  if(length(rtt_avg) < data_length){
    rtt_avg <- c(rtt_avg, NA)
    rtt_max <- c(rtt_max, NA)
    rtt_min <- c(rtt_min, NA)
    rtt_mdev <- c(rtt_mdev, NA)
  }
  if(length(packets_transmitted) < data_length){
    packets_transmitted <- c(packets_transmitted, NA)
    packets_received <- c(packets_received, NA)
    packet_loss <- c(packet_loss, NA)
  }
  
  
  
   
  # Create a dataframe
  data <- data.frame(
    ref = header_values["REF"] %>% as.character() %>% rep(length(time_data)),
    isp = header_values["ISP"] %>% rep(length(time_data)),
    lan_hardware = header_values["LAN_HARDWARE"] %>%rep(length(time_data)),
    modem = header_values["MODEM"] %>% rep(length(time_data)),
    mtu = header_values["MTU"] %>% rep(length(time_data)),
    temp_low = NA,
    temp_high = NA,
    time_data = as.POSIXct(time_data, origin="1970-01-01", tz="America/Los_Angeles"),
    packets_transmitted = packets_transmitted %>% as.integer(),
    packets_received = packets_received %>% as.integer(),
    packet_loss = (1-(packets_received/packets_transmitted))*100,
    rtt_min,
    rtt_avg,
    rtt_max,
    rtt_mdev,
    download,
    upload
  )
  
  # Set ISP, LAN_HARDWARE, and MODEM to all caps + refactor.
  # Messy code, but unable to do this at the vector level in R.
  data$isp <- toupper(data$isp) %>% as.factor()
  data$lan_hardware <- toupper(data$lan_hardware) %>% as.factor()
  data$modem <- toupper(data$modem) %>% as.factor()
  #replace packet loss NA with 100
  data$packet_loss <- replace(data$packet_loss, is.na(data$packet_loss), 100)
  
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
    currentDataFrame = parse_log_file2(i)
    assembledDataframe = assembledDataframe %>% rbind(currentDataFrame)
  }
  
  return(assembledDataframe)
}


# crawl log file with grep() and assemble into dataframe
# ARCHIVE: works with data created in initial log file format in early experiments.
# Keep for comparison with each ISPs data on older networking equipment prior to upgrade

parse_log_file <- function(file_path) {
  # Function to parse the log file
  getwd()
  lines <- readLines(file_path)
  
  # Extract header information
  #NEXT REV: loop through these so they don't have to be in order
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
  parse_complete = TRUE #NEXT REV: Replace parse_complete with a vector of logicals to track if each test is complete
  
  
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