file_path = "./Data/2024-01-14_COMCAST_Experiment_v2.txt"

lines <- readLines(file_path)

# Extract header information
headers <- c("REF", "ISP", "LAN_HARDWARE", "MODEM", "MTU", "TEMP_LOW", "TEMP_HIGH")
header_values <- sapply(headers,
                        function(header) {
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

expected_step = ""
ping_steps = c("TIME", "PACKETS", "RTT", "BANDWIDTH_CHECK")
data_count = 0
exceptions = c("bytes of data",
               "ping statistics ---",
               "Ping:",
               "Download:",
               "Upload:")

# Iterate over the lines and extract data
for (i in seq_along(lines)) {
  #Scan for new tests
  if(grepl("TEST: PING", lines[i])) {
    if(expected_step == "BANDWIDTH_CHECK"){
      download <- c(download, NA)
      upload <- c(upload, NA)
    }

    expected_step <- ping_steps[1]
    next
  }
  else if(grepl("TEST: BANDWIDTH", lines[i])){
    expected_step <- ""
    
    if(grepl("Download:", lines[i+2])){
      download <- c(download, as.numeric(str_extract(lines[i+2], "\\d+")))
      upload <- c(upload, as.numeric(str_extract(lines[i+3], "\\d+")))
    }
    else {
      download <- c(download, 100000)
      upload <- c(upload, 100000)
    }
    next
  }
  
  #Execute PING test collection
  switch(expected_step,
         "TIME" = {
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
           if (lines[i] == "" || grepl(paste(exceptions, collapse = "|"), lines[i])) {
             next
           }
           
           #if this is the last loop and its ending on RTT, add NA to bandwidth
           if(i == length(lines)){
             print(i)
             download <- c(download, NA)
             upload <- c(upload, NA)
           }
           
           if(grepl("packets transmitted", lines[i])) {
             transmitted_received <- str_extract_all(lines[i], "\\d+")[[1]]
             packets_transmitted <- c(packets_transmitted, as.numeric(transmitted_received[1]))
             packets_received <- c(packets_received, as.numeric(transmitted_received[2]))
             
             #if string found for packet loss is 0%, store zero, otherwise store the numeric value
             if (grepl("100%", lines[i])) {
               packet_loss <- c(packet_loss, 100)
             } else {
               packet_loss <- c(packet_loss, as.numeric(str_extract(lines[i], "\\d+\\.\\d+")))
             }
           }
           else {
             # Expected PACKETS but not found, insert NA
             packets_transmitted <- c(packets_transmitted, NA)
             packets_received <- c(packets_received, NA)
             packet_loss <- c(packet_loss, NA)
           }
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
#Add NA to upload and download, or RTT variables if they shorter than the other vectors
#This only occurs when 1. the test is interrupted or ends without a final speedtest. 2. the last test had 100% packet loss failure.
print(packet_loss)
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

test = data.frame(packet_loss)

# Create a dataframe
data <- data.frame(
  ref = header_values["REF"] %>% as.character() %>% rep(length(time_data)),
  isp = header_values["ISP"] %>% as.factor() %>% rep(length(time_data)),
  lan_hardware = header_values["LAN_HARDWARE"] %>% as.factor() %>% rep(length(time_data)),
  modem = header_values["MODEM"] %>% as.factor() %>% rep(length(time_data)),
  mtu = as.integer(header_values["MTU"]) %>% rep(length(time_data)),
  temp_low = as.integer(header_values["TEMP_LOW"]) %>% rep(length(time_data)),
  temp_high = as.integer(header_values["TEMP_HIGH"]) %>% rep(length(time_data)),
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
