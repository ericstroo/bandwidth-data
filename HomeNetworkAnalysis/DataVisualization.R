#Common vizualizations for network analysis

visualization_daily_network <- function(log_data) {
  time_range <- range(log_data$time_data)
  # Plotting rtt_avg over time with red error bars, dots for rtt_avg, and red X for specific condition
  
ping_data = log_data %>%
  filter(!is.na(packets_transmitted))

  plot_rtt <- ping_data %>%
    ggplot(aes(x = time_data)) +
    geom_errorbar(aes(y = rtt_avg, ymin = rtt_avg - rtt_mdev, ymax = rtt_avg + rtt_mdev), 
                  width = .5, linewidth = 0.5, color = "orange", alpha = .2) +
    geom_point(aes(y = rtt_avg), data = ping_data %>% filter(rtt_mdev <= 30),
               size = .5, color="orange") +
    geom_point(aes(y = rtt_avg), data = ping_data %>% filter(rtt_mdev > 30),
               shape = 5, color = "red", size = 3) +  # Red X for specific condition
    geom_col(aes(y = packet_loss, group = 1), position = position_dodge(width = 0.1), color = NA, fill = "red", alpha = 0.5) +  # Bar chart layer
    scale_y_continuous(
      "Ping (ms)", 
      limits = c(0, 100),
      sec.axis = sec_axis(~ . * 1, name = "Packet Loss (%)")  # Secondary axis (adjust the transformation as needed)
    ) +
    scale_x_datetime(limits = time_range, 
                     date_breaks = "1 hour",
                     date_labels = "%H:%M %p") +
    labs(title = "Ping Over Time with Bar Chart Overlay",
         x = "Time",
         y = "Ping (ms)") +
    theme_minimal()
    #theme(axis.text.y.right = element_text(color = "red", alpha = 0.1))  # Style for the secondary axis
  
  

bandwidth_data <- log_data %>% filter(!is.na(upload)) %>% filter(!is.na(download))
  
plot_bandwidth <- bandwidth_data %>%
    ggplot(aes(x = time_data)) +
    geom_point(aes(y = upload, color = "Upload"), size = 2) +
    geom_point(aes(y = download, color = "Download"), size = 2) +
    geom_point(data = bandwidth_data %>% filter(download < 10),
               aes(y = download),  # Specify the y aesthetic
               shape = 5, color = "red", size = 3) +  # Red dots for downloads < 10
    scale_color_manual(values = c("Upload" = "purple", "Download" = "cyan")) +
    scale_x_datetime(limits = time_range, 
                     date_breaks = "1 hour",
                     date_labels = "%H:%M %p") +
    labs(title = "Upload and Download Over Time",
         x = "Time",
         y = "Bandwidth (Mbit/s)",
         color = "Type") +
    ylim(0, 1000) +
    theme_minimal() +
    theme(legend.position=c(1,1),
          legend.direction="horizontal",
          legend.justification=c(1, 0), 
          legend.key.width=unit(1, "lines"), 
          legend.key.height=unit(1, "lines"))

  # Combine the plots
  grid.arrange(plot_bandwidth, plot_rtt, ncol = 1)
}

#boxplot ISP comparison function
visualization_boxplot <- function(log_data, factor, variable, tit="", xlabel="") {
  #create a boxplot comparing rtt_avg for TMOBILE data and COMCAST data

  log_data %>%
    ggplot(aes(x = factor, y = variable)) +
    geom_boxplot() +
    labs(title = tit) +
    xlab(xlabel) +
    theme_minimal()
}