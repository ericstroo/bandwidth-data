#Common visualizations for network analysis
library(grid)
library(gridExtra)


# This function creates a daily overview of Bandwidth, RTT, and packet loss data for a given ISP
visualization_daily_network <- function(log_data, plot_title) {
  
  
  time_range <- range(log_data$time_data)

  # Plot rtt_avg over time with jitter (rtt_mdev) error barsm abd secondary axis for packet loss bar plots
  # Includes and red diamonds for jitter or bandwidth drop events
  
  # Filter out any rows where packets weren't transmitted which signals a data collection error
  ping_data = log_data %>%
    filter(!is.na(packets_transmitted))

  plot_rtt <- ping_data %>%
    ggplot(aes(x = time_data)) +
    geom_errorbar(aes(y = rtt_avg, ymin = rtt_avg - rtt_mdev, ymax = rtt_avg + rtt_mdev), 
                  width = .5, linewidth = 0.5, color = "orange", alpha = .4) +
    geom_point(aes(y = rtt_avg), data = ping_data %>% filter(rtt_mdev <= 30),
               size = .5, color="orange") +
    geom_point(aes(y = rtt_avg), data = ping_data %>% filter(rtt_mdev > 30),
               shape = 5, color = "red", size = 3) +  # Red X for specific condition
    geom_col(width = 30,
             aes(y = packet_loss, group = 1), 
             position = position_dodge("dodge2"), 
             color = NA, fill = "red", alpha = 0.5) +  # Bar chart layer
    scale_y_continuous(
      "Ping (ms)", 
      limits = c(0, 100),
      sec.axis = sec_axis(~ . * .2, name = "Packet Loss (%)")  # Secondary axis (adjust the transformation as needed)
    ) +
    scale_x_datetime(limits = time_range, 
                     date_breaks = "1 hour",
                     date_labels = "%H:%M %p") +
    labs(title = "RTT and Packet Loss Over Time",
         x = "Time",
         y = "Ping (ms)") +
    theme_minimal() +
    theme(plot.margin=unit(c(20,10,20,20),"pt"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 10))
  
  # Plot bandwidth over time. Remove NA upload/download results to avoid printing errors
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
          legend.key.height=unit(1, "lines"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          plot.margin=unit(c(20,35,20,15),"pt"))
  
  # Combine the plots and add title
  grid.arrange(plot_bandwidth,
               plot_rtt,
               ncol = 1,
               top = textGrob(plot_title,
                              x=unit(20, "pt"),
                              y=unit(5, "pt"),
                              just="left",
                              hjust=0,
                              vjust=1))
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