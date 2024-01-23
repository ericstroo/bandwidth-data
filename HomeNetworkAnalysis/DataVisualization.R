#Common visualizations for network analysis
library(grid)
library(gridExtra)


# This function creates a daily overview of Bandwidth, RTT, and packet loss data for a given ISP
visualization_daily_network <- function(log_data, plot_title) {
  
  
  time_range <- range(log_data$time_data)
  
  num_days <- length(seq(from=as.Date(min(time_range)), to=as.Date(max(time_range)), by="day"))
  
  #set "date_breaks" for x-axis based on length of time_Range using switch function
  #scale is date_breaks = 1 hour for 1 day, 6 hours for 2 days - 5 days, 12 hours for 5+ days
  if(num_days == 1){ date_breaks_scale = "1 hour"} 
  else if(num_days <= 3) {date_breaks_scale = "4 hours"}
  else if(num_days <= 5){date_breaks_scale = "6 hours"}
  else {date_breaks_scale = "12 hours"}
  
  scaling_factor <- 1/(num_days*.7)
  # Plot rtt_avg over time with jitter (rtt_mdev) error barsm abd secondary axis for packet loss bar plots
  # Includes and red diamonds for jitter or bandwidth drop events
  
  # Create two data sets for bar plots, one with packet loss values <= 100, and another with packet loss values == 999
  plot_rtt <- log_data %>%
    ggplot(aes(x = time_data)) +
    geom_errorbar(aes(y = rtt_avg, ymin = rtt_avg - rtt_mdev, ymax = rtt_avg + rtt_mdev), 
                  width = .5/num_days, linewidth = 0.5*scaling_factor, color = "orange", alpha = .4) +
    geom_point(aes(y = rtt_avg), data = log_data %>% filter(rtt_mdev <= 30),
               size = .5*scaling_factor, color="orange") +
    geom_point(aes(y = rtt_avg), data = log_data %>% filter(rtt_mdev > 30),
               shape = 5, color = "red", size = 3) +  # Red X for specific condition
    geom_col(width = 130*scaling_factor,
             aes(y = packet_loss, group = 1), 
             position = position_dodge("dodge2"), 
             color = NA, fill = "#ff9999") +  # Bar chart layer
    scale_y_continuous(
      "Ping (ms)", 
      limits = c(0, 500),
      sec.axis = sec_axis(~ . * .2, name = "Packet Loss (%)")  # Secondary axis (adjust the transformation as needed)
    ) +
    scale_x_datetime(limits = time_range, 
                     date_breaks = date_breaks_scale,
                     date_labels = "%H:%M %p") +
    coord_cartesian(ylim=c(0,40)) +
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
    geom_point(aes(y = upload, color = "Upload"), size = 2*scaling_factor) +
    geom_point(data = bandwidth_data %>% filter(upload < 10),
               aes(y = upload),  # Specify the y aesthetic
               shape = 5, color = "red", size = 3) +  # Red dots for uploads < 10)
    geom_point(aes(y = download, color = "Download"), size = 2*scaling_factor) +
    geom_point(data = bandwidth_data %>% filter(download < 10),
               aes(y = download),  # Specify the y aesthetic
               shape = 5, color = "red", size = 3) +  # Red dots for downloads < 10
    scale_color_manual(values = c("Upload" = "purple", "Download" = "cyan")) +
    scale_x_datetime(limits = time_range, 
                     date_breaks = date_breaks_scale,
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

save_visualization_daily_network <- function(log_data, plot_title, file_path) {

#call daily visualization visualization function
p <- daily_visualization <- visualization_daily_network(log_data, plot_title)

png(paste("./Visualizations/", dailyISP, "_", start, "_", end, ".png", sep=""), bg="white", width=3000, height=900, res=140)
plot(p)
dev.off()
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