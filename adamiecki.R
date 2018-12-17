library(reshape2)
library(ggplot2)
library(dplyr)
library(gridExtra)

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")

df_melted <- list(
  data.frame(task = 'Data acquisition and curation', 
             start = '2019-09-01', 
             end = '2020-12-01', 
             secondment = FALSE),
  data.frame(task = 'Development of data-processing framework', 
             start = '2019-09-01', 
             end = '2019-12-01', 
             secondment = FALSE),
  data.frame(task = 'Devise a unified format to represent the HDX-MS data', 
             start = '2019-12-01', 
             end = '2020-03-01', 
             secondment = FALSE),
  data.frame(task = 'Develop internal tools for data transformation', 
             start = '2020-03-01', 
             end = '2020-08-01', 
             secondment = FALSE),
  data.frame(task = "Acquisition of external data", 
             start = '2020-08-01', 
             end = '2020-12-01', 
             secondment = FALSE),
  data.frame(task = 'Implementation of the database', 
             start = '2019-12-01', 
             end = '2020-12-01', 
             secondment = FALSE),
  data.frame(task = 'Implementation of searching tools', 
             start = '2020-12-01', 
             end = '2021-02-01', 
             secondment = FALSE),
  data.frame(task = 'Development of new visualisations and analytic tools', 
             start = '2021-02-01', 
             end = '2021-06-01', 
             secondment = FALSE),
  data.frame(task = 'Wrapping-up the project', 
             start = '2021-06-01', 
             end = '2021-09-01', 
             secondment = FALSE)
) %>% 
  do.call(rbind, .) %>% 
  mutate(start = as.Date(start),
         end = as.Date(end),
         task = factor(task)) %>% 
  melt %>% 
  mutate(secondment_nice = ifelse(secondment, "University of Michigan", "DTU"),
         task = factor(task, labels = gsub('(.{1,30})(\\s|$)', '\\1\n', levels(task))))



start_date <- as.Date('2019-07-01')

p <- ggplot(df_melted, aes(x = value, y = task, color = secondment_nice)) + 
  geom_line(size = 5) +
  theme_bw(base_size = 6) +
  scale_color_manual("", values = c("black", "grey")) +
  scale_x_date("", date_labels = "%m.%Y", limits = c(start_date, NA), date_breaks = '3 months') +
  scale_y_discrete("") + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_line(colour="black", linetype = "dashed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))
        #legend.position = "bottom",
        #legend.key = element_rect()) 

g_legend<-function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}

cairo_pdf("workplan.pdf", height = 3.6, width = 5)
grid.arrange(p + guides(color = FALSE),
              ncol = 1, heights = c(0.95, 0.05)) 
dev.off()
