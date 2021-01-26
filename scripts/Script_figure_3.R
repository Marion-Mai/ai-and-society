# Figure 3
# AI and Society
# R Script by Marion Maisonobe. Script last update: 2020-01-26
# To generate nice charts

library(readr)
library(tidyverse)

####

read_plus <- function(flnm) {
  read_delim(flnm, delim = "\t") %>%
    drop_na() %>%
    mutate(year = `Publication Years`) %>%
    select(year, records) %>%
    mutate(var = flnm) %>%
    mutate(var = str_extract(var, pattern = "\\w+(?=_PY)"))
}

cat <- c("traffic", "water_quality")

tbl <-
  list.files(path = paste0("./data/", cat, "/"),
             pattern = "*\\w_PY",
             full.names = T) %>%
  map_df(~read_plus(.))


colors <- c("#CC9900", "#3465A4")

plot_fig3 <- ggplot(tbl, aes(x = as.Date(year, format = "%Y"), y = records, group = var)) +
  geom_line(aes(color = var)) + # , lwd = 0.8) +
  geom_point(aes(color = var)) +
  theme_light() +
  xlab("Publication years up until 2018") + ylab("Number of publications") +
  scale_x_date(limits = as.Date(c("1961-01-01","2019-01-01"), format = "%Y"), expand = c(0, 80),
               breaks = seq(as.Date("1961-01-01"), as.Date("2019-01-01"), by = "4 years"), # breaks = date_breaks("2 years"), ##
               labels = date_format("%Y")) +
  ggtitle("Annual number of publications about traffic flow and water quality") + 
  scale_color_manual(values = colors, labels = c("Traffic flow studies", "Water quality studies")) + # , name = "Variables",
  theme(plot.title = element_text(size = 11), #face = "bold",
        plot.subtitle = element_text(face = "italic"),
        axis.text.x = element_text(angle = 40),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "cadetblue4", color = "cadetblue4"),
        # strip.text = element_text(colour = 'black'),
        legend.position = "bottom", # c(0.25, 0.75)
        # legend.box = "vertical",
        legend.background = element_rect(fill = NA), #colour = "grey80"
        legend.title = element_blank()
  ) ##

svg(paste("outputs/figures/Figure_3.svg"), width = 8, height = 4)
plot.new()
ggdraw(add_sub(plot_fig3, fontface = "italic", size = 8, color = "black", x = 0, y = 0.5, hjust = 0, vjust = 0.5, fontfamily = "sans", lineheight = 1,
               label =
                 "Indexes:  SCI-EXPANDED, SSCI, A&HCI, CPCI-S, CPCI-SSH, ESCI. Web of Science Core Collection. MM, 2021-01-11" #\n
))
dev.off()

 write_tsv(tbl, "outputs/tables/data_figure_3.tsv")
