
# Figure 1
# AI and Society
# R Script by Marion Maisonobe. Script last update: 2020-01-26
# To generate nice charts

library(eulerr)

fit <- euler(c("A" = 4, "B" = 6, "C" = 3, "D" = 2, "E" = 7, "F" = 3,
               "A&B" = 2, "A&F" = 2, "B&C" = 2, "B&D" = 1,
               "B&F" = 2, "C&D" = 1, "D&E" = 1, "E&F" = 1,
               "A&B&F" = 1, "B&C&D" = 1),
             shape = "ellipse")

plot(fit)

# eulerr.co()


intersections <- euler(c("A" = 426386, "B" = 313866, "C" = 83741,
                                 "A&B" = 59541, "A&C" = 10871, "B&C" = 5494,
                                 "A&B&C" = 2137))
# euler()
eulerr_options()
# opar <- par()

plot.new()
svg(filename = "outputs/figures/Figure_1.svg",
    width = 10,
    height = 9)
par(oma=c(4,4,0,4))
par(mar=c(10,12,4.1,12) + 0.3)
plot.new()
# par(mar = c(0,0,0,0))
plot(intersections,
     labels = list(labels = c("Machine Learning\n426,386 records", "Artificial Intelligence\n313,866 records", "Big Data\n83,741 records"),
                   col = c("darkgreen", "darkblue", "darkorange"),
                   cex = 1.5, font = 2),
     edges = NA,
     fills = list(fill = c("lightgreen", "steelblue4", "orange"), alpha = 0.5),
     main = "")
    # main = list(label = "", cex.main = 0.2, font.main = 2))

     title(main = ("Overlap between Big Data,\nArtificial Intelligence and Machine Learning publications"), line = 1,  cex.main = 1.5, font.main = 2) #line = 2.5
     mtext("Based on lexical queries made in the Web of Science Core Collection database", side = 1, line = 2, cex = 1.5, outer = T) # line = 4.7
     mtext("MM, 2021-10-01. Designed with the R package Eulerr", side = 4, cex = 1, line = 3, outer = T, adj = 1, font = 3, col = "grey")
dev.off()
