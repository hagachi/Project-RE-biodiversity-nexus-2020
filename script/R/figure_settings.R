sppcolors <- c("#35a16b", "#faf500",  "#ff2800", "#ff99a0", "#ff9900",   "#663300", "#9a0079", "darkseagreen2", "gray")

# theme_Publication <- function(base_size=14, base_family="helvetica") {
theme_Publication <- function(base_size=10) {
  library(grid)
  library(ggthemes)
  # (theme_foundation(base_size=base_size, base_family=base_family)
  (theme_bw(base_size=base_size, base_family = 'serif')
  # (theme_bw(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(# face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = 'black'),
            # axis.title = element_text(face = "bold",size = rel(1)),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            # axis.ticks.length = unit(-2, "mm"),
            # axis.text.x = element_text(margin = unit(rep(3,4), "mm")),
            # axis.text.y = element_text(margin = unit(rep(3,4), "mm")),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            # legend.direction = "horisontal",
            legend.key.size= unit(0.5, "cm"),
            legend.margin = unit(0.5, "cm"),
            # legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            # strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.background=element_rect(colour=NA,fill=NA),
            strip.text = element_text()
            # strip.text = element_text(face="bold")
    ))
}
