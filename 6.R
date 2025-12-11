library(ggplot2)
library(reshape2)
library(dplyr)

data("mpg")

ggplot(mpg,aes(x=displ,y=hwy,color=class))+
  geom_point(size=3,alpha=0.7)+
  geom_smooth(method="lm",se=TRUE,
              linetype="dashed",color="black",linewidth=1)+
  labs(
    title="EngineDisplacementvsHighwayMPG",
    x="EngineDisplacement(L)",
    y="HighwayMilesperGallon",
    color="VehicleClass"
  )+
  theme_bw()+
  theme(
    plot.title=element_text(hjust= 0.5,size=14,
                            face="bold"),
    axis.title=element_text(size=12),
    legend.position="bottom"
  )

ggplot(mpg,aes(x=displ,y=hwy)) +
  geom_point(color="darkgreen",size=2) + 
  facet_wrap(~class, ncol=3) + 
  labs(
    title = "faceted scatter plot by vehicle class",
    x="engine displacement (L)",
    y="highway miles per gallon"
  ) + 
  theme(
    strip.text = element_text(size=12,face="italic"),
    plot.title = element_text(hjust=0.5,size=14)
  )

ggplot(mpg,aes(x=displ,y=hwy, color=class)) + 
  geom_point(size = 3, shape = 21, fill = "lightblue",alpha=0.8) + 
  labs(
    title="customised scatter plot",
    x="engine displacement",
    y="highway miles per gallon",
    color="class"
  ) + 
  theme(
    plot.title = element_text(face="bold", size=14),
    axis.title.x=element_text(size=12),
    axis.title.y = element_text(size=12),
    legend.background = element_rect(fill="gray80")
  )

annotated_plot <- ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point(color="purple", size=3) + 
  annotate(
    "text", x=3, y=40,
    label="high effeciency zone",
    color="red",size=3,fontface="bold",angle=15
  ) + 
  annotate(
    "rect", xmin=2, xmax=4, ymin=30, ymax=45,
    alpha=0.2, fill="yellow",color="orange"
  ) + 
  labs(
    title="AnnotatedScatterPlot",
    x="EngineDisplacement(L)",
    y="highway miles per galoon",
  ) + 
  theme(
    plot.title = element_text(hjust=0.5)
  )
annotated_plot

ggsave(
  "annotated_scatter_plot.png", annotated_plot,
  width = 10, height = 8, dpi = 300
)