load("isoscape.rda") # created in the workflow

plot.isoscape <- plot(x = isoscape, palette = list(n.labels = 15, range = c(-130, 10), digits = 1))

CairoPNG("isoscape.png", units="in", 
         width=10, 
         height=8, 
         pointsize=24*92/72, 
         dpi=96)
         
plot.isoscape 

dev.off()
