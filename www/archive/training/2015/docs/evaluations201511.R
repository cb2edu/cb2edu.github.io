library(RColorBrewer)

# Read Table From File

evaluations <- 
  read.table("http://cmb.path.uab.edu/training/docs/evaluation201511.tsv",
             header = TRUE, 
             sep = "\t")

# Remove empty column
evaluations <- evaluations[,-4]

# Calculate Mean and Standard Error

vector=0*c(1:9)
std=0*c(1:9)

for (i in 1:9) {
  vector[i]<-mean(evaluations[,i+1][!is.na(evaluations[,i+1])])
  std[i]<-sd(evaluations[,i+1][!is.na(evaluations[,i+1])])
}

png("evaluation201511.png", width = 11, height = 5, units = "in", res = 120)

# Make BarPlot

nice <- brewer.pal(3, "Pastel2")

par(mfrow = c(1, 2), mgp=c(2,0.75,0))

bp <- barplot(vector, 
              ylim=c(0,10), 
              names=c(1:9),
              xlab="Question",
              ylab="Mean (score)",
              col=nice[3],
              las=1)

axis(side = 1, at = bp, labels = FALSE) 

box()

mids <- bp[, 1]

for (i in 1:9){
  arrows(x0 = mids[i],
         y0 = vector[i] - std[i],
         x1 = mids[i],
         y1 = vector[i] + std[i],
         code = 3,
         angle = 90,
         length=0.05)
}

# Make BoxPlot

boxplot(evaluations[, 2:10],
        col=nice[1],
        ylim=c(0,10),
        xlab="Question",
        ylab="score",
        names=c(1:9),
        las=1)

box()


dev.off()


