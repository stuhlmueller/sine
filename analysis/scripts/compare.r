model <- "bayesnet"

cosh <- read.csv(paste("data/", model, "-cosh.csv", sep=""), header=T)
enum <- read.csv(paste("data/", model, "-enum.csv", sep=""), header=T)
rejection <- read.csv(paste("data/", model, "-rejection.csv", sep=""), header=T)

pdf(paste(model, ".pdf", sep=""), width=9, height=6)
plot(enum, ylim=c(0,1))
lines(enum)
points(enum)
lines(cosh)
points(cosh)
lines(rejection)
points(rejection)

dev.off()
