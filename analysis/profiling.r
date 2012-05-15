pdf.name <- "profiling.pdf"
csv.name <- "profiling.csv"

cat("Running interpreter...\n")
system(paste("vic ../tests/interpreter.ss | tee", csv.name))

cat("Generating plot...\n")
pdf(pdf.name)
data <- read.csv(csv.name, header=T)
plot(data, main="Profiling: recursive make-list")
lines(data)
dev.off()

cat("Opening pdf...\n")
system(paste("open", pdf.name))
