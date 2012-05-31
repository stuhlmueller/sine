pdf.name <- "profiling-interpreter.pdf"
csv.name <- "profiling-interpreter.csv"

getdata <- function(){
  cat("Running program...\n");
  system(paste("vic ../tests/interpreter.ss | tee", csv.name));
  read.csv(csv.name, header=T);
}

lines.and.points <- function(data, col){
  lines(data, col=col);
  points(data, col=col);
}

data.subset <- function(data, col.name){
  matrix(c(data[["n"]], data[[col.name]]), length(data[["n"]]))
}

cat("Generating plot...\n")
pdf(pdf.name, width=9, height=5);

par(mfrow=c(1,2))

d1 <- getdata();
d2 <- getdata();
d3 <- getdata();
s1 <- data.subset(d1, "time");
s2 <- data.subset(d2, "time");
s3 <- data.subset(d3, "time");
plot(s1, main="Profiling: recursive make-list (time)", xlab="n", ylab="time (s)")
lines.and.points(s1, col="blue")
lines.and.points(s2, col="green")
lines.and.points(s3, col="red")

s1 <- data.subset(d1, "obj.store");
s2 <- data.subset(d2, "obj.store");
s3 <- data.subset(d3, "obj.store");
plot(s1, main="Profiling: recursive make-list (space)", xlab="n", ylab="space (#entries)")
lines.and.points(s1, col="blue")
lines.and.points(s2, col="green")
lines.and.points(s3, col="red")

## plot(data, main="Profiling: recursive make-list (space)")

dev.off()

cat("Opening pdf...\n")
system(paste("open", pdf.name))
