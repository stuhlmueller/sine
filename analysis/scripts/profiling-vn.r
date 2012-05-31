pdf.name <- "profiling-vn.pdf"
csv.name <- "profiling-vn.csv"

getdata <- function(){
  cat("Running program...\n");
  system(paste("vic ../tests/value-number.ss | tee", csv.name));
  read.csv(csv.name, header=T);
}

lines.and.points <- function(data, col){
  lines(data, col=col);
  points(data, col=col);
}

cat("Generating plot...\n")
pdf(pdf.name);
data <- getdata();
plot(data, main="Profiling: recursive make-list")
lines.and.points(data, col="blue")
lines.and.points(getdata(), col="green")
lines.and.points(getdata(), col="red")
dev.off()

cat("Opening pdf...\n")
system(paste("open", pdf.name))
