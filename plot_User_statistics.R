#!/usr/bin/env Rscript

# Read file
df <- read.csv("statistics_nginx_cOmicsArt.csv")
df$Date <- as.Date(df$Date, format = "%d/%b/%Y")
df <- df[order(df$Date), ]

# Save plot to PDF
pdf("counts_over_time.pdf", width = 7, height = 5)
plot(df$Date, df$Count,
     type = "b",                 # "b" = points + lines
     xlab = "Date",
     ylab = "Count",
     main = "Counts over Time",
     pch = 19)                   # solid points
dev.off()

cat("Plot saved!\n")
