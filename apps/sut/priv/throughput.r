#!/usr/bin/env Rscript

file <- tail(commandArgs(), 1)
data <- read.csv(file)
cat(mean((data["successful"]/data["window"])[,]))

