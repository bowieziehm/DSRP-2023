readableSetName <- "data/imbd.csv"
set <- read.csv(readableSetName)
View(set)



# gross analysis ####
gross <- na.omit(set$gross)

meanGross <- mean(gross)
medianGross <- median(gross)
rangeGross <- max(gross) - min(gross)
standardDeviationGross <- sd(gross)
varianceGross <- var(gross)
interQuartileRange = IQR(gross)


#outliers removed analysis ####

#threshhold calculation
lower <- meanGross - 3 * standardDeviationGross
upper <- meanGross + 3 * standardDeviationGross

outliersRemoved <- subset(gross, gross > lower & gross < upper)

oRmeanGross <- mean(outliersRemoved)
oRmedianGross <- median(outliersRemoved)
oRrangeGross <- max(outliersRemoved) - min(outliersRemoved)
oRstandardDeviationGross <- sd(outliersRemoved)
oRvarianceGross <- var(outliersRemoved)
oRinterQuartileRange = IQR(outliersRemoved)
