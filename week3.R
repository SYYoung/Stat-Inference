week3_1 <- function() {
    # Bootstrap example
    B <- 10000
    resamples <- matrix(sample(x, n*B, replace=TRUE), B, n)
    medians <- apply(resamples, 1, median)
    sd(medians)
    quantile(medians, c(0.025,0.975))
    g <- ggplot(data.frame(medians=medians), aes(x=medians))
    g <- g + geom_histogram(color="black",fill="lightblue", binwidth=0.05)
    g
    
}

week3_2 <- function() {
    subdata <- InsectSprays[InsectSprays$spray %in% c("B","C"),]
    y <- subdata$count
    group <- as.character(subdata$spray)
    testStat <- function(w,g) mean(w[g="B"]) - mean(w[g="C"])
    observedStat <- testStat(y,group)
    permutations <- sapply(1:10000, function(i) testStat(y,sample(group)))
    observedStat
    
    mean(permutations > observedStat)
    hist(permutations)
    abline(v=observedStat)
}