assg4_1 <- function() {
    # distribution of mean of 40 exponentials
    # 1. compare sample mean and theoretical mean
    # first observe the 40 exponentials and 1000*40 
    hist(rexp(40, 0.2),main="Histogram of 40 exponential data")
    hist(rexp(40*1000, 0.2))
    
    # 2. compare sample variance and theoretical variance
    # theoretical mean = 1/0.2 = 5
    samp <- rexp(40*1000, 0.2)
    samp1 <- matrix(samp, 1000, 40)
    samp_mean <- apply(samp1, 1, mean)
    hist(samp_mean, main="distribution of mean of 40 exp")
    m1 <- mean(samp_mean)
    print(paste("the sample mean is: ", m1))
    abline(v=mean(samp_mean))
    text(200,200, "mean is: ",)
    
    # 3. show distribution is approximately normal
    # difference of a large collection of randome exp and the distribution
    # of a large collection of average 40 exp
    samp_sd <- apply(samp1, 1, sd)
    hist(samp_sd, main="dist of sd")
    # list the (0.25, 0.975) of sample variance
    print(summary(samp_sd))
    print(paste("the 1st and 3rd quant of sd is: ",
                quantile(samp_sd, c(0.25,0.975))))
}

assg4_2 <- function() {
    # analyze data: ToothGrowth data
    # 1. basic summary of the data
    data(ToothGrowth)
    tg_vc <- subset(ToothGrowth, supp=="VC")
    tg_oc <- subset(ToothGrowth, supp=="OJ")
    print(summary(tg_vc))
    print(summary(tg_oc))
    par(mfrow=c(1,2))
    plot(tg_vc$len)
    plot(tg_oc$len)
    
    # 2. use conf int and hypo tests to compare tooth growth by supp and dose
    
    # 3. state conclusion and assumptions
}