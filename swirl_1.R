swirl_t_conf <- function() {
    # g1, g2 are paired
    difference <- g2-g1
    mn <- mean(difference)
    s <- sd(difference)
    mn + c(-1,1)*qt(.975,9)*s/sqrt(10)
    # or use t.test
    t.test(g2, g1, paired = TRUE)$conf.int
    t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int
    
    # 2 indep groups, assume common variances
    # new_sp <- sqrt((n_x-1)(s_x)^2 + (n_y-1)*(s_y)^2)/(n_x+n_y-2))
    # conf int = 
    # (x1-x2) + c(-1,1)*qt(0.975,ns)*sp*sqrt(1/n1 + 1/n2)
    # answer: -9.52,20.36.  since 0 lies in this range, therefore, we cannot
    # reject H0.
    t.test(g2,g1,paired=FALSE,var.equal=TRUE)$conf
    
    # 3 indep groups, unequal variances
    SE = (s1^2/n1) + (s2^2/n2)
    # when underlying X and Y data are iid normal and variances are different,
    # the normalized stat we started with, doesn't follow a t distribution.
    
}

swirl_hypothesis <- function() {
    # use fs as data
    t.test(fs$sheight-fs$fheight)
    # or other
    t.test(fs$sheight,fs$fheight,paired=TRUE)
    
}

swirl_pvalue <- function() {
    pt(q=2.5,df=15,lower.tail=FALSE)
    # for binominal p value
    pbinom(6,size=8,prob=0.5,lower.tail=FALSE)
    # poisson
    ppois(9,lambda=5,lower.tail=FALSE)
    
}

swirl_power <- function() {
    # power = pnorm with: 30+Z_95 * (sigma/sqrt(n))
    z <- qnorm(.95)
    pnorm(30+z*1, mean=32, sd=1, lower.tail=FALSE)
    
    # n=16, alpha=0.05, effect size=mu_a-mu_0
    # once the ratio of (mu_a-mu_0)/sd is same, power is same
    power.t.test(n=16,delta=2/4,sd=1,type="one.sample",alt="one.sided")$power
    
    power.t.test(power=.8,delta=2/4,sd=1,type="one.sample",alt="one.sided")$n
    power.t.test(power=.8,n=26,sd=1,type="one.sample",alt="one.sided")$delta
        
}

swirl_multi_test <- function() {
    # positive discovery rate: Declared significant & H0 is true / total declared significant
    # false positive rate: declared significant and H0 is true/total H0 is true
    # Family wise error rate : Pr(V>=1), Pr(declard significant & H0 is true >=1)
    
    # pValues has 51 items out of 1000 has value < 0.05. however, there is no relationship. therefore,
    # all 51 items are false alarms
    sum(p.adjust(pValues,method="bonferroni") < 0.05)
    # answer is 0.
    sum(p.adjust(pValues,method="BH") < 0.05)
    # another array, trueStatus: first 500 random, second 500 related
    table(pvalues2<0.05, trueStatus)
    # without correction, type1 error: 24
    table(p.adjust(pValues2, method="bonferroni")<0.05, trueStatus)
    # now no type1 error, but more type2 error
    table(p.adjust(pValues, method="BH")<0.05, trueStatus)
}

testStat <- function(w,g) {
  mean(w[g=="B"]) - mean(w[g=="C"])  
}

swirl_resample <- function() {
    # bootstrap principle used observed data to construct an estimated population distribution using
    # random sampling with replacement.
    # first simulating B complete data sets from observed data by sampling with replacement.
    # make sure B is large to create data sets the same size as the original
    # the only assumption behind it is that the observed sample is representative of the underlying
    # population
    sam <- sample(fh, nh*B, replace=TRUE)
    resam <- matrix(sample,B,nh)
    meds <- apply(resam, 1, median)
    # find the quantile of sons data
    quantile(resampledMedians, c(0.025,0.975))
    
    # now permutation test
    # permutation testing is based on the idea of exchanability of group labels. 
    obs <- testStat(BCcounts, group)
    perms <- sapply(1:10000, function(i) testStat(BCcounts, sample(group)))
    mean(perms - obs)
    # it is centered in zero. but obs is 13.5. therefore, label is significant
    
    
}