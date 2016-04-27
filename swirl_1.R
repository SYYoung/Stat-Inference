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
    
}