
set.seed(0)
rep <- 10000
seeds <- sample(928397, rep)
tasks <- expand.grid(n = c(100, 200, 500, 1000), seed = seeds, eff = c(0, 2, 4))

tau <- 14

sim <- function(i) {

  n <- tasks[i, 'n']
  eff <- tasks[i, 'eff']
  seed <- tasks[i, 'seed']

  set.seed(seed)

  data <- gendata(dat, n, eff)
  return(data)
}


expit <- function(arg) exp(arg) / (1 + exp(arg))
logit <- function(arg) log(arg / (1 - arg))

x <- seq(0, 1, by=0.01)
beta0 <- 1
beta1 <- 2
delta <- 0.5

y1 <- beta0 + beta1 * x
y2 <- beta0 + beta1 * x - delta

par(mfrow=c(1, 2))
plot(y1 ~ x, type='l')
lines(y2 ~ x, type='l', col='red')
plot(expit(y1) ~ x, type='l')
lines(expit(y2) ~ x, type='l', col='red')


age <- c("0-19", "20-44",
         "45-54", "55-64",
         "65-74", "75-84", "85+")
page <- c(0.004, 0.189, 0.162, 0.165, 0.225,
          0.143, 0.112)
page.death <- c(0.000, 0.009, 0.026, 0.079, 0.106, 0.166, 0.371)
page.icu <- c(0.000, 0.177, 0.319, 0.314, 0.373, 0.465, 0.347)
page.none <- c(1.000, 0.815, 0.655, 0.697, 0.521, 0.369, 0.281)

df <- data.frame(
  age=rep(age, 3),
  type=rep(c("death", "icu", "none"), each=7),
  prob=c(page.death, page.icu, page.none)
)

age.df <- data.frame(
  age=age,
  prob=page
)


library(ggplot2)
library(viridis)

# Stacked + percent
pdf("cond-dist.pdf", height=5, width=5)
ggplot(df, aes(fill=type, y=prob, x=age)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(discrete = T) +
  xlab("Age") + ylab("Probability") +
  labs(fill="Outcome") +
  theme_minimal()
dev.off()

pdf("dist.pdf", height=2, width=5)
ggplot(age.df, aes(y=prob, x=age)) + geom_bar(stat="identity",
                                          position="dodge") +
  theme_minimal() +
  xlab("Age") + ylab("Probability")
dev.off()

library(survival)

ex <- sim(3) %>% data.table
setnames(ex, "T", "time")
ex2 <- sim(50001) %>% data.table
setnames(ex2, "T", "time")
ex3 <- sim(100011) %>% data.table
setnames(ex3, "T", "time")

s1 <- with(ex, Surv(time, D))
s2 <- with(ex2, Surv(time, D))
s3 <- with(ex3, Surv(time, D))

pdf("surv-time.pdf", height=4, width=10)
par(mfrow=c(1, 3))
plot(survfit(s1 ~ A, ex), col=c("red", "blue"),
     xlim=c(0, 14), main=c("No Treatment Effect"),
     ylim=c(0.8, 1),
     xlab="Days",
     ylab="Survival")
plot(survfit(s2 ~ A, ex2), col=c("red", "blue"),
     xlim=c(0, 14), main=c("RMST about 0.5"),
     ylim=c(0.8, 1),
     xlab="Days")
plot(survfit(s3 ~ A, ex3), col=c("red", "blue"),
     xlim=c(0, 14), main=c("RMST about 1.0"),
     ylim=c(0.8, 1),
     xlab="Days")
dev.off()
