set.seed(123)
n<-100
walking_time<-runif(n,min=0,max=10)
waiting_onstreet<-runif(n,min=0,max=10)
waiting_offstreet<-runif(n,min=0,max=10)
beta_w<--0.1
beta_on<--0.2
beta_off<--0.15
p_target<-0.99
logit_target<-log(p_target/(1-p_target))
walk_star<-0
on_star<-5
off_star<-10
beta_0 <- logit_target - (
  beta_w   * walk_star +
    beta_on  * on_star   +
    beta_off * off_star
)
beta_0
eta <- beta_0 +
  beta_w   * walking_time +
  beta_on  * waiting_onstreet +
  beta_off * waiting_offstreet
p <- 1 / (1 + exp(-eta))
choice <- rbinom(n, size = 1, prob = p)
taxi_data <- data.frame(
  choice,
  walking_time,
  waiting_onstreet,
  waiting_offstreet
)
head(taxi_data)
summary(taxi_data)
model <- glm(
  choice ~ walking_time + waiting_onstreet + waiting_offstreet,
  data   = taxi_data,
  family = binomial(link = "logit")
)
summary(model)
