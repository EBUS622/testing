set.seed(123)
n<-300
walking_time<-runif(n,min=0,max=10)
waiting_onstreet<-runif(n,min=0,max=10)
waiting_offstreet<-runif(n,min=0,max=10)
beta_w<--0.5
beta_on<--0.5
beta_off<--0.15
p_target<-0.5
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
library(ggplot2)
library(scales)
coefs <- coef(model)
mins <- seq(0, 10, length.out = 100)
m_walk <- mean(taxi_data$walking_time)
m_on   <- mean(taxi_data$waiting_onstreet)
m_off  <- mean(taxi_data$waiting_offstreet)
eta_walk <- coefs["(Intercept)"] +
  coefs["walking_time"]      * mins +
  coefs["waiting_onstreet"]  * m_on +
  coefs["waiting_offstreet"] * m_off
p_walk <- 1 / (1 + exp(-eta_walk))
eta_on <- coefs["(Intercept)"] +
  coefs["walking_time"]      * m_walk +
  coefs["waiting_onstreet"]  * mins +
  coefs["waiting_offstreet"] * m_off
p_on <- 1 / (1 + exp(-eta_on))
eta_off <- coefs["(Intercept)"] +
  coefs["walking_time"]      * m_walk +
  coefs["waiting_onstreet"]  * m_on +
  coefs["waiting_offstreet"] * mins
p_off <- 1 / (1 + exp(-eta_off))
plotdata <- rbind(
  data.frame(minutes = mins, pred_prob = p_walk,
             variable = "Walking time"),
  data.frame(minutes = mins, pred_prob = p_on,
             variable = "On-street waiting time"),
  data.frame(minutes = mins, pred_prob = p_off,
             variable = "Off-street waiting time")
)
ggplot(plotdata, aes(x = minutes, y = pred_prob, colour = variable)) +
  geom_line() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),  # show as 0%, 10%, 20%, ...
    limits = c(0, 1)
  ) +
  labs(
    x = "Minutes",
    y = "Predicted probability of choosing EV taxi (%)",
    colour = "Variable",
    title = "Logistic regression model: effect of time variables",
    subtitle = "Each line varies one variable (0–10 minutes), others held at their mean"
  )
