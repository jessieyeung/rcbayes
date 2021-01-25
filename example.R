
# example
library(rcbayes)

ages <- 0:80
net_mig <- c(202,215,167,188,206,189,164,
             158,197,185,176,173,167,198,
             203,237,249,274,319,345,487,
             491,521,505,529,527,521,529,
             507,484,467,439,399,399,380,
             368,310,324,289,292,270,269,
             285,254,245,265,257,258,263,
             253,346,293,332,346,349,355,
             386,346,344,352,331,320,307,
             320,310,258,254,243,256,263,
             183,169,172,160,166,113,132,
             111,130,110,113)
pop <- c(105505,105505,105505,105505,105505,
         106126,106126,106126,106126,106126,
         100104,100104,100104,100104,100104,
         114880,114880,114880,114880,114880,
         136845,136845,136845,136845,136845,
         136582,136582,136582,136582,136582,
         141935,141935,141935,141935,141935,
         134097,134097,134097,134097,134097,
         130769,130769,130769,130769,130769,
         133718,133718,133718,133718,133718,
         154178,154178,154178,154178,154178,
         145386,145386,145386,145386,145386,
         126270,126270,126270,126270,126270,
         108314,108314,108314,108314,108314,
         79827,79827,79827,79827,79827,59556,
         59556,59556,59556,59556,59556)
res <- mig_estimate_rc(ages, net_mig, pop,
                       pre_working_age = TRUE,
                       working_age = TRUE,
                       retirement = TRUE,
                       post_retirement = TRUE, iter=100,
                       #control = list(adapt_delta = 0.95, max_treedepth = 10),
                       init = init_rc(ages, net_mig, pop,
                                             pre_working_age=TRUE, working_age=TRUE,
                                             retirement=TRUE, post_retirement=TRUE))

plot(ages, y/pop, ylab = "migration rate", xlab = "age")
lines(ages, res[["fit_df"]]$median, col = "red")
legend("topright", legend=c("data", "fit"), col=c("black", "red"), lty=1, pch = 1)

res[["fit_df"]] %>%
  ggplot(aes(ages, data)) +
  geom_point(aes(color = "data")) +
  geom_line(aes(x = ages, y = median, color = "model")) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(name = "", values = c(data = "red", model = "black", true = "blue")) +
  ylab("migration rate")

iv <- init_rc(ages, net_mig, pop, TRUE, TRUE, TRUE, TRUE)

res <- mig_estimate_rc(ages, net_mig, pop,
                       pre_working_age = TRUE,
                       working_age = TRUE,
                       retirement = TRUE,
                       post_retirement = TRUE,
                       init = iv)


