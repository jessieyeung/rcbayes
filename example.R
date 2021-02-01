
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
                       post_retirement = TRUE,
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

# vignette example
library(rcbayes)

ages <- 0:80

pop <- c(2028, 2193, 2271, 2370, 2403, 2160, 2109, 2206, 2456, 2334, 2392, 2534, 2542, 2601, 2526,
         2416, 2420, 2344, 2606, 2355, 2867, 2589, 2426, 2390, 2377, 2909, 2753, 2633, 2847, 2819,
         2979, 2608, 2708, 2602, 2745, 2883, 2624, 2607, 2677, 2637, 2964, 2414, 2481, 2464, 2510,
         2695, 2552, 2711, 2794, 2683, 2888, 2439, 2631, 2814, 2854, 2999, 2959, 2852, 2957, 2985,
         2970, 2882, 2839, 2737, 2782, 2799, 2710, 2527, 2512, 2530, 2505, 2521, 2551, 2125, 1838,
         2057, 2037, 1804, 1542, 1470, 1452)

net_mig <- c(49, 48, 48, 52, 50, 45, 42, 46, 45, 44, 47, 55, 57, 59, 67, 69, 71, 78, 93, 88, 116,
             106, 102, 104, 102, 123, 112, 102, 112, 105, 100, 83, 81, 77, 78, 77, 66, 64, 65, 64,
             68, 52, 59, 51, 54, 55, 52, 58, 64, 53, 68, 53, 57, 67, 71, 78, 75, 77, 77, 83, 88,
             80, 84, 79, 77, 83, 71, 59, 65, 67, 64, 63, 56, 50, 43, 46, 46, 38, 32, 28, 29)


res <- mig_estimate_rc(ages, net_mig, pop,
                       pre_working_age = TRUE,
                       working_age = TRUE,
                       retirement = TRUE,
                       post_retirement = FALSE)

res[['check_converge']]
