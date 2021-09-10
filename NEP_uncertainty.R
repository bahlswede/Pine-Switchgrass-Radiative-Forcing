rm(list = ls())
library(tidyverse)
library(runjags)
library(rjags)
library(patchwork)


univariate_regression <- "
model{


  b1 ~ dunif(-5000, 5000)
  b2 ~ dunif(-5000, 5000)
  b3 ~ dunif(0, 100)
  S_sd ~ dunif(0.001, 1000)    ## prior precision
  S <- 1/pow(S_sd, 2)

  for(i in 1:n){
      mu[i] <- b1 + (b2 - b1) * (x[i] / (x[i] + b3))     ## process model
      y[i]  ~ dnorm(mu[i],S)                ## data model
  }
}
"

######

d <- read_csv("/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_NEP_AGE.csv") %>% 
  rename(NEP = `NEP pub`,
         age = `age (years)`) %>%
  filter(!is.na(NEP))

y = d$NEP[!is.na(d$NEP)]
x = d$age[!is.na(d$NEP)]

plot(y~x)

data <- list(x = x, y = y, n = length(y))

inits <- list()
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b1 = rnorm(1, -1079.735190, 500),
                     b2 = rnorm(1, 1116.048092, 500),
                     b3 = rnorm(1, 3.407749, 1),
                     S_sd = runif(1, 100, 300))
}

j.model   <- jags.model (file = textConnection(univariate_regression),
                         data = data,
                         inits = inits,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("b1", "b2", "b3", "S_sd"),
                            n.iter = 10000)

plot(jags.out)
gelman.diag(jags.out)

NEP_pred <- array(NA, dim = c(31, 5000))

chain <- as.matrix(jags.out[1])
mean(chain[,4])

for(i in 1:5000){
  s <- sample(1:nrow(chain), 1)
  x <- seq(0, 30, 1)
  pred <- chain[s, 2] + (chain[s, 3] - chain[s, 2]) * (x / (x+ chain[s, 4])) 
  NEP_pred[ , i] <- rnorm(31, pred, chain[s, 1])
}

y_upper <- rep(NA, 31)
y_lower <- rep(NA, 31)
for(i in 1:31){
  y_upper[i] <- quantile(NEP_pred[i, ], 0.975)
  y_lower[i] <- quantile(NEP_pred[i, ], 0.025)
}

plot_data_p <- tibble(age = x,
                    y = colMeans(t(NEP_pred)),
                    y_upper = y_upper,
                    y_lower = y_lower)

ggplot(plot_data_p, aes(x = age, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line()

samp = tibble(data.frame(NEP_pred[,sample(1:5000,size = 5)],row.names = c(1:31))) %>% mutate(age = c(0:30)) 

d$Citation = NA
d$Citation[d$Reference == 'Clark et al. 2004. Eco Apps 14:1154'] = 'Clark et al. (2004)'
d$Citation[d$Reference == 'Bracho et al, 2012 Ecol Mono.'] = 'Bracho et al. (2012)'
d$Citation[d$Reference == 'TBD'] = 'This Study'

d$Citation = factor(d$Citation, levels = c('This Study', 'Clark et al. (2004)', 'Bracho et al. (2012)'))

f = ggplot(plot_data_p, aes(x = age, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line(lwd = 2) +
  geom_point(data = d, aes(x = age, y = NEP, pch = Citation, fill = Citation), size = 5) +
  theme_bw() +
  ylim(-1600,1400) +
  xlab('Age (years)') +
  scale_shape_manual(values = c(21:25)) +
  
  ylab(expression(NEP~(gC~~m^-2))) +
  theme(legend.position="top",title = element_text(size=20)
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 10)
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
  );f

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/Pine_NEP_Age_uncertainty.pdf',width = 12, height = 8)
print(f)
dev.off()

PINE_NEP_X = tibble(data.frame(NEP_pred)) %>% mutate(age = c(0:30)) 

### GRASS ##

univariate_regression <- "
model{


  b1 ~ dunif(-1000, 1000)
  b2 ~ dunif(-1000, 1000)
  b3 ~ dunif(0, 10)
  S_sd ~ dunif(0.001, 1000)    ## prior precision
  S <- 1/pow(S_sd, 2)

  for(i in 1:n){
      mu[i] <- b1 + (b2 - b1) * (x[i] / (x[i] + b3))     ## process model
      y[i]  ~ dnorm(mu[i],S)                ## data model
  }
}
"

# univariate_regression <- "
# model{
# 
# 
#   c ~ dunif(-500, 500)
#   S_sd ~ dunif(0.001, 1000)    ## prior precision
#   S <- 1/pow(S_sd, 2)
# 
#   for(i in 1:n){
#       mu[i] <- c    ## process model
#       y[i]  ~ dnorm(mu[i],S)                ## data model
#   }
# }
# "

names = names(read_csv("/Users/Ben/Dropbox/Dissertation/Chapter 1/summary_tables/SG_EC_Summary.csv", skip_empty_rows = T))

g_dat = read_csv("/Users/Ben/Dropbox/Dissertation/Chapter 1/summary_tables/SG_EC_Summary.csv", skip = 2, skip_empty_rows = T, col_names = names)
g_dat =  g_dat %>% filter(Measurment == "Year Round" & !is.na(NEE) & Location != "Bolgna, IT") %>%
  mutate(NEP = -NEE) %>%
  rename(age = `Stand Age`)
clear = d %>% filter(age == 0 & Reference == 'TBD') %>% select(NEP, age, Reference)
clear$Reference = 'This Study'
g_dat2 = g_dat
g_dat = bind_rows(g_dat,clear)



y = c(g_dat$NEP)
x = c(g_dat$age)

# y = c(g_dat2$NEP)
# x = c(g_dat2$age)


plot(x, y)


data <- list(x = x, y = y, n = length(y))

inits <- list()
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(#c = rnorm(1,0,100),
    b1 = rnorm(1, -600, 100),
    b2 = rnorm(1, 600, 100),
    b3 = rnorm(1, 3, .1),
    S_sd = runif(1, 0, 1000))
}

j.model   <- jags.model (file = textConnection(univariate_regression),
                         data = data,
                         inits = inits,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("b1", "b2", "b3", "S_sd"),
                            #variable.names = c("c", "S_sd"),
                            n.iter = 10000)

plot(jags.out)
gelman.diag(jags.out)

NEE_pred <- array(NA, dim = c(30, 5000))

chain <- as.matrix(jags.out[1])
mean(chain[,'b1'])
mean(chain[,'b2'])
mean(chain[,'b3'])

for(i in 1:5000){
  s <- sample(1:nrow(chain), 1)
  x <- c(0:9,1:10,1:10)
  pred <- chain[s, 2] + (chain[s, 3] - chain[s, 2]) * (x / (x+ chain[s, 4])) 
  #pred <- chain[s, 2]
  NEE_pred[ , i] <- rnorm(30, pred, chain[s, 1])
}

plot(chain[, 4], chain[ , 3])

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- quantile(NEE_pred[i, ], 0.975)
  y_lower[i] <- quantile(NEE_pred[i, ], 0.025)
}

plot_data_g <- tibble(age = x,
                    y = colMeans(t(NEE_pred)),
                    y_upper = y_upper,
                    y_lower = y_lower,
                    time = c(0:29))

# plot_data_g2 <- tibble(age = x,
#                       y = colMeans(t(NEE_pred)),
#                       y_upper = y_upper,
#                       y_lower = y_lower,
#                       time = c(0:29))

samp = tibble(data.frame(NEE_pred[,sample(1:5000,size = 5)])) %>% mutate(age = x, time = c(1:30)) 

g_dat$Reference = factor(g_dat$Reference, levels = c('This Study', 'Eichelmann et al. (2016)', 'Skinner & Adler (2010)', 'Zeri et al. (2011)', 'Abraha et al (2018)'))

f2 = ggplot(plot_data_g, aes(x = time, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line(lwd = 2) +
  
  # geom_line(aes(y = y_lower),data = plot_data_g2, lty = 3) +
  # geom_line(aes(y = y_upper),data = plot_data_g2, lty = 3) +
  # geom_line(data = plot_data_g2, lty = 2, col = 'red') +
  #geom_point(data = g_dat, aes(x = age, y = -NECB, pch = Reference, fill = Reference), size = 5) +
  #geom_jitter(data = g_dat, aes(x = age, y = -NECB, pch = Reference, fill = Reference), size = 5, width = 0.1) +
  geom_jitter(data = g_dat, aes(x = age, y = NEP, pch = Reference, fill = Reference), size = 5, width = 0.1) +
  theme_bw() +
  xlab('Time (years)') +
  ylab(NULL) +
  #ylab(expression(NECB~(gC~~m^-2))) +
  #scale_x_continuous(breaks= seq(0,9,by=2), limits = c(-0.1,9)) +
  ylim(-1600,1400) +
  scale_shape_manual(values = c(21:25)) +
  
  theme(legend.position="top",title = element_text(size=20)
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 10)
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
  );f2

f2 = f2+ guides(fill = guide_legend(nrow = 3), pch = guide_legend(nrow = 3))

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/Grass_NEP_Age_uncertainty_Constant_SBC.pdf',width = 8, height = 4)
print(f2)
dev.off()

f3 = ggplot(plot_data_g, aes(x = time, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line(lwd = 2) +
  geom_point(data = g_dat, aes(x = age, y = NEP, pch = Reference, fill = Reference), size = 5) +
  theme_bw() +
  xlab('Time (years)') +
  ylim(-1600,1400) +
  scale_shape_manual(values = c(21:25)) +
  ylab(expression(NEP~(gC~~m^-2))) +
  theme(legend.position="top",title = element_text(size=20)
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 10)
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
  );f3
f3 = f3 + guides(fill = guide_legend(nrow = 3), pch = guide_legend(nrow = 2))

patch = f + f2 + patchwork::plot_annotation(tag_levels = 'A')
patch

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/Pine_Grass_NEP_Age_uncertainty.pdf',width = 14, height = 7)
print(patch)
dev.off()

GRASS_NEP_X = tibble(data.frame(NEE_pred)) %>% mutate(time = c(1:30), age = x) 

write_csv(GRASS_NEP_X,'/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_NEP_CHAIN.csv')
write_csv(PINE_NEP_X,'/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_NEP_CHAIN.csv')

PINE_NEP_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_NEP_CHAIN.csv')
mean(unlist(c(PINE_NEP_X[1,])))
sd(unlist(c(PINE_NEP_X[1,])))

rowMeans(PINE_NEP_X)
mean(colSums(PINE_NEP_X[,1:5000]))
sd(colSums(PINE_NEP_X[,1:5000]))

mean(unlist(c(PINE_NEP_X[31,])))
sd(unlist(c(PINE_NEP_X[31,])))

GRASS_NEP_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_NEP_CHAIN.csv')
rowMeans(GRASS_NEP_X)
mean(unlist(c(GRASS_NEP_X[10,])))
sd(unlist(c(GRASS_NEP_X[10,])))

mean(colSums(GRASS_NEP_X[,1:5000]))
sd(colSums(GRASS_NEP_X[,1:5000]))


