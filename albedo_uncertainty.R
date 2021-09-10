rm(list = ls())
library(runjags)
library(rjags)
library(tidyverse)

univariate_regression <- "
model{


  c ~ dunif(0.15, 0.25)
  d ~ dunif(0.05, 0.15)
  e ~ dunif(0, 10)
  S_sd ~ dunif(0.001, 10)    ## prior precision
  S <- 1/pow(S_sd, 2)

  for(i in 1:n){
      mu[i] <- c + (d - c) * (1 - exp( -x[i] / e))     ## process model
      y[i]  ~ dnorm(mu[i],S)                ## data model
  }
}
"


######

d <- read_csv("/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_ALBEDO_AGE.csv")

d %>% 
  ggplot(aes(x = age, y = Albedo)) +
  geom_point(size = 5) +
  theme_bw() +
  xlab('Age (years)') +
  ylab(expression(Albedo))+
  theme(legend.position="right",title = element_text(size=20)
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 15)
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
  )


y = d$Albedo[!is.na(d$Albedo)]
x = d$age[!is.na(d$Albedo)]

data <- list(x = x, y = y, n = length(y))

inits <- list()
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(c = rnorm(1, 0.2, 0.01),
                     d = rnorm(1, 0.1, 0.01),
                     e = rnorm(1, 5.6, 1),
                     S_sd = runif(1, 0.1, 10))
}

j.model   <- jags.model (file = textConnection(univariate_regression),
                         data = data,
                         inits = inits,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("c", "d", "e", "S_sd"),
                            n.iter = 10000)

plot(jags.out)
gelman.diag(jags.out)

ALPHA_pred <- array(NA, dim = c(31, 5000))

chain <- as.matrix(jags.out[1])

mean(chain[4,])

for(i in 1:5000){
  s <- sample(1:nrow(chain), 1)
  x <- seq(0, 30, 1)
  pred <- chain[s, 2] + (chain[s, 3] - chain[s, 2]) * (1 - exp( -x / chain[s, 4]))
  ALPHA_pred[ , i] <- rnorm(31, pred, chain[s, 1])
}

y_upper <- rep(NA, 31)
y_lower <- rep(NA, 31)
for(i in 1:31){
  y_upper[i] <- quantile(ALPHA_pred[i, ], 0.975)
  y_lower[i] <- quantile(ALPHA_pred[i, ], 0.025)
}

plot_data_p <- tibble(age = x,
                    y = colMeans(t(ALPHA_pred)),
                    y_upper = y_upper,
                    y_lower = y_lower)

ggplot(plot_data_p, aes(x = age, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line()

samp = tibble(data.frame(ALPHA_pred[,sample(1:5000,size = 5)],row.names = c(1:30))) %>% mutate(age = c(0:29)) 

d$amflux = NA
d$amflux[d$Site == 'SBC Pine'] = 'This Study'
d$amflux[d$Site == 'North Carolina Clearcut'] = 'US-NC3'
d$amflux[d$Site == 'Duke Forest Loblolly Pine'] = 'US-DK3'
d$amflux[d$Site == 'North Carolina Loblolly Pine'] = 'US-NC2'

f = ggplot(plot_data_p, aes(x = age, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line(lwd = 2) +
  geom_point(data = d, aes(x = age, y = Albedo, fill = amflux, shape = amflux), size = 5) +
  theme_bw() +
  scale_shape_manual(values = c(21:25)) +
  ylim(0.08,0.26) +
  xlab('Age (years)') +
  ylab(expression(Albedo))+
  theme(legend.position="top",title = element_text(size=20)
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 10)
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
        ,plot.title = element_text(size=20)
  );f

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/Pine_Albedo_Age_uncertainty.pdf',width = 12, height = 8)
print(f)
dev.off()

PINE_ALPHA_X = tibble(data.frame(ALPHA_pred)) %>% mutate(age = c(0:30)) 

write_csv(PINE_ALPHA_X,'/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_ALBEDO_CHAIN.csv')

#### GRASS ####



##Constant
univariate_regression <- "
model{


  c ~ dunif(0.1, 0.3)
  S_sd ~ dunif(0.001, 10)    ## prior precision
  S <- 1/pow(S_sd, 2)

  for(i in 1:n){
      mu[i] <- c    ## process model
      y[i]  ~ dnorm(mu[i],S)                ## data model
  }
}
"

clear = d %>% filter(age == 0)
clear$Site = 'SBC Grass'
clear$amflux = NULL

g_dat = read_csv("/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_ALBEDO_AGE.csv", skip_empty_rows = T)
g_dat = g_dat %>% filter(Site != 'Kellogg')
#g_dat = g_dat %>% filter(Site == 'SBC Grass')

g_dat = bind_rows(g_dat,clear)

y = g_dat$Albedo
x = g_dat$age

plot(x, y)


data <- list(x = x, y = y, n = length(y))

inits <- list()
nchain = 3
for(i in 1:nchain){
  inits[[i]] <- list(c = rnorm(1, 0.2, 0.01),
                     #d = rnorm(1, 0.3, 0.01),
                     #e = rnorm(1, 3, 1),
                     S_sd = runif(1, 0.1, 10))
}



j.model   <- jags.model (file = textConnection(univariate_regression),
                         data = data,
                         inits = inits,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("c", "S_sd"),
                            n.iter = 10000)

plot(jags.out)
gelman.diag(jags.out)

ALPHA_pred <- array(NA, dim = c(30, 5000))

chain <- as.matrix(jags.out[1])

for(i in 1:5000){
  s <- sample(1:nrow(chain), 1)
  x <- seq(0, 29, 1)
  #pred <- chain[s, 2] + (chain[s, 3] - chain[s, 2]) * (1 - exp( -x / chain[s, 4]))
  pred <- chain[s,2]
  ALPHA_pred[ , i] <- rnorm(30, pred, chain[s, 1])
}


y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
y_up_mid <- rep(NA, 30)
y_low_mid <- rep(NA, 30)

for(i in 1:30){
  y_upper[i] <- quantile(ALPHA_pred[i, ], 0.975)
  y_lower[i] <- quantile(ALPHA_pred[i, ], 0.025)
  y_up_mid[i] <- quantile(ALPHA_pred[i, ], 0.75)
  y_low_mid[i] <- quantile(ALPHA_pred[i, ], 0.25)
}

plot_data_g <- tibble(age = x,
                    y = colMeans(t(ALPHA_pred)),
                    y_upper = y_upper,
                    y_lower = y_lower,
                    y_75 = y_up_mid,
                    y_25 = y_low_mid,
                    time = c(0:29))

samp = tibble(data.frame(ALPHA_pred[,sample(1:5000,size = 5)])) %>% mutate(age = x, time = c(1:30)) 

g_dat$Site = factor(g_dat$Site, levels = c('SBC Grass', 'ARM', 'UIEF'), labels = c('This Study', 'US-AR1', 'Miller et al 2016'
))

f2 = ggplot(plot_data_g, aes(x = time, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line(lwd = 2) +
  #geom_point(data = g_dat, aes(x = age, y = Albedo, fill = Site, shape = Site), size = 5) +
  geom_jitter(data = g_dat, aes(x = age, y = Albedo, fill = Site, shape = Site), size = 5, width = 0.1) +
  scale_shape_manual(values = c(21,22,24,25)) +
  theme_bw() +
  #xlim(0,10) +
  ylim(0.08,0.26) +
  xlab('Age (years)') +
  ylab(NULL) +
  #scale_x_continuous(breaks= seq(0,9,by=2), limits = c(-0.1,9)) +
  theme(legend.position="top",title = element_text(size=20)
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 10)
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
        ,plot.title = element_text(size=22)
  );f2


##########



patch = f + f2 + patchwork::plot_annotation(tag_levels = 'A')
patch

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/Pine_Grass_Albedo_Age_uncertainty.pdf',width = 14, height = 7)
print(patch)
dev.off()

f = ggplot(plot_data_g, aes(x = time, y = y)) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_line(lwd = 2) +
  geom_point(data = g_dat, aes(x = age, y = Albedo, fill = Site, shape = Site), size = 5) +
  scale_shape_manual(values = c(21,22,24,25)) +
  theme_bw() +
  ylim(0.08,0.26) +
  xlab('Age (years)') +
  ylab(expression(Albedo))+
  theme(legend.position="top",title = element_text(size=20)
        ,legend.title = element_blank()
        ,legend.text = element_text(size = 10)
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
        ,plot.title = element_text(size=22)
  );f

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/Grass_Albedo_Age_uncertainty_30Y.pdf', width = 12, height = 8)
print(f)
dev.off()

GRASS_ALPHA_X = tibble(data.frame(ALPHA_pred)) %>% mutate(time = c(1:30), age = x) 

write_csv(GRASS_ALPHA_X,'/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_ALBEDO_CHAIN.csv')

GRASS_ALPHA_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_ALBEDO_CHAIN.csv')
PINE_ALPHA_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_ALBEDO_CHAIN.csv')

mean(unlist(c(PINE_ALPHA_X[1,5000])))
sd(unlist(c(PINE_ALPHA_X[1,1:5000])))

rowMeans(PINE_ALPHA_X[,1:5000])


mean(unlist(c(PINE_ALPHA_X[31,1:5000])))
sd(unlist(c(PINE_ALPHA_X[31,1:5000])))



mean(colMeans(GRASS_ALPHA_X[1,1:5000]))
sd(colMeans(GRASS_ALPHA_X[1,1:5000]))
