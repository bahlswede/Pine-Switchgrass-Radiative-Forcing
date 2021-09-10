rm(list = ls())
require(tidyverse)
require(abind)

GRA_ALBEDO_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_ALBEDO_CHAIN.csv')
ENF_ALBEDO_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_ALBEDO_CHAIN.csv')
ENF_ALBEDO_X = ENF_ALBEDO_X[-30,]

GRA_NEP_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_NEP_CHAIN.csv')
ENF_NEP_X = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_NEP_CHAIN.csv')
ENF_NEP_X = ENF_NEP_X[-30,]

GRA_ALBEDO_MONTH = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRA_ALBEDO_MONTH.csv')
ENF_ALBEDO_MONTH = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/ENF_ALBEDO_MONTH.csv')

GRA_ALBEDO_DAT= read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/Grass_Albedo_Month_Year.csv')
ENF_ALBEDO_DAT= read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/Pine_Albedo_Month_Year.csv')
CLR_ALBEDO_DAT= read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/Clear_Albedo_Month_Year.csv')

GRA_NEP_DAT = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/GRASS_ANNUAL.csv')
ENF_NEP_DAT = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/PINE_ANNUAL.csv')
CLR_NEP_DAT = read_csv('/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/CLEAR_ANNUAL.csv')

TMP = GRA_NEP_X %>% dplyr::select(X1:X5000)
GRA_NEP_MEAN = tibble(NEP = colMeans(t(TMP)), Time = c(1:30))

TMP = ENF_NEP_X %>% dplyr::select(X1:X5000)
ENF_NEP_MEAN = tibble(NEP = colMeans(t(TMP)), Time = c(1:30))

ENF_NEP_MEAN$NECB = ENF_NEP_MEAN$NEP
ENF_NEP_MEAN$NECB[30] = ENF_NEP_MEAN$NECB[30] - 6874.55

TMP = GRA_ALBEDO_X %>% dplyr::select(X1:X5000)
TMP %>% summarise()

GRA_ALBEDO_MEAN = tibble(Albedo = colMeans(t(TMP)))

TMP = ENF_ALBEDO_X %>% dplyr::select(X1:X5000)
ENF_ALBEDO_MEAN = tibble(Albedo = colMeans(t(TMP)))

rm(TMP)

ENF_NECB_X = ENF_NEP_X
pine_yield = 4 * 0.5 * 907185 / 4046.86
ENF_NECB_X[2:30,1:5000] = ENF_NEP_X[2:30,1:5000] - pine_yield

GRA_NECB_X = GRA_NEP_X
grass_yield = 8.7 * 100 * 0.42
GRA_NECB_X[c(2:10,12:20,22:30),1:5000] = GRA_NEP_X[c(2:10,12:20,22:30),1:5000]  - grass_yield


#######Carbon RF#########


spy = 60*60*24*365

DIF_NEP_X = ENF_NEP_X %>% dplyr::select(X1:X5000) - GRA_NEP_X %>% dplyr::select(X1:X5000)
DIF_NECB_X = ENF_NECB_X %>% dplyr::select(X1:X5000) - GRA_NECB_X %>% dplyr::select(X1:X5000)
DIF_All_None_X = ENF_NECB_X %>% dplyr::select(X1:X5000) - GRA_NEP_X %>% dplyr::select(X1:X5000)
DIF_None_All_X = ENF_NEP_X %>% dplyr::select(X1:X5000) - GRA_NECB_X %>% dplyr::select(X1:X5000)

Ma = 28.966 #molar mass of atmosphere
Mc = 12.01 #molar mass of carbon
ma = 5.148e21 #mass of atmospher


f = 5.35 * log(1 + (( (1e3/2) /  2.134e15 )/ 400), base = exp(1)) * 510e12

DIF_RE_X1 =  - DIF_NEP_X * 1e-3 * f
DIF_RE_X2 =  - DIF_NECB_X * 1e-3 * f
DIF_RE_X3 =  - DIF_All_None_X * 1e-3 * f
DIF_RE_X4 =  - DIF_None_All_X * 1e-3 * f

CLR_NEP_DAT$RFC = CLR_NEP_DAT$NEE * 1e-3 * f
GRA_NEP_DAT$RFC = GRA_NEP_DAT$NEE * 1e-3 * f
ENF_NEP_DAT$RFC = ENF_NEP_DAT$NEE * 1e-3 * f

#86400 * 5.35 * log(1 + ((-1e6/2.123e15)/ 390), base = exp(1)) * 510e12 * 365 * 1e-09 should == 104



######ALBEDO RF#######
kernel = c(68.898
           ,92.537
           ,120.638
           ,155.796
           ,167.547
           ,190.012
           ,176.585
           ,160.503
           ,134.136
           ,104.100
           ,80.064
           ,58.545
)

DIF_ALBEDO_Jan = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[1]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[1])) * kernel[1]
DIF_ALBEDO_Feb = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[2]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[2])) * kernel[2]
DIF_ALBEDO_Mar = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[3]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[3])) * kernel[3]
DIF_ALBEDO_Apr = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[4]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[4]))* kernel[4]
DIF_ALBEDO_May = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[5]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[5]))* kernel[5]
DIF_ALBEDO_Jun = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[6]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[6]))* kernel[6]
DIF_ALBEDO_Jul = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[7]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[7]))* kernel[7]
DIF_ALBEDO_Aug = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[8]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[8]))* kernel[8]
DIF_ALBEDO_Sep = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[9]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[9]))* kernel[9]
DIF_ALBEDO_Oct = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[10]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[10]))* kernel[10]
DIF_ALBEDO_Nov = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[11]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[11]))* kernel[11]
DIF_ALBEDO_Dec = ((ENF_ALBEDO_X[,1:5000] + ENF_ALBEDO_MONTH$offset[12]) - (GRA_ALBEDO_X[,1:5000] + GRA_ALBEDO_MONTH$offset[12]))* kernel[12]

TMP = abind(as.matrix(DIF_ALBEDO_Jan)
      ,as.matrix(DIF_ALBEDO_Feb)
      ,as.matrix(DIF_ALBEDO_Mar)
      ,as.matrix(DIF_ALBEDO_Apr)
      ,as.matrix(DIF_ALBEDO_May)
      ,as.matrix(DIF_ALBEDO_Jun)
      ,as.matrix(DIF_ALBEDO_Jul)
      ,as.matrix(DIF_ALBEDO_Aug)
      ,as.matrix(DIF_ALBEDO_Sep)
      ,as.matrix(DIF_ALBEDO_Oct)
      ,as.matrix(DIF_ALBEDO_Nov)
      ,as.matrix(DIF_ALBEDO_Dec)
      ,along = 3)

DIF_RD_X = tibble(data.frame(apply(TMP*-1,MARGIN = c(1,2), FUN = mean)))


rm(TMP)

CLR_ALBEDO_DAT$RFA = -CLR_ALBEDO_DAT$Albedo * kernel
GRA_ALBEDO_DAT$RFA = -GRA_ALBEDO_DAT$Albedo * kernel
ENF_ALBEDO_DAT$RFA = -ENF_ALBEDO_DAT$Albedo * kernel

CLR_DAT = left_join(CLR_NEP_DAT, CLR_ALBEDO_DAT %>% group_by(obs_year) %>% summarise(Albedo = mean(Albedo), RFA = mean(RFA)))
GRA_DAT = left_join(GRA_NEP_DAT, GRA_ALBEDO_DAT %>% group_by(obs_year) %>% summarise(Albedo = mean(Albedo), RFA = mean(RFA)))
ENF_DAT = left_join(ENF_NEP_DAT, ENF_ALBEDO_DAT %>% group_by(obs_year) %>% summarise(Albedo = mean(Albedo), RFA = mean(RFA)))

####Combined########
ENF_DAT$year = c(2018,2015,2017,2016)
ENF_DAT = ENF_DAT %>% arrange(year)
ENF_DAT = ENF_DAT %>% mutate(RFC_sum = cumsum(RFC)) %>% mutate( RF_NET = RFC_sum + RFA)

GRA_DAT$year = c(2016,2018,2017)
GRA_DAT = GRA_DAT %>% arrange(year)
GRA_DAT = GRA_DAT %>% mutate(RFC_sum = cumsum(RFC)) %>% mutate( RF_NET = RFC_sum + RFA)

CLR_DAT = CLR_DAT %>% mutate(RFC_sum = cumsum(RFC)) %>% mutate( RF_NET = RFC_sum + RFA)

ALL_DAT = bind_rows(CLR_DAT,ENF_DAT,GRA_DAT) %>% dplyr::select(PFT,obs_year, NEE, Albedo, RFC_sum, RFA, RF_NET)

write_csv(ALL_DAT, '/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/ALLSBC_Albedo_Carbon_RF_observed.csv')

LIL_DAT = ALL_DAT %>% group_by(PFT) %>% summarise(NEE = mean(NEE), Albedo = mean(Albedo), RFA = mean(RFA), RFC = mean(RFC_sum), RF_NET = mean(RF_NET))
write_csv(LIL_DAT, '/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/ALLSBC_Albedo_Carbon_RF_observed_MEANS.csv')

DIF_RE_X_sum1 = DIF_RE_X1 %>% mutate_at(vars(X1:X5000), cumsum)
DIF_RE_X_sum2 = DIF_RE_X2 %>% mutate_at(vars(X1:X5000), cumsum)
DIF_RE_X_sum3 = DIF_RE_X3 %>% mutate_at(vars(X1:X5000), cumsum)
DIF_RE_X_sum4 = DIF_RE_X4 %>% mutate_at(vars(X1:X5000), cumsum)

DIF_NET1 = DIF_RE_X_sum1 + DIF_RD_X
DIF_NET2 = DIF_RE_X_sum2 + DIF_RD_X
DIF_NET3 = DIF_RE_X_sum3 + DIF_RD_X
DIF_NET4 = DIF_RE_X_sum4 + DIF_RD_X

plot(rowMeans(DIF_NET1), ylim = c(-30,30))
points(rowMeans(DIF_NET2), col = 'red')
points(rowMeans(DIF_NET3), col = 'blue')
points(rowMeans(DIF_NET4), col = 'green')

plot(rowMeans(DIF_RE_X1))
points(rowMeans(DIF_RE_X2), col = 'red')
points(rowMeans(DIF_RE_X3), col = 'blue')
points(rowMeans(DIF_RE_X4), col = 'green')

plot(rowMeans(DIF_RE_X_sum1), ylim = c(-30,30))
points(rowMeans(DIF_RE_X_sum2), col = 'red')
points(rowMeans(DIF_RE_X_sum3), col = 'blue')
points(rowMeans(DIF_RE_X_sum4), col = 'green')

###############
rowMeans(DIF_RE_X_sum1)
mean(unlist(DIF_RE_X_sum1[6,]))
sd(unlist(DIF_RE_X_sum1[6,]))

mean(unlist(DIF_RE_X_sum1[30,]))
sd(unlist(DIF_RE_X_sum1[30,]))

mean(unlist(DIF_RD_X[1,]))
sd(unlist(DIF_RD_X[1,]))

mean(unlist(DIF_RD_X[30,]))
sd(unlist(DIF_RD_X[30,]))

rowMeans(DIF_NET1)
mean(unlist(DIF_NET1[1,]))
sd(unlist(DIF_NET1[1,]))

mean(unlist(DIF_NET1[10,]))
sd(unlist(DIF_NET1[10,]))

mean(unlist(DIF_NET1[25,]))
sd(unlist(DIF_NET1[25,]))


###############
y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_NET1[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_NET1[i,], 0.025))
}
net_plot_data1 <- tibble(time = c(1:30),
                    y = rowMeans(DIF_NET1),
                    y_upper = y_upper,
                    y_lower = y_lower,
                    VAR = 'Net')

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_NET2[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_NET2[i,], 0.025))
}
net_plot_data2 <- tibble(time = c(1:30),
                        y = rowMeans(DIF_NET2),
                        y_upper = y_upper,
                        y_lower = y_lower,
                        VAR = 'Net')

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_NET3[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_NET3[i,], 0.025))
}
net_plot_data3 <- tibble(time = c(1:30),
                        y = rowMeans(DIF_NET3),
                        y_upper = y_upper,
                        y_lower = y_lower,
                        VAR = 'Net')

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_NET4[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_NET4[i,], 0.025))
}
net_plot_data4 <- tibble(time = c(1:30),
                        y = rowMeans(DIF_NET4),
                        y_upper = y_upper,
                        y_lower = y_lower,
                        VAR = 'Net')

#####
y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_RE_X_sum1[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_RE_X_sum1[i,], 0.025))
}
re_plot_data1 <- tibble(time = c(1:30),
                        y = rowMeans(DIF_RE_X_sum1),
                        y_upper = y_upper,
                        y_lower = y_lower,
                        VAR = 'NEP')

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_RE_X_sum2[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_RE_X_sum2[i,], 0.025))
}
re_plot_data2 <- tibble(time = c(1:30),
                       y = rowMeans(DIF_RE_X_sum2),
                       y_upper = y_upper,
                       y_lower = y_lower,
                       VAR = 'NEP')

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_RE_X_sum3[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_RE_X_sum3[i,], 0.025))
}
re_plot_data3 <- tibble(time = c(1:30),
                       y = rowMeans(DIF_RE_X_sum3),
                       y_upper = y_upper,
                       y_lower = y_lower,
                       VAR = 'NEP')

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_RE_X_sum4[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_RE_X_sum4[i,], 0.025))
}
re_plot_data4 <- tibble(time = c(1:30),
                       y = rowMeans(DIF_RE_X_sum4),
                       y_upper = y_upper,
                       y_lower = y_lower,
                       VAR = 'NEP')

y_upper <- rep(NA, 30)
y_lower <- rep(NA, 30)
for(i in 1:30){
  y_upper[i] <- as.numeric(quantile(DIF_RD_X[i,], 0.975))
  y_lower[i] <- as.numeric(quantile(DIF_RD_X[i,], 0.025))
}

###########
rd_plot_data <- tibble(time = c(1:30),
                       y = rowMeans(DIF_RD_X),
                       y_upper = y_upper,
                       y_lower = y_lower,
                       VAR = 'Albedo')
##########
cbbPalette <- c("#E69F00", "#009E73", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_data = bind_rows(net_plot_data1,re_plot_data1,rd_plot_data)
plot_data$VAR = factor(plot_data$VAR, labels = c('Albedo', 'Carbon', 'Net'))




f = ggplot(plot_data, aes(x = time, y = y, col = VAR, fill = VAR)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2, col = NA) +
  geom_line(lwd = 2) +
  theme_bw() +
  scale_color_manual(values = cbbPalette) + scale_fill_manual(values = cbbPalette) +
  ylab(expression(Radiative~Forcing~(W~m^-2))) +
  xlab('Years') +
  theme(legend.position="right",title = element_text(size=20)
        ,legend.title = element_blank()
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,panel.border = element_rect(colour = "black",fill = NA)
  );f

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/NEP_ALBEDO_RF_time_plots_uncertainty.pdf',width = 8, height = 5)
print(f)
dev.off()

###########

dif_net = c(colMeans(DIF_NET1))
dif_re = c(colMeans(DIF_RE_X_sum1))
dif_rd = c(colMeans(DIF_RD_X))

plot_data_all1 = gather(tibble(RE = dif_re, RD = dif_rd, NET = dif_net,COMP = 'NEE_NEE'),key = 'VAR', value = 'y', RE,RD,NET)

dif_net = c(colMeans(DIF_NET2))
dif_re = c(colMeans(DIF_RE_X_sum2))
dif_rd = c(colMeans(DIF_RD_X))

plot_data_all2 = gather(tibble(RE = dif_re, RD = dif_rd, NET = dif_net,COMP = 'NECB_NECB'),key = 'VAR', value = 'y', RE,RD,NET)

dif_net = c(colMeans(DIF_NET3))
dif_re = c(colMeans(DIF_RE_X_sum3))
dif_rd = c(colMeans(DIF_RD_X))

plot_data_all3 = gather(tibble(RE = dif_re, RD = dif_rd, NET = dif_net,COMP = 'NECB_NEE'),key = 'VAR', value = 'y', RE,RD,NET)

dif_net = c(colMeans(DIF_NET4))
dif_re = c(colMeans(DIF_RE_X_sum4))
dif_rd = c(colMeans(DIF_RD_X))

plot_data_all4 = gather(tibble(RE = dif_re, RD = dif_rd, NET = dif_net,COMP = 'NEE_NECB'),key = 'VAR', value = 'y', RE,RD,NET)
plot_data_all = bind_rows(plot_data_all1,plot_data_all2,plot_data_all3,plot_data_all4)

plot_data_all$VAR = factor(plot_data_all$VAR, labels = c('Net', 'Albedo', 'Carbon'))
plot_data_all$VAR = factor(plot_data_all$VAR, levels = c('Albedo', 'Carbon', 'Net'))

plot_data_small = plot_data_all %>% group_by(VAR,COMP) %>% summarise(Mean = quantile(y, probs = 0.5), x95 = quantile(y, probs = 0.975), x05 = quantile(y, probs = 0.025))
plot_data_small$COMP = factor(plot_data_small$COMP, levels = c("NEE_NEE","NECB_NEE","NEE_NECB",'NECB_NECB'))
plot_data_all$COMP = factor(plot_data_all$COMP, levels = c("NEE_NEE","NECB_NEE","NEE_NECB",'NECB_NECB'))
plot_data_all$COMP = factor(plot_data_all$COMP, labels = c("A","B","C",'D'))
plot_data_small$COMP = factor(plot_data_small$COMP, labels = c("A","B","C",'D'))

f2 = ggplot(plot_data_small, aes(x = VAR, y = Mean, pch = VAR)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~COMP, dir = 'h', nrow = 2) +
  geom_violin(data = plot_data_all, aes(y = y, fill = VAR), col = NA) +
  geom_pointrange(aes(ymin = x05,ymax = x95), lwd = 1) +
  theme_bw() +
  scale_color_manual(values = cbbPalette) + scale_fill_manual(values = cbbPalette) +
  xlab(NULL) +
  ylab(expression(Radiative~Forcing~(W~m^-2))) +
  theme(legend.position="top",title = element_text(size=20)
        ,legend.title = element_blank()
        ,text = element_text(size=20)
        ,strip.background = element_blank()
        ,strip.text = element_text(hjust = 0)
        ,panel.border = element_rect(colour = "black",fill = NA)
  );f2

plot_data_all 

pdf('/Users/Ben/Dropbox/Dissertation/Chapter 3/Plots/NET_ALBEDO_RF_point_plots_uncertainty.pdf',width = 7, height = 7)
print(f2)
dev.off()

write_csv(plot_data_all,'/Users/Ben/Dropbox/Dissertation/Chapter 3/Data/RF_NET_ALBEDO_CARBON_MEANS.csv')

yields = c(21,42,44,85,63,113)
gramCm2 = yields * 907185 / 4046.86 / 2
mean(gramCm2)
sd(gramCm2)

4 / 2 * 907185 / 4046.86


################
tmp = plot_data_all %>% filter(COMP == 'NEE_NEE' & VAR == 'Net')
mean(tmp$y)
sd(tmp$y)
round((sum(tmp$y < 0)/5000)*100, 1)

tmp = plot_data_all %>% filter(COMP == 'NECB_NEE' & VAR == 'Net')
mean(tmp$y)
sd(tmp$y)
round((sum(tmp$y < 0)/5000)*100, 1)

tmp = plot_data_all %>% filter(COMP == 'NEE_NECB' & VAR == 'Net')
mean(tmp$y)
sd(tmp$y)
round((sum(tmp$y > 0)/5000)*100, 1)

tmp = plot_data_all %>% filter(COMP == 'NECB_NECB' & VAR == 'Net')
mean(tmp$y)
sd(tmp$y)
round((sum(tmp$y > 0)/5000)*100, 1)
