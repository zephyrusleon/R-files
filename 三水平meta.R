#three level meta-analysis example
#clean up
rm(list=ls())m

#install packages
install.packages("metafor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("dmetar")
install.packages("esc")

#load packages
library(metafor)
library(dplyr)
library(ggplot2)
library(dmetar)
library(esc)

#import data
#data结构为长数据 包括ES_number study 
#Exp_mean\ Exp_SD\ Exp_N\ Ctrl_mean\ Ctrl_SD\ Ctrl_N
library(readxl)
data <- read_excel("data.xlsx")

#calculate effect sizes
data1 <- escalc(measure = "SMD",
               m1i = Exp_mean, 
               sd1i = Exp_SD,
               n1i = Exp_N,
               m2i = Ctrl_mean,
               sd2i = Ctrl_SD,
               n2i = Ctrl_N,
               data = data)

write.csv(data, "data_es.csv")

#run three level meta-analysis
m.multi <- rma.mv(yi, #效应量
                  vi, #方差
                  random = ~ 1 | Study/ES_number, #随机效应结构 最上层random 中层study 最下层es_number
                  method = "REML",
                  dfs = "contain", #自由度计算方法
                  test = "t", 
                  data = data1)
m_multi

#examine variance#
#we need to teach R how to do this. First, copy-paste the code from the link to 
#the CONSOLE, and then hit enter.
#https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/mlm.variance.distribution.R
#Then proceed

#calculate i2 for each level
i2 <- var.comp(m_multi)
summary(i2)
i2

##################################
#check for outliers and influence#
##################################

#outliers
#adapting CI calculation and plotting from https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/detecting-outliers-influential-cases.html
# Calculate CI for all observed effect sizes
dat1$upperci <- dat1$yi + 1.96 * sqrt(dat1$vi)
dat1$lowerci <- dat1$yi - 1.96 * sqrt(dat1$vi)
# Create filter variable
dat1$outlier <- dat1$upperci < m_multi$ci.lb | dat1$lowerci > m_multi$ci.ub
# Count number of outliers:
sum(dat1$outlier)
dat1
# Make a basic plot, based on the data in dat1, and specify that the x-variable is the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = dat1, aes(x = yi, colour = outlier, fill = outlier)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_multi$b[1]) +
  # Apply a black and white theme
  theme_bw()

##Print file that lists outliers. This goes into working directory
write.csv(dat1, file = "outliers indicated.csv")


#influence - read about influence diagnostics here: https://wviechtb.github.io/metafor/reference/influence.rma.uni.html 
#cook's distance - results over 0.50 can indicate influential comparison
cooks <- cooks.distance(m_multi)
plot(cooks, type="o", pch=19, xlab="Observed Outcome", ylab="Cook's Distance")

#Calculate dfbetas - vaules absolute value over 1 can indicate influential comparison
dfbetas <-dfbetas(m_multi)
dfbetas 

#calculate hatvalues - values larger than 3*(p/k), where p is the number of model coefficients and k is the number of cases can indicate signifcant influence
hatvalues <- hatvalues(m_multi)
hatvalues

##Print file that lists influence info to examine them. This goes into working directory
influence<-cbind(cooks, dfbetas, hatvalues)
write.csv(influence, file = "influenceinfo.csv")


################################
#categorical moderator analysis#
################################
#note - metafor has two different tests of moderator depending on if intercept is or is not in the model. 

####Grade Level  
#calculate qb
mod.gradeq <- rma.mv(yi,
                     vi,
                     data = dat1,
                     random = ~ 1 | Study/ES_number, 
                     method = "REML",
                     test = "t",
                     dfs = "contain",
                     mods = ~ factor(grade_c))
summary(mod.gradeq)

#calculate ES
mod.grade <- rma.mv(yi,
                    vi,
                    data = dat1,
                    random = ~ 1 | Study/ES_number, 
                    method = "REML",
                    test = "t",
                    dfs = "contain",
                    mods = ~ factor(grade_c)-1)
summary(mod.grade)

#create table
#Only save table results for word 
mod.grade_table <-coef(summary(mod.grade))
#calculate participants in each group and add it to the table
mod.gradeSumParticipants <- dat1 %>%
  group_by(grade_c) %>%
  summarise(nexp = sum(Exp_n, na.rm = TRUE),
            nctrl = sum(Ctrl_n, na.rm = TRUE))
mod.gradeNumComp <- dat1 %>%
  count(grade_c)
mod.gradeNumComp <- rename(mod.gradeNumComp, kcomparisons = n)
mod.grade_table.final<- cbind(mod.gradeSumParticipants,mod.gradeNumComp[c(2)], mod.grade_table) 
write.csv(mod.grade_table.final, "mod.gradeResult.csv")

# Save QM Test and write it into a text file
Qmod.grade_collapsed_string <- paste(mod.gradeq[["QMdf"]], collapse = ", ")
Qmod.grade1 <- data.frame(CollapsedQMdf = Qmod.grade_collapsed_string)
Qmod.grade2 <- round(mod.gradeq$QM,2)
Qmod.grade3 <- round(mod.gradeq$QMp,3)

Qmod.gradeQ <- paste(
  "Qb(",Qmod.grade_collapsed_string,") =", 
  Qmod.grade2,
  ", p =", 
  Qmod.grade3,
  collapse = " "
)

cat(Qmod.gradeQ, "\n")
mod.gradeQtest <- data.frame(Text = Qmod.gradeQ)
write.table(mod.gradeQtest, file = "Qmod.gradeQ.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)


##############
#create plots#
##############

#note different data formatting requirements for these particular codes
#code slightly adapted from Fernández-Castilla et al. (2020) - https://doi.org/10.5964/meth.4013 

#load packages
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(metafor)
library(metaSEM)
library(ggrepel)

#load data
mydf<-read.csv("data for plots.csv")


study<-mydf$study
out<-mydf$outcome
ES<-mydf$effect_size
var<-mydf$variance
se<-mydf$standard_error
author<-mydf$author

forest_plot_3<-function(author, study, ES, out, var, se, size_lines){
  size_lines=size_lines
  dataset<-data.frame(study,author, ES, out, var, se)
  row = 1
  nrow=max(dataset$study)
  studyn=max(dataset$study)
  studyinfo = data.frame(Study = numeric(nrow),
                         author = numeric(nrow),
                         id = numeric(nrow),
                         ES= numeric(nrow),
                         SE= numeric(nrow),
                         Var=numeric(nrow),
                         cilb= numeric(nrow),
                         ciub= numeric(nrow),
                         k= numeric(nrow),
                         out=numeric(nrow),
                         median_Var=numeric(nrow),
                         S_cilb=numeric(nrow),
                         S_ciub=numeric(nrow),
                         Weight=numeric(nrow))
  Study1 =c()
  Study2 =c()
  dataset$author<-as.character(dataset$author)
  meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
  estimate<-round(meta_abu$coefficients$Estimate[1], digits=2)
  tau<-meta_abu$coefficients$Estimate[3]
  out<-meta_abu$coefficients$Estimate[2]
  
  
  
  for (i in 1:max(dataset$study)){
    data<-subset(dataset, study==i)
    uni=nrow(data)
    
    if (uni==1) {
      studyinfo$ES[row]<-data$ES
      studyinfo$SE[row]<-data$se
      studyinfo$cilb[row]<-(data$ES-(data$se*1.96))
      studyinfo$ciub[row]<-(data$ES+(data$se*1.96))
      studyinfo$S_cilb[row]<-(data$ES-(data$se*1.96))
      studyinfo$S_ciub[row]<-(data$ES+(data$se*1.96))
      studyinfo$Weight[row]<-1/ (data$se^2)
    }
    else {
      a<-rma(y=data$ES, vi=data$var, data=data, method="REML")
      
      diagonal<-1/(data$var+out)
      D<-diag(diagonal)
      obs<-nrow(data)
      I<-matrix(c(rep(1,(obs^2))),nrow=obs)
      M<-D%*%I%*%D
      inv_sumVar<-sum(1/(data$var+out))
      O<-1/((1/tau)+inv_sumVar)
      V<-D-(O*M)
      T<-as.matrix(data$ES)
      X<-matrix(c(rep(1,obs)), ncol=1)
      var_effect<-solve(t(X)%*%V%*%X)
      
      studyinfo$ES[row]<-a$b
      studyinfo$SE[row]<-a$se
      studyinfo$cilb[row]<-a$ci.lb
      studyinfo$ciub[row]<-a$ci.ub
      studyinfo$S_cilb[row]<-a$b - 1.96*median(data$se)
      studyinfo$S_ciub[row]<-a$b + 1.96*median(data$se)
      studyinfo$Weight[row]<-1/ var_effect
    }
    
    studyinfo$Study[row]<-c(Study1,paste("Study",i))
    studyinfo$id[row]<-i
    studyinfo$k[row]<-nrow(data)
    studyinfo$author[row]<-data$author[1]
    studyinfo$out[row] <- c(Study2, paste("J =",studyinfo$k[i]))
    studyinfo$median_Var[row]<-median(data$var)
    studyinfo$Var<-(studyinfo$SE)^2
    row = row + 1      
  }
  
  
  minimum<-min(studyinfo$S_cilb)
  maximum<-max(studyinfo$S_ciub)
  lim_minimum<-minimum-0.10
  lim_maximum<-maximum+0.25
  r_lim_minimum<-round(lim_minimum, digits=0)
  r_lim_maximum<-round(lim_maximum, digits=0)
  abs_r_lim_minimum<-abs(r_lim_minimum)
  abs_r_lim_maximum<-abs(r_lim_maximum)
  dec_min<-round(abs((lim_minimum-r_lim_minimum)*100), digits=0)
  dec_max<-round(abs((lim_maximum-r_lim_maximum)*100), digits=0)
  
  if (dec_min < 25) {
    c=25/100
  } else if (dec_min>25 & dec_min<50) {
    c=50/100
  } else if (dec_min>50 & dec_min<75) {
    c=75/100
  } else {
    c=abs_r_lim_minimum+1
  }
  
  if (dec_max < 25) {
    d=25/100
  } else if (dec_max>25 & dec_max<50) {
    d=50/100
  } else if (dec_max>50 & dec_max<75) {
    d=75/100
  } else {
    d=abs_r_lim_maximum+1
  }
  
  lim_minimum<-r_lim_minimum-c
  lim_maximum<-r_lim_maximum+d
  
  Axis_ES <- seq(lim_minimum, lim_maximum, by=0.50)
  Axis_ES<-Axis_ES[order(Axis_ES)]
  empty <- data.frame(id=c(NA,NA), ES=c(NA, NA), cilb=c(NA, NA),ciub=c(NA,NA),
                      k=c(NA,NA), Study=c(NA,NA), SE=c(NA, NA), 
                      out=c(NA,NA),median_Var=c(NA,NA), S_cilb=c(NA,NA), S_ciub=c(NA,NA),
                      Var=c(NA, NA), Weight=c(NA,NA), author=c("","Summary"))
  
  studyinfo <- rbind(studyinfo, empty)
  studyinfo$Study=factor(studyinfo$Study ,levels=unique(studyinfo$Study))
  studyinfo$author=factor(studyinfo$author ,levels=unique(studyinfo$author))
  r_diam<-studyn-2
  sum.y <- c(1, 0.7, 1, 1.3, rep(NA,r_diam )) 
  sum.x <- c(meta_abu$coefficients$lbound[1], meta_abu$coefficients$Estimate[1], meta_abu$coefficients$ubound[1], meta_abu$coefficients$Estimate[1], rep(NA, r_diam))
  studyinfo<-data.frame(studyinfo, sum.x, sum.y )
  studyinfo<-studyinfo[, c(15,16,3,4,5,6,7,8,9,10,11,12,13,14,1,2)]
  
  forest<-ggplot()+ geom_point(data=studyinfo, aes(y=factor(author), x = ES, xmin =cilb, xmax = ciub, size=Weight), shape=15) +
    #scale_size_area()+
    geom_errorbarh(data=studyinfo, aes(y=factor(author), x = ES, xmin =cilb, xmax = ciub), size=1, height=.2)+
    scale_x_continuous(limits=c(lim_minimum,lim_maximum),breaks=Axis_ES)+ 
    scale_y_discrete(limits=rev(levels(studyinfo$author)))+
    geom_vline(xintercept=0)+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none",
          panel.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.ticks.y =element_blank(),
          axis.title.x=element_text(size=10, color ="black",family="sans"),
          axis.title.y=element_blank(),
          axis.text.y = element_text(family="sans",size=10, color = "black",hjust=0, angle=0),
          axis.text.x = element_text(size=10, color="black",family="sans"), 
          axis.line.y =element_blank())+
    labs(x = paste("Pooled Effect Size", estimate), hjust=-2)+
    geom_polygon(aes(x=sum.x, y=sum.y))+
    geom_vline(xintercept=estimate, colour="black",linetype=4)+
    geom_text(aes(x=lim_maximum, y=factor(studyinfo$author),label = studyinfo$out), size=3)
  
  if (size_lines==1){
    
    forest<-forest+geom_point(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub), shape=15)+
      geom_errorbarh(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub, size=k), width=.8,  height=.4, alpha=.2) #Cambiar .3 por .8
  } else{
    
    forest<-forest+geom_point(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub), shape=15)+
      geom_errorbarh(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub), width=.8,  height=.4, alpha=.5) #Cambiar .3 por .8
    
  }
  print(forest)
  
}


##If we want the size of the grey lines to be proportional to the number of outcomes within each study, then size_lines=1
##otherwise, specify that size_lines=0

forest_plot_3(author, study, ES, out, var, se, size_lines=1)




#catepillar plot of all comparisons 
Caterpillar<-function(study, ES, out, var, se){ 
  dataset<-data.frame(study, ES, out, var, se)
  dataset$cilb<-dataset$ES-(1.96*dataset$se)
  dataset$ciub<-dataset$ES+(1.96*dataset$se)
  meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
  dataset<-dataset[order(dataset$ES),]
  dataset$id<-c(rep(1:length(dataset$se)))
  P_combined<-nrow(dataset)+10
  combined_ES<-data.frame(ES=meta_abu$coefficients$Estimate[1],
                          cilb=meta_abu$coefficients$lbound[1], ciub=meta_abu$coefficients$ubound[1],
                          id=P_combined)
  
  
  minimum<-min(dataset$cilb)
  maximum<-max(dataset$ciub)
  lim_minimum<-minimum-0.10
  lim_maximum<-maximum+0.10
  r_lim_minimum<-round(lim_minimum, digits=0)
  r_lim_maximum<-round(lim_maximum, digits=0)
  abs_r_lim_minimum<-abs(r_lim_minimum)
  abs_r_lim_maximum<-abs(r_lim_maximum)
  dec_min<-round(abs((lim_minimum-r_lim_minimum)*100), digits=0)
  dec_max<-round(abs((lim_maximum-r_lim_maximum)*100), digits=0)
  
  if (dec_min < 25) {
    c=25/100
  } else if (dec_min>25 & dec_min<50) {
    c=50/100
  } else if (dec_min>50 & dec_min<75) {
    c=75/100
  } else {
    c=abs_r_lim_minimum+1
  }
  
  if (dec_max < 25) {
    d=25/100
  } else if (dec_max>25 & dec_max<50) {
    d=50/100
  } else if (dec_max>50 & dec_max<75) {
    d=75/100
  } else {
    d=abs_r_lim_maximum+1
  }
  
  lim_minimum<-r_lim_minimum-c
  lim_maximum<-r_lim_maximum+d
  
  Axis_ES <- seq(lim_minimum, lim_maximum, by=2)
  #Axis_ES<- c(Axis_ES,0)
  Axis_ES<-Axis_ES[order(Axis_ES)]
  
  p <- ggplot()+
    geom_point(data=dataset, aes(y=id, x=ES),colour = "black")+
    geom_errorbarh(data=dataset, aes(y=id, x=ES, xmin = cilb, xmax = ciub),size=1,  height=.2)+
    scale_x_continuous(limits=c(lim_minimum,lim_maximum),breaks=Axis_ES)+ 
    geom_vline(xintercept=0,size=1.2, alpha=0.7,colour="#EF3B2C", linetype="twodash")
  p<-p+
    geom_point(data=combined_ES, aes(y=id, x=ES), colour = "red", size=2)+
    geom_errorbarh(data=combined_ES, aes(y=id, x=ES, xmin = cilb, xmax = ciub), colour="red", size=1,  height=.2)+
    coord_flip()+
    theme( axis.line=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           legend.position="none",panel.background = element_blank(), axis.line.x = element_blank(), axis.line.y = element_line(colour = "black"),
           axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y = element_text(size=12, color="black"), axis.text.x = element_blank(), axis.ticks = element_blank())+
    xlab("Effect sizes")
  
  print(p)
  
}
Caterpillar(study, ES, out, var, se)


#catepillar by study

caterpillar_studies<-function(study, ES, out, var, se){
  dataset<-data.frame(study, ES, out, var, se)
  meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
  row = 1
  nrow=max(dataset$study)
  studyn=max(dataset$study)
  studyinfo = data.frame(Study = numeric(nrow),
                         ES= numeric(nrow),
                         SE= numeric(nrow),
                         cilb= numeric(nrow),
                         ciub= numeric(nrow),
                         S_cilb=numeric(nrow),
                         S_ciub=numeric(nrow))
  Study1 =c()
  Study2 =c()
  
  for (i in 1:max(dataset$study)){
    data<-subset(dataset, study==i)
    uni=nrow(data)
    if (uni==1) {
      studyinfo$ES[row]<-data$ES
      studyinfo$SE[row]<-data$se
      studyinfo$cilb[row]<-(data$ES-(data$se*1.96))
      studyinfo$ciub[row]<-(data$ES+(data$se*1.96))
      studyinfo$S_cilb[row]<-(data$ES-(data$se*1.96))
      studyinfo$S_ciub[row]<-(data$ES+(data$se*1.96)) 
    }
    
    else {
      a<-rma(y=data$ES, vi=data$var, data=data, method="REML")
      studyinfo$ES[row]<-a$b
      studyinfo$SE[row]<-a$se
      studyinfo$cilb[row]<-a$ci.lb
      studyinfo$ciub[row]<-a$ci.ub
      studyinfo$S_cilb[row]<-a$b - 1.96*median(data$se)
      studyinfo$S_ciub[row]<-a$b + 1.96*median(data$se)
    }
    studyinfo$Study[row]<-c(Study1,paste("Study",i))
    row = row + 1      
    
  }
  
  studyinfo<- studyinfo[order(studyinfo$ES),]
  studyinfo$id<-c(rep(1:length(studyinfo$ES)))
  
  P_combined<-nrow(studyinfo)+2
  combined_ES<-data.frame(ES=meta_abu$coefficients$Estimate[1],
                          cilb=meta_abu$coefficients$lbound[1], ciub=meta_abu$coefficients$ubound[1],
                          id=P_combined)
  
  
  minimum<-min(studyinfo$S_cilb)
  maximum<-max(studyinfo$S_ciub)
  lim_minimum<-minimum-0.10
  lim_maximum<-maximum+0.10
  r_lim_minimum<-round(lim_minimum, digits=0)
  r_lim_maximum<-round(lim_maximum, digits=0)
  abs_r_lim_minimum<-abs(r_lim_minimum)
  abs_r_lim_maximum<-abs(r_lim_maximum)
  dec_min<-round(abs((lim_minimum-r_lim_minimum)*100), digits=0)
  dec_max<-round(abs((lim_maximum-r_lim_maximum)*100), digits=0)
  
  if (dec_min < 25) {
    c=25/100
  } else if (dec_min>25 & dec_min<50) {
    c=50/100
  } else if (dec_min>50 & dec_min<75) {
    c=75/100
  } else {
    c=abs_r_lim_minimum+1
  }
  
  if (dec_max < 25) {
    d=25/100
  } else if (dec_max>25 & dec_max<50) {
    d=50/100
  } else if (dec_max>50 & dec_max<75) {
    d=75/100
  } else {
    d=abs_r_lim_maximum+1
  }
  
  lim_minimum<-r_lim_minimum-c
  lim_maximum<-r_lim_maximum+d
  
  Axis_ES <- seq(lim_minimum, lim_maximum, by=0.50)
  Axis_ES<- c(Axis_ES,0)
  Axis_ES<-Axis_ES[order(Axis_ES)]
  
  
  r <- ggplot()+
    geom_point(data=studyinfo, aes(y=id, x=ES),colour = "black")+
    geom_errorbarh(data=studyinfo, aes(y=id, x=ES, xmin = cilb, xmax = ciub),  size=1, height=.2)+
    scale_x_continuous(limits=c(lim_minimum,lim_maximum),breaks=Axis_ES)+ 
    geom_vline(xintercept=0,size=1.2, alpha=0.7,colour="#EF3B2C", linetype="twodash")
  
  r<-r + geom_point(data=studyinfo, aes(y=id, x=ES),colour = "black")+
    geom_errorbarh(data=studyinfo, aes(y=id, x=ES, xmin = S_cilb, xmax =  S_ciub),width=.2,  height=.2, alpha=.5)+
    geom_point(data=combined_ES, aes(y=id, x=ES),colour = "red", size=2)+
    geom_errorbarh(data=combined_ES, aes(y=id, x=ES, xmin = cilb, xmax =ciub),height=.2, colour = "red")+
    coord_flip()+
    theme(axis.line=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position="none",panel.background = element_blank(), axis.line.x = element_blank(), axis.line.y = element_line(colour = "black"),
          axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y = element_text(size=12, color="black"), axis.text.x = element_blank(), axis.ticks = element_blank())+
    xlab("Meta-analytic study means")
  
  print(r)
}

caterpillar_studies(study, ES, out, var, se)



###########################
#Publication Bias##########
###########################

#funnel plot of all effects
three_funnel<-function(study, ES, out, var, se){
  
  dataset<-data.frame(study, ES, out, var, se)
  contour.points=200
  meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
  estimate<-meta_abu$coefficients$Estimate[1]
  tau<-meta_abu$coefficients$Estimate[3]
  out<-meta_abu$coefficients$Estimate[2]
  
  maxse<-max(dataset$se)
  ylim<-c(0, maxse)
  csize <- seq(ylim[1], ylim[2], length.out = contour.points)
  csize[csize <= 0] <- 1e-07 * min(dataset$se)
  csize
  
  CI_Lim<-matrix(0, nrow=length(csize), ncol=2)
  colnames(CI_Lim)<-c("lb_total", "ub_total")
  
  for (i in 1:length(csize)){
    CI_Lim[i,1]<-estimate-1.96*sqrt((csize[i]^2)+tau+out) #add 1.96*
    CI_Lim[i,2]<-estimate+1.96*sqrt((csize[i]^2)+tau+out)
  }
  CI_Lim<-as.data.frame(CI_Lim)
  
  dataset$study<-as.character(dataset$study)
  dataset$study <- factor(dataset$study)
  geom.text.size = 3
  max_SE<-max(dataset$se)
  le<-length(CI_Lim[,1])
  
  if ((CI_Lim[le,1])< 0) {
    minimum=min(CI_Lim[,1])
  } else {
    minimum=max(CI_Lim[,1])
  } 
  
  if ((CI_Lim[le,2]) > 0) {
    maximum=max(CI_Lim[,2])
  } else {
    maximum=min(CI_Lim[,2])
  } 
  
  
  lim_minimum<-floor(minimum-0.10)
  lim_maximum<-ceiling(maximum+0.10)
  Axis_ES <- seq(lim_minimum, lim_maximum, by=1)
  
  d <- ggplot(data=dataset, aes(x = se, y = ES, ylim(0,max_SE)))+
    geom_point()+
    xlab('Standard Error')+ 
    ylab('Effect size: g')+
    geom_hline(yintercept= estimate)+
    geom_hline(yintercept= 0, color='grey')+
    scale_x_reverse()+
    scale_y_continuous(breaks=Axis_ES, limits =c(lim_minimum,lim_maximum))+
    coord_flip()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title = element_text(size=14),
          axis.text = element_text(size=12, color="black"),
          text=element_text())
  
  d <- d + geom_line(data=CI_Lim, aes(y=lb_total, x=csize), colour="black")+
    geom_line(data=CI_Lim, aes(y=ub_total, x=csize), colour="black")
  print(d)
}

three_funnel(study, ES, out, var, se)



#funnel plot by study
three_funnel_study<-function(study, ES, out, var, se, size_dots, numbers){
  numbers=numbers
  size_dots=size_dots
  dataset<-data.frame(study, ES, out, var, se)
  contour.points=200
  
  meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
  estimate<-meta_abu$coefficients$Estimate[1]
  tau<-meta_abu$coefficients$Estimate[3]
  out<-meta_abu$coefficients$Estimate[2]
  
  row = 1
  nrow=max(dataset$study)
  studyinfo = data.frame(Study = numeric(nrow),
                         id = numeric(nrow),
                         ES= numeric(nrow),
                         SE= numeric(nrow),
                         k= numeric(nrow),
                         median_SE=numeric(nrow))
  Study1 =c()
  geom.text.size = 3
  
  for (i in 1:max(dataset$study)){
    data<-subset(dataset, study==i)
    uni=nrow(data)
    
    if (uni==1) {
      studyinfo$ES[row]<-data$ES
      studyinfo$SE[row]<-data$se
      studyinfo$median_SE[row]<-data$se
    }
    
    else {
      
      a<-rma(y=data$ES, vi=data$var, data=data, method="REML")
      studyinfo$ES[row]<-a$b
      studyinfo$SE[row]<-a$se
      studyinfo$median_SE[row]<-median(data$se)
    }
    
    studyinfo$id[row]<-i
    studyinfo$k[row]<-nrow(data)
    studyinfo$Study[row]<-c(Study1,paste("Study",i))
    row = row + 1      
  }
  
  median_k<- median(studyinfo$k)
  maxse<-max(studyinfo$SE)
  ylim<-c(0, maxse)
  csize <- seq(ylim[1], ylim[2], length.out = contour.points)
  csize[csize <= 0] <- 1e-07 * min(studyinfo$SE)
  CI_Lim<-matrix(0, nrow=length(csize), ncol=2)
  colnames(CI_Lim)<-c("lb_total", "ub_total")
  
  for (i in 1:length(csize)){
    CI_Lim[i,1]<-estimate-1.96*sqrt((((csize[i]^2)+out)/median_k)+tau)#add 1.96*
    CI_Lim[i,2]<-estimate+1.96*sqrt((((csize[i]^2)+out)/median_k)+tau)
  }
  CI_Lim<-as.data.frame(CI_Lim)
  
  le<-length(CI_Lim[,1])
  
  
  
  if ((CI_Lim[le,1])< 0) {
    minimum=min(CI_Lim[,1])
  } else {
    minimum=max(CI_Lim[,1])
  } 
  
  if ((CI_Lim[le,2]) > 0) {
    maximum=max(CI_Lim[,2])
  } else {
    maximum=min(CI_Lim[,2])
  } 
  
  
  lim_minimum<-floor(minimum-0.10)
  lim_maximum<-ceiling(maximum+0.10)
  Axis_ES <- seq(lim_minimum, lim_maximum, by=1)
  
  if (size_dots==1){
    if(numbers==1){
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point(data=studyinfo, aes(size=k)) +
        geom_text_repel(aes(label=factor(studyinfo$k)), hjust=0, vjust=-0.40, size=geom.text.size, direction="x", segment.size  = 0.2, segment.color = "grey50")+
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    } else {
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point(data=studyinfo, aes(size=k)) +
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    }
    
  } else {
    
    if (numbers==1){
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point() +
        geom_text_repel(aes(label=factor(studyinfo$k)), hjust=0, vjust=-0.40, size=geom.text.size, direction="x", segment.size  = 0.2, segment.color = "grey50")+
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    }else{
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point() +
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    }
  }
  
  e <- e + geom_line(data=CI_Lim, aes(y=lb_total, x=csize), colour="black")+
    geom_line(data=CI_Lim, aes(y=ub_total, x=csize), colour="black")
  print(e)
  
} 


# if size_dots=1, then the size of the dots representing the study-effects will be proportional to the number of effect
#sizes included in that study. If size_dots=0, then all dots will have the same size.
# if numbers=1, then a number will appear next to the dot representing the study-effect indicating the number of effect
#sizes include in that study. if numbers=0, then no number will appear.

three_funnel_study(study,ES, out,var,se, size_dots=1, numbers=1)