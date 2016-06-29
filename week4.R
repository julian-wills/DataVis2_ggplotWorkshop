# Purpose: Introduction to exploratory data analysis with ggplot
# Author/Instructor: Julian Wills
# Date: 6/29/2016


# Setup: install packages, load functions, and load in data -----------------------------------
require(MASS) || {install.packages("MASS"); require(MASS)}
require(Hmisc) || {install.packages("Hmisc"); require(Hmisc)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}
require(ggplot2) || {install.packages("ggplot2"); require(ggplot2)}
require(scales) || {install.packages("scales"); require(scales)}
require(gridExtra) || {install.packages("gridExtra"); require(gridExtra)}
require(tidyr) || {install.packages("tidyr"); require(tidyr)}
require(plotly) || {install.packages("plotly"); require(plotly)}

# # Install and load packages
# toInstall <- c( "MASS", "dplyr", "ggplot2", "plotly", "scales", "gridExtra", "tidyr") 
# install.packages(toInstall, repos = "http://cran.r-project.org") #install packages from CRAN
# lapply(toInstall, library, character.only = TRUE) #load packages into environment 

#logistic regression model fit 
bin_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

#poisson regression model fit 
p_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "poisson"), ...)
}

# Set working directory and load in data
setwd("~/../GDrive/Misc/Summer Workshops/dataVis2/") #for those @ home, change this! rest of code will fail

dOrlando <- tbl_df(read.csv("orlando_tweets.csv"))

# Univariate: histograms and density plots -----------------
  
# Default histogram
ggplot(dOrlando) + 
  geom_histogram(aes(x=src_ideo))

# Change bins
ggplot(dOrlando) + 
  geom_histogram(aes(x=src_ideo),bins=120)

ggplot(dOrlando) + 
  geom_histogram(aes(x=src_ideo),bins=40)

ggplot(dOrlando) + 
  geom_histogram(aes(x=src_ideo),bins=40,fill=grey) #debug

ggplot(dOrlando) + 
  geom_histogram(aes(x=src_ideo),bins=40,fill="grey",color="black") 

ggplot(dOrlando) + 
  geom_histogram(aes(x=src_ideo),bins=40,fill="grey",color="black") +
  theme_light()

old <- theme_set(theme_light(base_size = 13)) #change default theme and font size

ggplot(dOrlando) + 
  geom_histogram(aes(x = src_ideo),bins=40,fill="grey",color="black") + 
  xlab("Ideology of Original Tweet Author") + 
  ylab("Frequency") + 
  ggtitle("Distribution of Orlando Shooting Tweet Authors' Ideology")

ggplot(dOrlando) + 
  geom_density(aes(x = src_ideo),fill="grey",color="black") + 
  xlab("Ideology of Original Tweet Author") + 
  ylab("Frequency") + #change
  ggtitle("Distribution of Orlando Shooting Tweet Authors' Ideology")

# Univariate: split by categorical variables -----------------

# How is the author's ideology related to whether the tweet is shared across the spectrum?
ggplot(dOrlando) +
  geom_density(aes(x=src_ideo,fill=crossTalk),color="black",alpha=.5) #tune alpha

# How is the retweeters' ideology related to the tweet author's political affiliation?
ggplot(dOrlando) +
  geom_density(aes(x=rt_ideo_mean,fill=party),color="black",alpha=.5) +
  scale_color_manual(values=c("Navy","Firebrick3")) #debug

# Is author ideology related to the use of moral-emotional language?
ggplot(dOrlando) +
  geom_density(aes(x=src_ideo,fill=MoralEmo_cnt),color="black",alpha=.4) #debug

# Turn moral emotional count into a factor, then retry code above^
dOrlando <- mutate(dOrlando,MoralEmo_factor=as.factor(MoralEmo_cnt))
  
# Create violin plots split by frequency of moral-emotional content
ggplot(dOrlando) +
  geom_violin(aes(x=MoralEmo_factor,y=src_ideo,fill=MoralEmo_factor),draw_quantiles = c(.25,.5,.75))

# Add observations to our violin plots
ggplot(dOrlando) +
  geom_violin(aes(x=MoralEmo_factor,y=src_ideo,fill=MoralEmo_factor),draw_quantiles = c(.25,.5,.75)) +
  geom_point(aes(x=MoralEmo_factor,y=src_ideo))

# Randomly jitter these observations to examine distribution
ggplot(dOrlando) +
  geom_violin(aes(x=MoralEmo_factor,y=src_ideo,fill=MoralEmo_factor),draw_quantiles = c(.25,.5,.75)) +
  geom_jitter(aes(x=MoralEmo_factor,y=src_ideo))

# Add mean and boostrapped CI (tip: notice warning message)
ggplot(dOrlando) +
  geom_violin(aes(x=MoralEmo_factor,y=src_ideo,fill=MoralEmo_factor),draw_quantiles = c(.25,.5,.75)) +
  stat_summary(aes(x=MoralEmo_factor,y=src_ideo),fun.data=mean_cl_boot)

# Avoid redunancy, set general aesthetics in main ggplot function
ggplot(dOrlando,aes(x=MoralEmo_factor,y=src_ideo)) +
  geom_violin(aes(fill=MoralEmo_factor),draw_quantiles = c(.25,.5,.75)) + #comment out later
  stat_summary(fun.data=mean_cl_boot) #try with color

# Try with error bars instead
ggplot(dOrlando,aes(x=MoralEmo_factor,y=src_ideo)) +
  stat_summary(fun.data=mean_cl_boot,geom="errorbar") +
  stat_summary(fun.y=mean,geom="point")

# Split by crossTalk variable
ggplot(dOrlando,aes(x=MoralEmo_factor,y=src_ideo)) +
  stat_summary(fun.data=mean_cl_boot,geom="errorbar") +
  stat_summary(fun.y=mean,geom="point") +
  facet_wrap(~crossTalk) #free axes

# Bivariate: scatterplots, surface plots, and smooths ---------------------

# Create scatterplot of original tweeter's ideology (X) by avg. retweeters' ideology (Y)
ggplot(dOrlando,aes(x=src_ideo,y=rt_ideo_mean)) +
  geom_point() #try alpha

# Let's zoom into that weird streak in the top left
ggplot(dOrlando,aes(x=src_ideo,y=rt_ideo_mean)) +
  geom_point() + #adjust alpha
  ylim(1,2) + xlim(-1.5,-.05) #navigate back to old plot

# Create heatmap to better visualize bivariate density
ggplot(dOrlando,aes(x=src_ideo,y=rt_ideo_mean)) +
  geom_bin2d()

# BONUS: 3D surface plot 
kd <- with(dOrlando, MASS::kde2d(src_ideo, rt_ideo_mean, n = 50)) #set kernel density
with(kd, plot_ly(x = x, y = y, z = z, type = "surface"))

# Add smooth fit -- run one line at a time
ggplot(dOrlando,aes(x=src_ideo,y=rt_ideo_mean)) +
  geom_smooth() + #you can also do stat_smooth here
  # geom_abline(slope=1,linetype="dashed") + #add prediction line
  # geom_vline(xintercept=0,linetype=4,color="purple",alpha=.3,size=1) + #add reference lines
  # geom_hline(yintercept=0,linetype=5,color="purple",alpha=.3,size=1) + #add reference lines
  # ylim(-1.5,3) + xlim(-1.5,3) + #rescale axes
  # facet_wrap(~crossTalk) #split by crossTalk

# Add linear model fit
ggplot(dOrlando,aes(x=src_ideo,y=rt_ideo_mean)) +
  geom_smooth(method="lm") +
  geom_abline(slope=1,linetype="dashed") + 
  facet_wrap(~crossTalk)

# Add polynomial model fit
ggplot(dOrlando,aes(x=src_ideo,y=rt_ideo_mean)) +
  geom_smooth(method="lm",formula=y~poly(x,2)) +
  geom_abline(slope=1,linetype="dashed") + 
  facet_wrap(~crossTalk)

# Does a tweet's virality depend on the ideology of the original author?
ggplot(dOrlando,aes(x=src_ideo,y=rt_ideo_mean)) +
  geom_smooth(method="lm",formula=y~poly(x,2)) + #change for kicks
  geom_abline(slope=1,linetype="dashed") + 
  facet_wrap(~crossTalk)

# Predicting virality -- comparing smooths
ggplot(dOrlando,aes(x=src_ideo,y=isViral)) +
  # geom_smooth(method="lm",alpha=.2) +
  # bin_smooth(alpha=.2,color="red") +
  # geom_smooth(alpha=.2,color="purple") +
  geom_smooth(aes(color=crossTalk))

# How does cross-talk affect the relationship between ideology and virality?
ggplot(dOrlando,aes(x=src_ideo,y=isViral)) +
  geom_smooth(aes(color=crossTalk)) + #remove SE
  # ylim(0,.05) + #careful! clips off data
  # coord_cartesian(ylim=c(0,1)) #restricts the axes range

# Interactive plots using 'plotly' ------------------------------------------

# What do tweets with cross-talk really look like? 

dOrlando_tweets <- tbl_df(read.csv("verified_tweets.csv"))

p <- dOrlando_tweets %>% 
  ggplot(aes(rt_ideo_mean,id)) +
  geom_vline(xintercept=0,linetype="dashed") +
  geom_point(aes(color=crossTalk,label=text)) +
  geom_point(aes(color=crossTalk,x=src_ideo,label=src),color="grey",shape=1) +
  geom_errorbarh(aes(xmin=ideo_min,xmax=ideo_max,color=crossTalk,label=text),height=.1) +
  geom_errorbarh(aes(xmin=rt_ideo_mean-rt_ideo_sd,xmax=rt_ideo_mean+rt_ideo_sd,color=crossTalk,label=text)) +
  xlab("\n Range of Retweeter Ideology") +
  theme_light() +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank()
  ) 

ggplotly(p)

# Multiple dependent variables  ------------------------------------------

# reshaping data with tidyr::gather 
dOrlando_MoralEmo <- gather(dOrlando,Dictionary,WordRatio,Emotion:MoralNeg)
dOrlando_MFT <- gather(dOrlando,Dictionary,WordRatio,Harm:Purity)
dOrlando_NegEmo <- gather(dOrlando,Dictionary,WordRatio,Guilt:Sad)

ggplot(dOrlando_MFT,aes(WordRatio,rt_count,color=Dictionary)) + 
  p_smooth(se=F) + 
  facet_wrap(~party,scales="free") +
  ylab("Retweet rt_count") + xlab("Ratio of Words in Tweet") + 
  ggtitle("Orlando Tweet Virality w.r.t. \n Moral Foundations & Political Party")

# compare to other linguistic predictors of retweet rt_count

old <- theme_set(theme_light(base_size = 12)) 

P <- list() #create empty list object called 'P'. We will use it to store ggplots shortly. 
p1 <- ggplot(dOrlando_MoralEmo,aes(WordRatio,rt_count,color=Dictionary)) +
  p_smooth(se=F) + coord_cartesian(ylim=c(2,13)) +
  ylab("Retweet rt_count") + xlab("Ratio of Words in Tweet") + ggtitle("Moral vs. Emotion")

p2 <-ggplot(dOrlando_MFT,aes(WordRatio,rt_count,color=Dictionary)) +
  p_smooth(se=F) + coord_cartesian(ylim=c(2,13)) +
  ylab("Retweet rt_count") + xlab("Ratio of Words in Tweet") + ggtitle("Moral Foundations")

p3 <-ggplot(dOrlando_NegEmo,aes(WordRatio,rt_count,color=Dictionary)) + 
  p_smooth(se=F) + coord_cartesian(ylim=c(2,13)) +
  ylab("Retweet rt_count") + xlab("Ratio of Words in Tweet") + ggtitle("Negative Emotions")

P <- c(list(p1),list(p2),list(p3)) #cast ggplot objects to lists and append to 'P'

do.call(grid.arrange, c(P, nrow = 1))

# Custom functions  ------------------------------------------

dOrlando_top1k <- slice(dOrlando,1:1000)

source("helpFunc.R") #loads in helper functions from file

scattergrid(dOrlando_top1k,"src_ideo")
scattergrid(dOrlando_top1k,"src_ideo",method="lm",se=T,alpha=.3)

# That's it! Hope you enjoyed  ------------------------------------------
