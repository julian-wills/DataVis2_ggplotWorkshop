# Purpose: Introduction to data manipulation with dplyr and tidyr
# Author/Instructor: Julian Wills
# Date: 7/11/2016


# Setup: install packages, load functions, and load in data -----------------------------------
require(MASS) || {install.packages("MASS"); require(MASS)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}
require(ggplot2) || {install.packages("ggplot2"); require(ggplot2)}
require(tidyr) || {install.packages("tidyr"); require(tidyr)}
require(geepack) || {install.packages("geepack"); require(geepack)}
require(stringr) || {install.packages("stringr"); require(stringr)}
require(ggrepel) || {install.packages("ggrepel"); require(ggrepel)}
require(broom) || {install.packages("broom"); require(broom)}

source("helpFunc.R") #Loads in helper functions
theme_set(theme_light(base_size = 12)) #Changes ggplot default to light theme

# First, we're going to set our working directory to where our data lives. 
# Make sure to to change this on your personal computer. 
setwd("/Users/Julian/GDrive/Misc/Summer Workshops/dataManip/")

# Next, we're going to import the raw E-Prime data that's been saved as a .csv.
# We're also going to use the 'tbl_df' function from the 'dplyr' package which provides
#  a convenient alternative for displaying dataframes in the R console. 
dLong <- tbl_df(read.csv("PGGfMRI_rawEprime.csv"))

# Selecting, rearranging, and renaming variables ---------------------------------------------

# The dplyr package provides many helpful functions for manipulating data. 
# Virtually all of these functions are intuitively named with verbs (e.g., select, filter). 

# There are some variables here that we won't be using in this tutorial such as:
#  ButtonScheme, Prior, FlagColor, Offer.OnsetTime... ITI2.OnsetTime.

# Use 'select' to narrow down columns we care about. Store this in a new dataframe.  
dLong2 <- select(dLong,SubjID,SumWrong,Norm,BlockID,TrialID,Offer,Offer.RT,NumGiv)

# The 'Norm' variable tells us whether subjects were playing with the prosocial (1) or 
#  antisocial (0) school. Because this changes on each block (as indicated by 'BlockID'), 
#  it might be more intuitive if our variables were arranged in sequence from broad to specific
#  (i.e., Subject --> Block --> Trial). Let's move Norm to the right of BlockID so that
#  we don't confuse it with subject-level variables (e.g., SumWrong).

# Use 'select' to specify the order of variable in data frame. 
dLong2 <- select(dLong,SubjID,SumWrong,BlockID,Norm,TrialID,Offer,Offer.RT,NumGiv)

# Use ':' instead of ',' to include sequence of variables. 
dLong2 <- select(dLong,SubjID,SumWrong,BlockID,Norm,TrialID:NumGiv)

# Use '-' to drop variables. This is an alternative way of yielding the same output. 
dLong2 <- select(dLong,-ButtonScheme:-Prior,-FlagColor,-Offer.OnsetTime:-ITI2.OnsetTime)

# Use '=' to rename variables. Perhaps I want to rename 'Offer.RT' to 'RT' to preserve space.
dLong2 <- select(dLong,SubjID,SumWrong,BlockID,Norm,TrialID,Cooperate=Offer,RT=Offer.RT,NumGiv)

# Use 'rename' to rename variables without dropping the rest. Here I rename 'Cooperate'
#  back to 'Offer' since it has fewer letters and fits easier in the console. 
dLong2 <- rename(dLong2,Offer=Cooperate)

# Transforming variables  ---------------------------------------------

# Use 'mutate' to transform existing variables or compute new ones
dLong2 <- mutate(dLong2,LogRT=log(RT)) #Log-transform reaction time
dLong2 <- mutate(dLong2,Missed=ifelse(RT==0,1,0)) #Flag missed trials w/ '1', otherwise '0'
dLong2 <- mutate(dLong2,Offer_f=ifelse(Offer==8,"Cooperate","Free-Ride")) #Create string/factor

# Alternatively, this can be done in one fell swoop:
dLong2 <- mutate(dLong2,LogRT=log(RT),
                 Missed=ifelse(RT==0,1,0),
                 Offer_f=ifelse(Offer==8,"Cooperate","Free-Ride")) 

# Use 'distinct' to preserve rows with unique values of specific variables
distinct(dLong2,BlockID) #This returns 4 rows corresponding to four blocks
dLong_check <-  distinct(dLong2,BlockID,TrialID) 
View(dLong_check) 

# Notice that 'TrialID' resets for each block (1-25). What if I want a way of indexing trials
#  regardless of the specific block (e.g., 1-100)? Let's compute a new variable. 
dLong2 <- mutate(dLong2,TrialSum = TrialID + (25 *(BlockID-1)))
dLong_check <-  distinct(dLong2,BlockID,TrialSum) 
View(dLong_check) 

# Now we have a better way of examining reaction time over time..
ggplot(dLong2,aes(TrialID,LogRT,color=Offer_f)) + geom_smooth()
ggplot(dLong2,aes(TrialSum,LogRT,color=Offer_f)) + geom_smooth()

# Summarizing and grouping data  ---------------------------------------------

# How many trials were missed overall? Use 'summarize' to compute statistics for variables.
summarize(dLong2,Missed_grandSum=sum(Missed))

# What percentage of trials were missed overall?
summarize(dLong2,
          Missed_grandSum=sum(Missed),
          Missed_grandPerc = (Missed_grandSum*100) /n() ) #<-- NOTE: 'n()' is extremely useful

# What percentage of trials were missed by each subject?
dLong_grp <- group_by(dLong2,SubjID) #Create a grouped dataframe with Subject ID as variable
summarize(dLong_grp,Missed=sum(Missed)) #Now aggregates within, instead of across, subjects

# Tip: Computed variables will be referenced by subsequent variables
dSum_missed <- summarize(dLong_grp,
                         Missed=sum(Missed),
                         Recorded=100-Missed, #NOTE: Don't have to use sum(Missed) again.
                         Coop=sum(Offer/8)) #Number of trials cooperated

# How do I get the mean and SE of my variables of interest?
summarize(dLong2,
          Offer_mean=mean(Offer),Offer_sd=sd(Offer),
          LogRT_mean=mean(LogRT),LogRT_sd=sd(LogRT))

# Uh oh! It's because we computed LogRT on missed trials. Since log(0) = -Infinity 
#  this is throwing off our summary. Let's change this by converting all missed trials to NA
dLong2 <- mutate(dLong2,LogRT=ifelse(Missed==1,NA,LogRT)) #Change '-Inf' to 'NA'

# Now we need to include an additional argument to the mean() function to ignore NAs. 
summarize(dLong2,
          Offer_mean=mean(Offer),Offer_sd=sd(Offer),
          LogRT_mean=mean(LogRT,na.rm=T),LogRT_sd=sd(LogRT,na.rm=T))

# Sometimes it's a pain to write all of this out for each variable. 
summarize_each(dLong2, funs(mean,sd), Offer,RT,NumGiv)

# We can do a similar trick to apply transformations to multiple variables. 
# For instance, let's say we want to mean center our predictors before running a model. 
dLong2_C <- mutate_each(dLong2, funs(.-mean(.)), Offer,NumGiv,TrialSum)

# Notice how the intercept and lower-order terms change. 
summary(lm(LogRT ~ Offer+NumGiv*TrialSum,data=dLong2)) #Original data
summary(lm(LogRT ~ Offer+NumGiv*TrialSum,data=dLong2_C)) #Centered data

# Or using custom function that uses this same logic
summary(lm(LogRT ~ Offer+NumGiv*TrialSum,
           data=demean(dLong2,"LogRT"))) 


# Filtering observations --------------------------------------------------

# Next question: How many subjects missed at least 5% of all trials?

# Use 'filter' to preserve subset of dataframe
dSum_badSubs <- filter(dSum_missed,Missed>=5) # Look at 'Row x Col' header of dataframe
nrow(dSum_badSubs) # Non-interactive way of obtaining number

# It is often very useful to filter on grouped data
dLong_goodSubs <- filter(dLong_grp,sum(Missed)<5)
filter(dLong2,sum(Missed)<5) # Notice what happens when we don't use grouped data 

# Which subjects missed the most trials?
arrange(dSum_missed,Recorded) # Sort by Recorded trials (Fewest to Most)
dSum_sort <- arrange(dSum_missed,desc(Missed)) # Same as above since using 'desc' (i.e., descending)

library(ggrepel)
ggplot(dSum_sort,aes(x=SubjID,y=Missed)) +
  geom_point() + #Not quite what we want
  geom_label_repel(aes(label=SubjID)) + #Labels all subjects (comment out or remove this)
  # geom_label_repel(aes(label=SubjID),data=dSum_badSubs) + #Labels the 'bad' subjects only
  list() #This is a just a 'hack' so that ggplot still runs when there's a trailing '+'



# Piping: putting it all together -----------------------------------------
tbl_df(read.csv("PGGfMRI_rawEprime.csv")) %>% 
  select(SubjID,Offer.RT) %>% 
  mutate(Missed=ifelse(Offer.RT==0,1,0)) %>% 
  group_by(SubjID) %>% 
  summarize(Missed=sum(Missed),Recorded=100-Missed) %>% 
  ggplot(aes(x=SubjID,y=Missed)) + geom_point() +
  geom_label_repel(aes(label=SubjID),data=dSum_badSubs) 

# Practically all of Hadley functions take a dataframe as the first argument.
# When using the pipe, there is an implicit '.' at the beginning of the subsequent function. 
# This acts as a reference to the outputted dataframe from the previous function. 
# As a result, this lets you read/write code from left -> right, instead of left <- right. 
# To illustrate, consider the following four ways of summarizing our data:

# (1) The most compact approach, but a pain to read/write
summarize(group_by(mutate(select(dLong,SubjID,Offer.RT),Missed=ifelse(Offer.RT==0,1,0)),SubjID),Missed=sum(Missed))

# (2) Less compact, a little easier to parse but still kind of a pain
summarize(
  group_by(
    mutate(
      select(dLong,SubjID,Offer.RT),
      Missed=ifelse(Offer.RT==0,1,0)
    ),
    SubjID
  ),
  Missed=sum(Missed)
)

# (3) Slightly less compact, but MUCH easier to understand
dLong %>% 
  select(SubjID,Offer.RT) %>% 
  mutate(Missed=ifelse(Offer.RT==0,1,0)) %>% 
  group_by(SubjID) %>% 
  summarize(Missed=sum(Missed))

# (4) For completeness, here's what's actually happening with the pipe.
# The initial '.' argument is implicit/optional and is just a way of saying:
# "Use the output from the previous object (usually a dataframe) as the 
#  first argument for the next function"
dLong %>% 
  select(.,SubjID,Offer.RT) %>% 
  mutate(.,Missed=ifelse(Offer.RT==0,1,0)) %>% 
  group_by(.,SubjID) %>% 
  summarize(.,Missed=sum(Missed))

# When using RStudio, the 'pipe' operator can be generated with 'Ctrl + Shft + M'.
# On Mac, the keyboard shortcut is 'Cmd + Shft + M'. 
# For the remainder of these tutorials, we will primarily be using the pipe. 


# Grouped mutate/filter: ---------------------------------------

# How does previous feedback effect subsequent decisions?

# First, create a new dataframe that contains 'shifted' variable from previous trial
dLong_prv <- dLong %>% 
  mutate(Norm_f=ifelse(Norm==1,"Pro","Anti")) %>% #Convert 'Norm' into string/factor
  group_by(SubjID,BlockID) %>% 
  select(TrialID,Norm_f,Offer,Offer.RT,NumGiv) %>% #Notice that grouped variables are always retained
  mutate(NumGiv.P = lag(NumGiv), #Compute new variable that references feedback from previous trial 
         Offer_B=Offer/8)  #Binarize outcome variable for logistic smooth to work

# QA to make sure lag variable worked correctly. 
# Here, 'slice' returns the first two rows of each group. 
dLong_prv %>% slice(1:3) %>% View()
  
dLong_prv %>% 
  ggplot(aes(NumGiv.P,Offer_B)) +
  geom_jitter(shape=1,alpha=.2,height = .1) + #Jitter to see distribution
  bin_smooth() + #Logistic regression model fit
  list()

# Is the effect still there when we breakdown by norm?
dLong_prv %>% 
  ggplot(aes(NumGiv.P,Offer_B,color=Norm_f)) + 
  geom_jitter(shape=1,alpha=.2,height = .1) + #Jitter to see distribution
  bin_smooth() #Logistic regression model fit

# Are certain subjects more sensitive to previous feedback?
dLong_prv %>% 
  ggplot(aes(NumGiv.P,Offer_B,color=Norm_f)) + 
  bin_smooth() + 
  facet_wrap(~SubjID)
  
# Most fMRI analyses are based on 'contrasting' the relative neural activation between trial 
#  types. So if we want to contrast VMPFC activity between Cooperate vs. Free-Ride trials, then
#  we will have to exclude subjects who had invariant decisions (i.e., always made the same choice).

# In addition, if we want to compare this activation between norms, then we can only include subjects
#  with invariant decisions within each norm. 

# How many subjects are lost?
dKeep <- dLong %>% 
  group_by(SubjID,Norm) %>% 
  summarize(choiceVar=var(Offer)) %>% #compute variance of decisions within each norm
  filter(choiceVar>0) %>% #remove rows where subjects have no decision variability
  filter(n()>1) %>% #remove subjects who have invariable decisions in at least one norm
  distinct(SubjID) #only preserve rows that have unique Subject ID

# Tip: You can also combine 'group_by' and 'mutate' to your advantage 
#  (e.g., grand vs. group-mean centering)

# Which subjects were lost? Use 'anti_join' to preserve rows that are NOT in the other dataframe.
dExclude <- dLong %>% 
  distinct(SubjID) %>% 
  anti_join(dKeep,by="SubjID") %>% 
  select(SubjID)

# Yet another way of excluding subjects. Though complicated, this is a very practical use of 
#  filter. The '!' means NOT. So basically, remove any subject IDs that overlap with dExclude's
#  subject IDs. 
dLong_cln <- dLong %>% filter(!SubjID %in% dExclude$SubjID)

# If we remove the '!', then it only preserves subjects we've excluded.
dLong_excl <- dLong %>% filter(SubjID %in% dExclude$SubjID)

# Joining and Merging Data --------------------------------------------

# Let's take a look at the cleaned qualtrics data. 
tbl_df(read.csv("PGGfMRI_QualtricsCleaned.csv"))

# It looks like we have four variables. The number of cents (out of $1.00) given to a prosocial
#  school student (DG_Pro), an antisocial student (DG_Anti), and the extent to which each subject
#  identifies with the prosocial school (ID_Pro) and antisocial school (ID_Anti). 

# We want to see if cooperative subjects also tend to donate more in the dictator game, but we 
#  aren't interested in whether the donation is selective to one school. Let's compute a composite
#  variable that averages each subject's generosity from the DG and store in a new dataframe. 
#  Also note that the ID variable is 'SubjectID' rather than 'SubjID'. Let's change this so that we
#  have a common variable name between our qualtrics data and the E-Prime data. 
dQualtrics <- tbl_df(read.csv("PGGfMRI_QualtricsCleaned.csv")) %>% 
  mutate(DG_Mean = (DG_Pro + DG_Anti)/2 ) %>% 
  rename(SubjID = SubjectID)


# I use left_join 99% of the time but I've included these other examples for you to go through:
dQualtrics %>% left_join(dLong_cln) #Retains matches in dLong_cln + 8 extra subs that were excluded
dQualtrics %>% right_join(dLong_cln) #Only retains matches in dLong_cln
dQualtrics %>% full_join(dLong_cln) #Retains observations that do not match for either dataset.
dQualtrics %>% anti_join(dLong_cln) #Only retains observatations that do NOT match in right dataset.
dQualtrics %>% semi_join(dLong_cln) #Like left_join, but only keeps columns from left dataset.
  
# Now we summarize our long data, merge it with the qualtrics data, and proceed as usual.
dLong2 %>% 
  group_by(SubjID) %>% 
  summarize(Cooperation_Mean=mean(Offer)) %>% 
  left_join(dQualtrics) %>% 
  ggplot(aes(DG_Mean,Cooperation_Mean)) + geom_point() + geom_smooth(method=lm)

# Alternatively, we can pipe this right into a multilevel model
dLong2 %>% 
  mutate(Offer_B = Offer/8) %>% #Need binary outcome variabe for logistic regression
  filter(Missed == 0) %>% #Remove missed trials
  left_join(dQualtrics) %>% #Merge with qualtrics data
  geeglm(Offer_B ~ DG_Mean + LogRT + TrialSum, data=., id=SubjID, family=binomial) %>% summary()

# BONUS: You can also use the 'broom' package to plot regression diagnostics. For example,
#  we can plot the residuals against the fitted values to assess homoscedascicity (as well
#  as coloring the most influential subjects)
library(broom)
dLong2 %>% 
  group_by(SubjID) %>% 
  summarize(Cooperation_Mean=mean(Offer)) %>% 
  left_join(dQualtrics) %>% 
  do(augment(lm(Cooperation_Mean~ DG_Mean, data=.))) %>% 
  bind_cols(select(dQualtrics,SubjID)) %>% 
  ggplot(aes(.fitted,.resid,fill=.cooksd)) + geom_point() +
  geom_label_repel(aes(label=SubjID)) + geom_smooth() + 
  scale_fill_gradient(low="white",high="pink") +
  geom_hline(yintercept=0,linetype="dashed")


# Reshaping data ----------------------------------------------------------

# Currently, our qualtrics data has separate variables for responses directed towards students
#  associated with each norm (i.e., Prosocial, Antosocial). But this isn't very helpful
#  if we want to compare responses between norms. For instance, we might want to examine whether
#  higher identification with each norm is associated with greater cooperation with that school. 
#  In order to do so, we need to slightly reshape our data so that 'Norm' is a separate variable. 
#  We already have this in our long data, we just need our qualtrics data to behave the same. 


# This is where the 'tidyr' package comes into play. To get the most out of tidyr, try to 
#  conceptualize your reshaping problem in terms of 'key-value pairings'. In our case, 
#  'Norm' is the key and 'Identification' is the value we are associated with each. 
library(tidyr)
dQual_norm <- dQualtrics %>% 
  select(SubjID,ID_Pro,ID_Anti) %>% #Narrow down the identification variables we want
  gather(Norm,ID,ID_Pro,ID_Anti) %>% #Notice we lose a column and double the observations
  mutate(Norm=str_replace(Norm,"ID_","")) #Replace the leading 'ID_' with blank text
  
dQual_norm %>% arrange(SubjID,Norm) #Rearrange to QA/examine wide data

# Now we want to aggregate our long data by SubjID AND Norm
dLong %>% 
  group_by(SubjID,Norm) %>% 
  summarize(Cooperation_Mean=mean(Offer)) %>% 
  mutate(Norm=ifelse(Norm==1, "Pro", "Anti")) %>% #Recode 'Norm' so that it matches dataframe above
  left_join(dQual_norm) %>% #Now we can join our dataframes
  ggplot(aes(Cooperation_Mean,ID,color=Norm)) + geom_point() + geom_smooth(method=lm)

# Advanced: exploratory analysis ---------------------------------------------

# Disclaimer: the approach below may be highly beneficial for EXPLORATORY analysis but 
#  can be abused for CONFIRMATORY analysis. This essentially identifies variables that most 
#  strongly moderate your pattern of interest and then plots them in order. Remember to 
#  adjust for multiple comparisons and be weary of p-hacking. 

dBetas_norm <- tbl_df(read.csv("PGGfMRI_ROI_betas.csv"))

dNorm <- dBetas_norm %>% 
  left_join(dQual_norm) %>% 
  left_join(
    dLong %>% mutate(Norm=ifelse(Norm==1,"Pro","Anti"),OfferB=Offer/8)
  )

adj_vars <- names(select(dBetas_norm,MPFC:vACC)) #Store names of variables to interact
dOut <- NULL #Initialize dataframe where results will be stored
for (var in adj_vars) { #var = adj_vars[1]
  cat(str_c("\nRunning Interaction with: ",var)) #Print current model to console
  dOut <- dOut %>% bind_rows( 
    dNorm %>% mutate_(adj=var) %>% 
      do(tidy(geeglm(OfferB~Norm*adj,data=.,id=SubjID,family=binomial))) %>%
      mutate(adj=var,sig=ifelse(p.value<.05,1,0)) #Flag 'significant' results w/ a '1'
  )
}

dOut %>% filter(term=="NormPro:adj") %>% arrange(p.value) %>% View()
dSigVars <- dOut %>% filter(term=="NormPro:adj") %>% arrange(p.value) %>% filter(sig>0) %>% select(adj)

dBetas_norm %>% 
  left_join(
    dLong %>% mutate(Norm=ifelse(Norm==1,"Pro","Anti")) %>% 
      group_by(SubjID,Norm) %>% summarise(Offer=mean(Offer))
  ) %>% 
  scattergrid(DV="Offer",IVs=dSigVars$adj,color="Norm",method="rlm",se=T)
       