# Purpose: Some helper functions for R workshop
# Author/Instructor: Julian Wills
# Date: 6/29/2016


# Counts the number of valid (non-NA) observations that may covary with specified column
count_valid_cors  <- function(df, var, ...)  {
  d1 <- df %>% filter_(!is.na(var)) 
  d2 <- df %>%  select(which(sapply(., is.numeric))) 
  d1 %>% count_nas() %>% rename(NAs=V1,var=rowname) %>% 
    left_join(
      d2 %>% summarise_each(funs(sd(.,na.rm=T)),1:ncol(d2)) %>% t() %>% 
        as.data.frame() %>% add_rownames() %>% tbl_df() %>% rename(sd=V1,var=rowname) 
    ) %>% arrange(sd)
} 

# Sums the number of missing values for each column, arranges highest to lowest, and returns as 2-column vector
count_nas  <- function(df, ...)  {
  df_range <- df %>% ncol()
  df %>% summarise_each(funs(sum(is.na(.))),1:df_range) %>% t() %>% 
    as.data.frame() %>% add_rownames() %>% tbl_df() %>% arrange(desc(V1))
} 

# Wrapper for perm_gglines. Adds lines mean, median, and zero.
boot_gglines <- function(p,...) {
  perm_gglines(p,boot=TRUE)
}

# Adds CI interval for null distribution (red) and original estimate (blue)
perm_gglines <- function(p,boot=FALSE,...){
  
  p = (p + geom_histogram(bins = 40,fill="grey",color="black") +
         geom_vline(aes(xintercept=base_est),color="blue") +
         geom_vline(aes(xintercept=base_est+base_SE),linetype="dashed",color="blue") +
         geom_vline(aes(xintercept=base_est-base_SE),linetype="dashed",color="blue") +
         geom_vline(aes(xintercept=low_CI),linetype="dashed",color="red") +
         geom_vline(aes(xintercept=high_CI),linetype="dashed",color="red"))
  
  if (boot) {
    p <- (p + geom_vline(aes(xintercept=mean),color="red") +
            geom_vline(aes(xintercept=median),color="green") +
            geom_vline(xintercept=0,linetype="dashed",color="black"))
  }
  
  return(p)
}

# Computes provided confidence interval for each permuted parameter.
# Also appends alpha level needed to reject parameters within the confidence interval. 
perm_ci <- function(df1,CI=.95) {
  
  CI_L <- (1-CI)/2
  df_lines <- df1 %>% filter(replicate!=0) %>%
    group_by(term) %>% arrange(estimate) %>% slice(round(n()*CI_L)) %>% ungroup() %>% 
    select(term,low_CI=estimate) %>% 
    bind_cols(
      df1 %>% filter(replicate!=0) %>%
        group_by(term) %>% arrange(estimate) %>% slice(round(n()*(1-CI_L))) %>% ungroup() %>% 
        select(high_CI=estimate)
    ) %>% 
    left_join(
      df1 %>% filter(replicate!=0) %>% group_by(term) %>% 
        summarise(mean=mean(estimate),median=median(estimate))
    ) %>% 
    left_join(
      df1 %>% filter(replicate==0) %>% group_by(term) %>% 
        select(term,base_est=estimate,base_SE=std.error)
    )
  
  
  df_alpha <- df1 %>% group_by(term) %>% 
    distinct(term,replicate) %>% 
    mutate(base_est = ifelse(replicate==0,estimate,NA),
           base_est = first(base_est),
           close_est = abs(estimate-base_est)) %>% 
    filter(replicate!=0) %>% 
    mutate(nSims=max(replicate)) %>% 
    arrange(estimate) %>% 
    add_rownames() %>% mutate(id=as.numeric(rowname)) %>% select(-rowname) %>% 
    group_by(term) %>% mutate(id=id-first(id)+1) %>% 
    arrange(close_est) %>% 
    filter(close_est<5 & close_est>-.5) %>% 
    slice(1) %>% ungroup() %>% select(-close_est) %>% 
    mutate_each(funs(round(.,2)),base_est) %>% 
    mutate(perTile=id/nSims,
           alpha=ifelse(perTile>.5,1-perTile,perTile),
           alpha=alpha*2) %>% 
    select(term,base_est,nSims,id,perTile,alpha) %>% 
    mutate(p_str=ifelse(round(alpha,2)>0,str_c(term," (p=",round(alpha,3),")"),term)) %>% 
    select(term,alpha,p_str)
  
  df_fin <- df1 %>% left_join(df_lines) %>% left_join(df_alpha)
  return(df_fin)
  
}

# Computes provided confidence interval for each bootrapped parameter.
# Also appends alpha level needed to reject parameters within the confidence interval. 
boot_ci <- function(df1,CI=.95) {
 
  CI_L <- (1-CI)/2
  df_lines <- df1 %>% filter(replicate!=0) %>%
    group_by(term) %>% arrange(estimate) %>% slice(round(n()*CI_L)) %>% ungroup() %>% 
    select(term,low_CI=estimate) %>% 
    bind_cols(
      df1 %>% filter(replicate!=0) %>%
        group_by(term) %>% arrange(estimate) %>% slice(round(n()*(1-CI_L))) %>% ungroup() %>% 
        select(high_CI=estimate)
    ) %>% 
    left_join(
      df1 %>% filter(replicate!=0) %>% group_by(term) %>% 
        summarise(mean=mean(estimate),median=median(estimate))
    ) %>% 
    left_join(
      df1 %>% filter(replicate==0) %>% group_by(term) %>% 
        select(term,base_est=estimate,base_SE=std.error)
    )
  
  df_alpha <- df1 %>% group_by(term) %>% arrange(estimate) %>% 
    distinct(term,replicate) %>% 
    filter(replicate!=0) %>% 
    mutate(nSims=max(replicate)) %>% 
    add_rownames() %>% mutate(id=as.numeric(rowname)) %>% select(-rowname) %>% 
    group_by(term) %>% mutate(id=id-first(id)+1) %>% 
    mutate(zero=abs(estimate-0)) %>% arrange(zero) %>% 
    slice(1) %>% ungroup() %>% select(-zero) %>% 
    mutate(perTile=id/nSims,
           boot_p=ifelse(perTile>.5,1-perTile,perTile),
           boot_p=boot_p*2) %>% 
    left_join(
      df_lines
    ) %>% 
    mutate(sig=ifelse(sign(low_CI)==sign(high_CI),1,0),
           p_str=ifelse(sig==0,str_c(term," (p=",round(boot_p,3),")"),term)) %>% 
    select(term,low_CI:base_SE,boot_p,p_str,sig)
  
  df_fin <- df1 %>% left_join(df_alpha)
  return(df_fin)
}

# Prints the first ten steps of current iteration. 
# Then print out progress and estimated time remaining. 
# BUG: underestimates first time estimate
loopFeedback <- function(nIter=2000,nFeedbackIntervals=20,time_start=proc.time()[3],cnt=0,start=1) {
  library(beepr)
  
  if (cnt == 1) { cat("\n Iteration =",i)}
  else if (cnt < 10) { cat("",i)}
  else if (cnt == 10) { cat(paste0(" ",i,"..."))}
  else if (i %% (nIter/nFeedbackIntervals) == 0) { 
    cat("\n",round(i*100/nIter),"% Completed")
    time_now <- proc.time()[3]
    run_time <- time_now - time_start
    est_full_time <- run_time  * (nIter+1-start)/(i+1-start)
    time_left <- est_full_time - run_time
    mins_left <- round(time_left/60)
    cat(", ",mins_left,"minutes remaining")
    if (mins_left < 1) {
      beep(sound=2)
      time_end <- proc.time()[3]
      run_time <- time_end - time_start
      cat("\n \n Completed Permutation for","in",run_time/60,"minutes","--------------------- \n")
    }
  }
}

# Attempts model on supplied dataframe. Returns 0 (Success), 1 (Error), or 2 (Warning).
# If ignoreWarnings = TRUE, then return 0 (Success) even if warning produced. 
tryModel <- function(df1, ignoreWarnings=FALSE,...) {
  library(broom)
  
  errorCheck <- tryCatch(
    {tidy(tmpModel(df1)) %>% slice(1) %>% extract(1)},
    error=function(cond) {return(1)},
    warning=function(cond) {
      if (ignoreWarnings) return(0)
      else return(2)
      })
  
  if (errorCheck=="(Intercept)") {return(0)}
  else (return(errorCheck))
}



## Visualizing scatterplot matrices
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


# scatterplots w/ LOESS, correlations, and histograms
pairPlot <- function(df,...)
{
  p <- pairs(df,lower.panel=panel.smooth,upper.panel=panel.cor, diag.panel = panel.hist)
  # print(p)
}

# view first 10 rows in dataframe
Vhead <- function(df,n=10,...)
{
  df2 <- df %>% head(n) %>% View()
}

#Return all correlations above .12 with given variable (and plots 25 most significant in order)
testCors <- function(df,colName,Poly1=1,Poly2=1,gg=F,...) { 
  dT3 <- NULL;
  df %<>% select(which(sapply(., is.numeric)))
  for (col in names(df)) {
    if (col!=colName) {
      v1 <- select_(df,colName) %>% unlist() 
      v2 <- select_(df,col) %>%  unlist()
      v1 <- v1^Poly1; v2 <- v2^Poly2; 
      dT2 <- cor.test(v1,v2)
      if (dT2$p.value < .12) {
        dT3$Variable <- c(dT3$Variable,col)
        dT3$p.value <- c(dT3$p.value,dT2$p.value)
        dT3$p.value <- c(dT3$p.value,dT2$p.value)
        dT3$estimate <- c(dT3$estimate,dT2$estimate)
      }
    }
  }
  dT3 <- dT3 %>% as.data.frame %>% distinct(Variable) %>% mutate(effect=abs(estimate)) %>% arrange(desc(effect))
  dT4 <- dT3 
  if (gg==F) {
    if (length(dT3$Variable)>9) {dT4 <- dT3 %>% slice(1:9)} #trim to top 9
    (pairPlot(select_(df,.dots=c(colName,dT4$Variable)) %>%  
                mutate_each(funs(.^Poly2),2:ncol(dT4)+1) %>% 
                mutate_each(funs(.^Poly1),1)))
  } else {
    if (length(dT3$Variable)>25) {dT4 <- dT3 %>% slice(1:25)} #trim to top 9
    scattergrid(df,DV=colName,IVs = dT4$Variable,...)
  }

  cat(colName,"positively associated with:\n",dT3[dT3$estimate>0,]$Variable,"\n\n")
  cat("and negatively associated with:\n",dT3[dT3$estimate<0,]$Variable,"\n\n")
  return (dT3)
}

# Scatterplot grid for specified DV. Takes  DV string and optional vector of IV strings. 
# If more than 25 IVs in dataframe, returns the first 25. 
scattergrid <- function(df, DV, IVs = NA, method="auto", se=F, alpha=1) { #df = dT; DV="EVA8"
  library(gridExtra); library(stringr); P <- list()
  if (is.na(IVs)) {
    DVdrop = str_c("-",DV) #to drop DV column
    IVs <- names(select_(df,DVdrop)) 
    if (length(IVs)>25) {IVs <- IVs[1:25]} #limit to 25
  } 
  DVclass = sapply(select_(df,DV),class)
  if (DVclass=="integer" | DVclass=="numeric") {df <-  df %>% mutate_each_(funs(jitter),DV)}
  for (IV in IVs) { #IV=IVs[1]
    old <- theme_set(theme_light(base_size = 9)) 
    IVdrop = str_c("-",IV)
    IVclass = sapply(select_(df,IV),class)
    if (IVclass=="integer" | IVclass=="numeric") {df <-  df %>% mutate_each_(funs(jitter),IV)}
    p <- df %>% ggplot(aes_string(x = IV, y = DV)) + geom_point(alpha=alpha) + 
      geom_smooth(se=se,method=method) + ylab("")
    P <- c(P,list(p))
  }
  do.call(grid.arrange, c(P))
}

#create index variable for sorted variable for subsequent plotting
sortIdx <- function(df,colName) { 
  df %>% arrange_(colName) %>% add_rownames() %>% 
    mutate(Sort=as.numeric(rowname)) %>% select(-rowname)
  dT <- NULL;
  dT <- df %>% arrange_(colName) %>% add_rownames() %>% 
    mutate(Sort=as.numeric(rowname)) %>% select(-rowname)
  return (dT)
}

#logistic regression ggplot
bin_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

#poisson regression ggplot
p_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "poisson"), ...)
}

# standard error of the mean
se <- function(...) {
  sd(.)/(sqrt(n())-1)
}


# Better plots ------------------------------------------------------------

theme_Publication <- 
  function(base_size=12) {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size) + 
      theme(plot.title = element_text(face = "bold",size = rel(1), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2,size = rel(1.05),hjust=.5),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_blank(), #added
            panel.grid.minor = element_blank(),
            axis.line.x = element_line(color="black", size = 1), #added
            axis.line.y = element_line(color="black", size = 1),
            # legend.title=element_blank(), #added
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            legend.text=element_text(size = rel(.95)),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold"))
    )
    
  }

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

select <- dplyr::select 
