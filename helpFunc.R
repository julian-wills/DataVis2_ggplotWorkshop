# Purpose: Some helper functions for R workshop
# Author/Instructor: Julian Wills
# Date: 6/29/2016

demean <- function(df, response=NA, ...)  { #response="count" response="twid"
  # mean centers all numeric variables, except for response variable
  allVars <- df %>% names()
  numVars <- df %>% select(which(sapply(., is.numeric)))  %>% names()
  if (!is.na(response)) {numVars <- numVars[numVars!=response]}
  varIdxs <- which(allVars %in% numVars)
  df %>% mutate_each(funs(.-mean(.,na.rm=T)),varIdxs)
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
scattergrid <- function(df, DV, IVs = NA, method="auto", se=F, alpha=1, color=NULL,
                        flip=F) { #df = dT; DV="EVA8"
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
    p <- df %>% ggplot(aes_string(x = IV, y = DV, color=color)) + geom_point(alpha=alpha) + 
      geom_smooth(se=se,method=method) + ylab("")
    if (flip) {p <- p + coord_flip()} #flip coordinates if set to true
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


select <- dplyr::select 
