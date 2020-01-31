#Loading Data into R
mobileBanking.Df <- read.csv("Mobile Banking in Kenya.csv", header = TRUE, stringsAsFactors = FALSE )
mobileBanking.Df <- mobileBanking.Df[,-1]

#Load required packages or install if not present.----
load.libraries <- c('data.table','tidyverse','gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs, dependences = TRUE)

#Load libraries and flag TRUE
sapply(load.libraries, require, character = TRUE)


#Check Structure of the data
dim(mobileBanking.Df)
#Data set has 43 rows with 11 variables


#Check Categorical VS Numeric Characters----
cat_vars <- names(mobileBanking.Df)[which(sapply(mobileBanking.Df, is.character))]
            #cat_vars <- c(cat_vars, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')

numeric_vars <- names(mobileBanking.Df)[which(sapply(mobileBanking.Df, is.numeric))]


#Checking data for any missing values
colSums(sapply(mobileBanking.Df, is.na))
#From thie table above it can be seen that there's no column with missing values in it.
#This therefore means we have no reason to check for the same among the variables.

#Visualizing if there's missing data
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}


plot_Missing(mobileBanking.Df[,colSums(is.na(mobileBanking.Df)) > 0, with = FALSE])

#Gaining Insights
sum(mobileBanking.Df[,'YearRemodAdd', with = FALSE] != mobileBanking.Df[,'mobileBanking.Df', with = FALSE])
### TO DO -------


#Visualizing distributions
# #Gender
# mobileBanking.Df %>% count(Gender) #Counting gender distribution--
#
# ggplot(data = mobileBanking.Df) +
#   geom_bar(mapping = aes(x=Gender))

#Data Summary----
summary(mobileBanking.Df)

####Convert character to factors----
setDT(mobileBanking.Df)[,(cat_vars) := lapply(.SD, as.factor), .SDcols = cat_vars]

mobileBanking.Df_cat <- mobileBanking.Df[,.SD, .SDcols = cat_vars]
    ##mobileBanking.Df_cont <- mobileBanking.Df[,.SD,.SDcols = numeric_vars]

#Functions for Plots---

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() +
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light()
  return(p)

}

#Plotting categorical values----
#Bar plots
doPlots(mobileBanking.Df_cat, fun = plotHist, ii = 1:4, ncol = 2)

##TO DO EXPLAIN -------

#Histograms
doPlots(mobileBanking.Df_cat, fun = plotHist, ii  = 4:7, ncol = 2)

##TO DO EXPLAIN -------

#Bar plots
doPlots(mobileBanking.Df_cat, fun = plotHist, ii = 8:11, ncol = 2)

##TO DO EXPLAIN -------

#################-----
# Association between Variables
# Numerically exploring associations between pairs of categorical
# variables is not as simple as the numeric variable case.
# The general question we need to address is,
# “do different combinations of categories seem to
# be under or over represented?” We need to understand
# which combinations are common and which are rare.
# The simplest thing we can do is ‘cross-tabulate’
# the number of occurrences of each combination.
# The resulting table is called a contingency table.
# The counts in the table are sometimes referred to as frequencies.
names(mobileBanking.Df)
xtabs(~ Gender + Have.Bak.Account, data = mobileBanking.Df)
bar_plt <- ggplot(mobileBanking.Df, aes(x = Gender, fill = Have.Bak.Account)) + geom_bar()

xtabs(~ Gender + Satisfaction, data = mobileBanking.Df)
bar_plt <- ggplot(mobileBanking.Df, aes(x = Gender, fill = Satisfaction)) +  geom_bar

xtabs(~ Gender + MB.used.for, data = mobileBanking.Df)
xtabs(~ Gender + MB.Safety, data = mobileBanking.Df)
xtabs(~ Gender + Age.Range, data = mobileBanking.Df)
xtabs(~ Age.Range + Importance, data = mobileBanking.Df)
xtabs(~ Age.Range + Gender + Bank.Visit, data = mobileBanking.Df)
xtabs(~ Age.Range +Gender + Influence, data = mobileBanking.Df)


xtabs(~ Gender + MB.used.for, data = mobileBanking.Df)

names(mobileBanking.Df)
table(mobileBanking.Df$Gender, mobileBanking.Df$MB.Safety)
