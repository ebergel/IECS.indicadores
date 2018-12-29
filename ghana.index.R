#' Improving Maternal Health Measurement Capacity and Use (IMHM)
#' 
#' Propose methodology to generate an index with EPMM Phase I indicators 
#' as a proxy of health system performance for site selection
#' 
#' Compute indicator for GHANA
#' 
#' 

# install.packages(c("dplyr","ggplot2","ggcorrplot","devtools","psych" ,"GGally","boot","gtools","sjPlot","readr","descr"))

library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(devtools)
library(psych)
library(GGally)
library(boot)
library(gtools)
 library(sjPlot)
library(readr)
library(descr)

# load data

df <- read_csv("data.ghana.by.region.csv")


# var names?
names(df)

#[1] "NATIONAL" "REGION"   "ID" "MMR"      "ANC"      "SBA"      "PNC"   

# rename variables
df <- dplyr::rename(  df,   ANC = ANC)
df <- dplyr::rename(  df,   ID = DISTRICT)



# RECODE 0 to NA
 df[df == 0] <- NA
 
 
# remove missing data
 df <- df[complete.cases(df),]


# compute dataframe summary
summary( dplyr::select(df, MMR , ANC ,  PNC  ,   SBA))
  
# variable TRANSFORMATIONS
 
# df$MMR.sqr    <-    sqrt(df$MMR) 
# df$SBA.cube   <-    df$SBA * df$SBA  * df$SBA 
# 
# df$PNC.sq     <-    df$PNC * df$PNC  
# df$PNC.sq     <-    sqrt(df$PNC ) 
# 
# df$ANC.sq    <-    df$ANC * df$ANC 
# df$ANC.sq    <-    sqrt(df$ANC ) 

# Transform to ZScores
df$MMR     <-    as.numeric(scale(  df$MMR,  center = TRUE, scale = TRUE)  )
df$SBA     <-    as.numeric(scale(  df$SBA,  center = TRUE, scale = TRUE) )
df$ANC     <-    as.numeric(scale(  df$ANC, center = TRUE, scale = TRUE) )
df$PNC     <-    as.numeric(scale(  df$PNC,  center = TRUE, scale = TRUE) )

# summary after transfomations and ZScores
summary( dplyr::select(df, MMR , ANC ,  PNC  ,   SBA))
 
# compute correlation matrix
df.CM           <- dplyr::select(df, MMR,           ANC,          PNC,          SBA           ) 
df.CM.noMMR     <- dplyr::select(df,                ANC,          PNC,          SBA           )

# remove missing values in dataframe
df.CM        <- df.CM  [complete.cases(df.CM  ), ]
df.CM.noMMR  <- df.CM.noMMR[complete.cases(df.CM2.noMMR), ]

# plot correlation matrix
ggpairs(df.CM)
ggpairs(df.CM.noMMR)
 

# factor analysis 

  # all vars
cormat <- cor(df.CM)
cormat
factors_data <- fa(r = cormat, nfactors = 3)
factors_data


  # excluding MMR
cormat <- cor(df.CM.noMMR)
cormat
factors_data <- fa(r = cormat, nfactors = 2)
factors_data



#principal components analysis

  #all vars
fit <- princomp(df.CM, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

  #without MMR
fit <- princomp(df.CM.noMMR, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)




# creating INDEX

d<- df.CM

index.4vars   <- d$ANC +  d$PNC +   d$SBA - d$MMR
index.4vars.z <- as.numeric(scale(  index.4vars, center = TRUE, scale = TRUE)  )
 
index.3vars   <- d$ANC +  d$PNC +   d$SBA  
index.3vars.z <- as.numeric(scale(  index.3vars, center = TRUE, scale = TRUE)  )

d<- data.frame(index.3vars.z, index.4vars.z )

ggpairs(d)

index.4vars.qrt <- as.integer(gtools::quantcut(index.4vars.z))
index.3vars.qrt <- as.integer(gtools::quantcut(index.3vars.z))
 
df.index <- dplyr::select(df,  ID, MMR, ANC, SBA, PNC)
df.index        <- df.index  [complete.cases(df.index  ), ]
df.index['index.4vars'] <-  index.4vars.z
df.index['index.4vars'] <-  index.3vars.z
df.index['index.4vars.qrt'] <-  index.4vars.qrt
df.index['index.4vars.qrt'] <-  index.3vars.qrt

dplyr::filter(df.index,  index.4vars.qrt ==1)$ID
dplyr::filter(df.index,  index.4vars.qrt ==2)$ID 
dplyr::filter(df.index,  index.4vars.qrt ==3)$ID
dplyr::filter(df.index,  index.4vars.qrt ==4)$ID

# export data and index
library(xlsx)
write.xlsx(df.index, "index.xlsx")




