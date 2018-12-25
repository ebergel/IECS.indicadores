#' Improving Maternal Health Measurement Capacity and Use (IMHM)
#' 
#' Propose methodology to generate an index with EPMM Phase I indicators 
#' as a proxy of health system performance for site selection
#' 
#' Bergel E. (IECS), November 27, 2018
#' 
#' 



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
df.ANC4 <- read_csv("index.ANC.4visits.csv")
df.MMR  <- read_csv("index.maternal.mortality.csv")
df.PNC  <- read_csv("index.postnatal.care.csv")
df.SBA  <- read_csv("index.skilled.birth.attendant.csv")

# remove missing data
df.ANC4 <- df.ANC4[complete.cases(df.ANC4),]
df.MMR  <- df.MMR[ complete.cases(df.MMR),]
df.PNC  <- df.PNC[ complete.cases(df.PNC),]
df.SBA  <- df.SBA[ complete.cases(df.SBA),]

# remove outlier - sierra leona (SLE)

df.MMR <- dplyr::filter(df.MMR, df.MMR$ISO3codes != 'SLE')


# set country varname 
 
df.ANC4 <- dplyr::rename(  df.ANC4,   country = Countriesandareas)
df.PNC  <- dplyr::rename(  df.PNC,    country = Countriesandareas)
df.SBA  <- dplyr::rename(  df.SBA,    country = Countriesandareas)
df.MMR  <- dplyr::rename(  df.MMR,    country = Countries)



# add Argentina to PNC dataset 

df.PNC <- rbind(df.PNC, c('Argentina',2015, 98))
df.PNC <- df.PNC[order(df.PNC$country),] 

# get latest available data (last year)

df.PNC   <- dplyr::arrange(df.PNC, country, desc(Year))
df.PNC   <- df.PNC[!duplicated(df.PNC$country),]
df.SBA   <- dplyr::arrange(df.SBA, country, desc(Year))
df.SBA   <- df.SBA[!duplicated(df.SBA$country),]
df.ANC4  <- dplyr::arrange(df.ANC4, country, desc(Year))
df.ANC4  <- df.ANC4[!duplicated(df.ANC4$country),] 

df.MMR$MMR <- df.MMR$`2015`
df.MMR$year <- 2015
df.MMR      <- dplyr::select(df.MMR, ISO3codes, country, year, MMR)

# Merge,  generates working dataset

df.ANC4 <- dplyr::rename(  df.ANC4,   ANC4 = National)
df.PNC  <- dplyr::rename(  df.PNC,    PNC  = National)
df.SBA  <- dplyr::rename(  df.SBA,    SBA  = National)

df.ANC4 <- dplyr::rename(  df.ANC4,   year_ANC4 = Year)
df.PNC  <- dplyr::rename(  df.PNC,    year_PNC  = Year)
df.SBA  <- dplyr::rename(  df.SBA,    Year_SBA  = Year)
df.MMR  <- dplyr::rename(  df.MMR,    Year_MMR  = year)

df   <- dplyr::left_join(  df.MMR, df.ANC4 , df.SBA, by = 'country' )
df   <- dplyr::left_join(  df,   df.SBA, by = 'country' )
df   <- dplyr::left_join(  df,   df.PNC, by = 'country' )

 
 
# df   <- dplyr::inner_join(  df.MMR, df.ANC4 , df.SBA, by = 'country' )
# df   <- dplyr::inner_join(  df,   df.SBA, by = 'country' )
# df   <- dplyr::inner_join(  df,   df.PNC, by = 'country' )
# df <- df2

df$PNC <- as.integer(df$PNC)


# compute dataframe summary

summary( dplyr::select(df, MMR , ANC4 ,  PNC  ,   SBA))
  
# fix string vars 
 
df$MMR.sqr    <-    sqrt(df$MMR) 
df$SBA.cube   <-    df$SBA * df$SBA  * df$SBA  
df$ANC4.sq    <-    df$ANC4 * df$ANC4  
df$PNC.sq     <-    df$PNC * df$PNC  


df$MMR.sqr    <-    as.numeric(scale(  df$MMR.sqr, center = TRUE, scale = TRUE)  )
df$SBA.cube   <-    as.numeric(scale(  df$SBA.cube, center = TRUE, scale = TRUE) )
df$ANC4.sq    <-    as.numeric(scale(  df$ANC4.sq , center = TRUE, scale = TRUE) )
df$PNC.sq     <-    as.numeric(scale(  df$PNC.sq  , center = TRUE, scale = TRUE) )
 
 
# compute correlation matrix

df.CM          <- dplyr::select(df, MMR,   MMR.sqr, ANC4, ANC4.sq, PNC, PNC.sq , SBA, SBA.cube )
df.CM2         <- dplyr::select(df,     MMR.sqr, ANC4.sq,  PNC.sq ,   SBA.cube )
df.CM3         <- dplyr::select(df,     MMR , ANC4 ,  PNC  ,   SBA  )
 
df.CM2.noMMR   <- dplyr::select(df,       ANC4.sq,  PNC.sq ,   SBA.cube )
 
df.CM2.noPNC   <- dplyr::select(df,     MMR.sqr, ANC4.sq,     SBA.cube )
 

# remove missing values in dataframe

df.CM        <- df.CM  [complete.cases(df.CM  ), ]
df.CM2       <- df.CM2 [complete.cases(df.CM2 ), ]
df.CM3       <- df.CM3 [complete.cases(df.CM3 ), ]
df.CM2.noMMR <- df.CM2.noMMR[complete.cases(df.CM2.noMMR), ]
df.CM2.noPNC <- df.CM2.noPNC[complete.cases(df.CM2.noPNC), ]

# plot correlation matrix

ggpairs(df.CM)
ggpairs(df.CM2)
ggpairs(df.CM3)
ggpairs(df.CM2.noPNC)

# factor analysis 

  # all vars
cormat <- cor(df.CM2)
cormat
factors_data <- fa(r = cormat, nfactors = 3)
factors_data


  # excluding MMR
cormat <- cor(df.CM2.noMMR)
cormat
factors_data <- fa(r = cormat, nfactors = 2)
factors_data



#principal components analysis

  #all vars
fit <- princomp(df.CM2, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

  #without MMR
fit <- princomp(df.CM2.noMMR, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)


  # without PNC
fit <- princomp(df.CM2.noPNC, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)


# creating INDEX

d<- df.CM2

index.4vars   <- d$ANC4.sq +  d$PNC.sq +   d$SBA.cube - d$MMR.sqr 
index.4vars.z <- as.numeric(scale(  index.4vars, center = TRUE, scale = TRUE)  )
 
index.3vars   <- d$ANC4.sq +  d$PNC.sq +   d$SBA.cube  
index.3vars.z <- as.numeric(scale(  index.3vars, center = TRUE, scale = TRUE)  )

d<- data.frame(index.3vars.z, index.4vars.z )

ggpairs(d)

index.4vars.qrt <- as.integer(gtools::quantcut(index.4vars.z))
index.3vars.qrt <- as.integer(gtools::quantcut(index.3vars.z))
 
df.index <- dplyr::select(df, ISO3codes, country, MMR, ANC4, SBA, PNC)
df.index        <- df.index  [complete.cases(df.index  ), ]
df.index['index.4vars'] <-  index.4vars.z
df.index['index.4vars'] <-  index.3vars.z
df.index['index.4vars.qrt'] <-  index.4vars.qrt
df.index['index.4vars.qrt'] <-  index.3vars.qrt

dplyr::filter(df.index,  index.4vars.qrt ==1)$country
dplyr::filter(df.index,  index.4vars.qrt ==2)$country 
dplyr::filter(df.index,  index.4vars.qrt ==3)$country
dplyr::filter(df.index,  index.4vars.qrt ==4)$country

# export data and index
library(xlsx)
write.xlsx(df.index, "index.xlsx")




