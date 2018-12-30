#' Improving Maternal Health Measurement Capacity and Use (IMHM)
#' 
#' Propose methodology to generate an index with EPMM Phase I indicators 
#' as a proxy of health system performance for site selection
#' 
#' Bergel E. (IECS), December 28, 2018
#' bergel@gmail.com
#' 
  

# Load Libraries
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
library(xlsx)

##################################################################
# R function to generate index

fx.IMHM.index <- function(df, ws = NULL, q = 4){ 
     
      # Transform to ZScores
      df$MMRz     <-    as.numeric(scale(  df$MMR,  center = TRUE, scale = TRUE)  )
      df$SBAz     <-    as.numeric(scale(  df$SBA,  center = TRUE, scale = TRUE) )
      df$ANCz     <-    as.numeric(scale(  df$ANC, center = TRUE, scale = TRUE) )
      df$PNCz     <-    as.numeric(scale(  df$PNC,  center = TRUE, scale = TRUE) )
      df$UABz     <-    as.numeric(scale(  df$UAB,  center = TRUE, scale = TRUE) )
      
      # creating INDEX  
        
        df$index         <-   df$ANCz +   
                              df$PNCz +   
                              df$SBAz +   
                              df$UABz -  
                              df$MMRz
        
      df$index.z       <- as.numeric(scale(  df$index, center = TRUE, scale = TRUE)  )
      df$index.qrt     <- as.integer(gtools::quantcut(df$index.z, q = q))
 
        
      if ( !is.null(ws) ){
  
        df$index.w       <-   ws[['ANC']] * df$ANCz + 
                              ws[['PNC']] * df$PNCz + 
                              ws[['SBA']] * df$SBAz + 
                              ws[['UAB']] * df$UABz + 
                              ws[['MMR']] * df$MMRz
        
        df$index.w.z     <- as.numeric(scale(  df$index.w, center = TRUE, scale = TRUE)  ) 
        df$index.w.qrt   <- as.integer(gtools::quantcut(df$index.w.z, q = q))
        
      } 
 
 
      
      return(df) 
}


##################################################################
# Argentina  - load data

# Read Data
df <- read_csv("Phase I - Argentina Data.csv")  

# rename variables
df <- dplyr::rename(  df,   ID = Provincia)  
 

##################################################################
# Argentina  - Exploratory Analysis

 
# compute correlation matrix 
df.CM          <- dplyr::select(df, MMR,   ANC ,  PNC,  SBA, UAB ) 
ggpairs(df.CM)
ggpairs(df.CM2) 

# drop PNC and SBA

df.CM          <- dplyr::select(df, MMR,   ANC ,    UAB ) 

# factor analysis 
 
cormat <- cor(df.CM)
cormat
factors_data <- fa(r = cormat, nfactors = 3)
factors_data


ws <- list()
ws['ANC'] <-  0.48
ws['PNC'] <-  0
ws['SBA'] <-  0
ws['UAB'] <-  0.75
ws['MMR'] <- -0.73

df <- fx.IMHM.index(df, ws )

# compute correlation matrix 
df.CM          <- dplyr::select(df, MMR,   ANC ,  UAB, index.z, index.w.z ) 
ggpairs(df.CM) 


 

################################################################## 
# Argentina Region -  Analysis

 
# Read Data
df <- read_csv("Phase I - Argentina Data.csv")  

# rename variables
df <- dplyr::rename(  df,   ID = Region)  
 

# compute summary by region

#  export data and index
df <-   df %>% 
        dplyr::group_by(ID ) %>%
        dplyr::summarise( MMR = mean(MMR) ,
                          ANC = mean(ANC) ,
                          UAB = mean(UAB) ,
                          SBA = mean(SBA) ,
                          PNC = mean(PNC)  
                          )  
ws <- list()
ws['ANC'] <-  0.48
ws['PNC'] <-  0
ws['SBA'] <-  0
ws['UAB'] <-  0.75
ws['MMR'] <- -0.73 

df <- fx.IMHM.index(df, ws,q = 3)

# compute correlation matrix 
df.Argentina.index.by.region         <- dplyr::select(df, ID, MMR,   ANC ,  UAB, index.w.z ,  index.w.qrt) 
 
write.csv(df.Argentina.index.by.region , "Argentina.index.by.region.csv")

  

################################################################## 
# Argentina State -  Analysis - Region: Centro

 
# Read Data
df <- read_csv("Phase I - Argentina Data.csv")  

# rename variables
df <- dplyr::rename(  df,   ID = Provincia)  

# compute summary by Provincia
# rename variables
df <- dplyr::filter(df, Region == 'CENTRO') 
 
ws <- list()
ws['ANC'] <-  0.48
ws['PNC'] <-  0
ws['SBA'] <-  0
ws['UAB'] <-  0.75
ws['MMR'] <- -0.73 

df <- fx.IMHM.index(df, ws ,q =  3)

# compute correlation matrix 
df.Argentina.index.CENTRO        <- dplyr::select(df, ID, MMR,   ANC ,  UAB, index.w.z ,  index.w.qrt)  
 
write.csv(df.Argentina.index.CENTRO , "Argentina.index.CENTRO.csv")

##################################################################   

################################################################## 
# Argentina State -  Analysis - Region: Noroeste

 
# Read Data
df <- read_csv("Phase I - Argentina Data.csv")  

# rename variables
df <- dplyr::rename(  df,   ID = Provincia)  

# compute summary by Provincia
# rename variables
df <- dplyr::filter(df, Region == 'NOROESTE') 
 
ws <- list()
ws['ANC'] <-  0.48
ws['PNC'] <-  0
ws['SBA'] <-  0
ws['UAB'] <-  0.75
ws['MMR'] <- -0.73 

df <- fx.IMHM.index(df, ws ,q =  3)

# compute correlation matrix 
df.Argentina.index.Noroeste        <- dplyr::select(df, ID, MMR,   ANC ,  UAB, index.w.z ,  index.w.qrt)  
 
write.csv(df.Argentina.index.Noroeste , "Argentina.index.Noroeste.csv")

##################################################################  
