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

fx.IMHM.index <- function(df){ 
      
      # Transform to ZScores
      df$MMRz     <-    as.numeric(scale(  df$MMR,  center = TRUE, scale = TRUE)  )
      df$SBAz     <-    as.numeric(scale(  df$SBA,  center = TRUE, scale = TRUE) )
      df$ANCz     <-    as.numeric(scale(  df$ANC, center = TRUE, scale = TRUE) )
      df$PNCz     <-    as.numeric(scale(  df$PNC,  center = TRUE, scale = TRUE) )

      # creating INDEX

      df$index.4vars   <- df$ANCz +  df$PNCz +   df$SBAz - df$MMRz
      df$index.4vars.z <- as.numeric(scale(  df$index.4vars, center = TRUE, scale = TRUE)  )
       
      df$index.3vars   <- df$ANCz +  df$PNCz +   df$SBAz  
      df$index.3vars.z <- as.numeric(scale(  df$index.3vars, center = TRUE, scale = TRUE)  )
      
      df$index.MMR.z     <- df$MMRz   
      
      df$index.4vars.qrt <- as.integer(gtools::quantcut(df$index.4vars.z))
      df$index.3vars.qrt <- as.integer(gtools::quantcut(df$index.3vars.z))
      df$index.MMR.qrt   <- as.integer(gtools::quantcut(df$index.MMR.z)) 
      
      return(df) 
}

##################################################################
# Ghana Region - by region index is only MMRz

# Read Data
df <- read_csv("data.ghana.by.region.csv") 

# rename variables
df <- dplyr::rename(  df,   ID = REGION)

# Remove Ghana from database (not a region)
df <- dplyr::filter(df, ID != 'Ghana')

# RECODE 0 to NA (issue in Ghana by district)
df[df == 0] <- NA

# Generate Index
df <- fx.IMHM.index(df)


################################################################## 
# Ghana Region -  Analysis

  
#  export data and index
df.ghana.by.region <-  df %>% 
              dplyr::select(ID, index.MMR.qrt, MMR, ANC, SBA, PNC ) %>%
              dplyr::arrange( index.MMR.qrt    ) 

write.xlsx(df.ghana.by.region , "ghana.index.by.Region.xlsx")

# Random selection

df.sample <- df.ghana.by.region %>% 
             dplyr::group_by(index.MMR.qrt) %>% sample_n(size = 1 )

 
##################################################################
# Ghana Distric -   ANC, PNC, SBA  
#
# ---> 1) Brong Ahafo


#Read data
df <- read_csv("data.ghana.by.district.csv") 

# Select regions (from first level selection by Region )  c("Brong Ahafo",  "Northern")

df   <-  df  %>% dplyr::filter( REGION  == "Brong Ahafo"  )         

# rename variables
df <- dplyr::rename(  df,   ID = DISTRICT) 

 # RECODE 0 to NA (issue in Ghana by district)
df[df == 0] <- NA

# Generate Index
df <- fx.IMHM.index(df)


################################################################## 
# Ghana District -  Analysis
#
# ---> 1) Brong Ahafo

#  export data and index
df.ghana.by.district.Brong_Ahafo <-  df %>% 
              dplyr::select(REGION, ID,  index.3vars.qrt, MMR, ANC, SBA, PNC ) %>%
              dplyr::arrange(  index.3vars.qrt    ) 

write.csv(df.ghana.by.district.Brong_Ahafo , "df.ghana.by.district.Brong_Ahafo.csv")

# Random selection

df.sample <- df.ghana.by.district.Brong_Ahafo %>% 
             dplyr::group_by(index.3vars.qrt) %>% sample_n(size = 1 )

 

##################################################################
# Ghana Distric -   ANC, PNC, SBA  
#
# ---> 2) "Northern"


#Read data
df <- read_csv("data.ghana.by.district.csv") 

# Select regions (from first level selection by Region )  c("Brong Ahafo",  "Northern")

df   <-  df  %>% dplyr::filter( REGION  == "Northern"  )         

# rename variables
df <- dplyr::rename(  df,   ID = DISTRICT) 

 # RECODE 0 to NA (issue in Ghana by district)
df[df == 0] <- NA

# Generate Index
df <- fx.IMHM.index(df)


################################################################## 
# Ghana District -  Analysis
#
# ---> 2) Northern

#  export data and index
df.ghana.by.district.Northern <-  df %>% 
              dplyr::select(REGION, ID,  index.3vars.qrt, MMR, ANC, SBA, PNC ) %>%
              dplyr::arrange(  index.3vars.qrt    ) 

write.csv(df.ghana.by.district.Northern , "df.ghana.by.district.Northern.csv")

# Random selection
df.sample <- df.ghana.by.district.Northern %>% 
             dplyr::group_by(index.3vars.qrt) %>% sample_n(size = 1 )

 



