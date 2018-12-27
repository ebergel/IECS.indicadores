
 
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

# Ghana Region

# Read Data
df <- read_csv("data.ghana.by.region.csv") 

# rename variables
df <- dplyr::rename(  df,   ID = REGION)

# Remove Ghana from database (not a region)
df <- dplyr::filter(df, ID != 'Ghana')

# RECODE 0 to NA
df[df == 0] <- NA
 
# Remove missing data
df <- df[complete.cases(df),]


# compute dataframe summary
summary( dplyr::select(df, MMR , ANC ,  PNC  ,   SBA))

# Transform to ZScores
df$MMRz     <-    as.numeric(scale(  df$MMR,  center = TRUE, scale = TRUE)  )
df$SBAz     <-    as.numeric(scale(  df$SBA,  center = TRUE, scale = TRUE) )
df$ANCz     <-    as.numeric(scale(  df$ANC, center = TRUE, scale = TRUE) )
df$PNCz     <-    as.numeric(scale(  df$PNC,  center = TRUE, scale = TRUE) )
 
# compute correlation matrix
df.CM           <- dplyr::select(df, MMRz,           ANCz,          PNCz,          SBAz           ) 
df.CM.noMMR     <- dplyr::select(df,                ANCz,          PNCz,          SBAz           )

# remove missing values in dataframe
df.CM        <- df.CM  [complete.cases(df.CM  ), ]
df.CM.noMMR  <- df.CM.noMMR[complete.cases(df.CM.noMMR), ]

# plot correlation matrix
ggpairs(df.CM)
ggpairs(df.CM.noMMR)


# factor analysis 

  # all vars
cormat <- cor(df.CM)
cormat
factors_data <- fa(r = cormat, nfactors = 3)
factors_data


# creating INDEX

d<- df.CM

index.4vars   <- d$ANCz +  d$PNCz +   d$SBAz - d$MMRz
index.4vars.z <- as.numeric(scale(  index.4vars, center = TRUE, scale = TRUE)  )
 
index.3vars   <- d$ANCz +  d$PNCz +   d$SBAz  
index.3vars.z <- as.numeric(scale(  index.3vars, center = TRUE, scale = TRUE)  )

index.MMR.z     <- d$MMRz  

d<- data.frame(index.3vars.z, index.4vars.z, index.MMR.z )

ggpairs(d)

index.4vars.qrt <- as.integer(gtools::quantcut(index.4vars.z))
index.3vars.qrt <- as.integer(gtools::quantcut(index.3vars.z))
index.MMR.qrt   <- as.integer(gtools::quantcut(index.MMR.z)) 

# Create DataFrame with index and source vars

df.index                <-  dplyr::select(df,  ID, MMR, ANC, SBA, PNC)
df.index                <-  df.index  [complete.cases(df.index  ), ]

df.index['index.4vars']     <-  index.4vars.z
df.index['index.3vars']     <-  index.3vars.z
df.index['index.MMR']       <-  index.MMR.z
df.index['index.4vars.qrt'] <-  index.4vars.qrt
df.index['index.3vars.qrt'] <-  index.3vars.qrt
df.index['index.MMR.qrt']   <-  index.MMR.qrt

dplyr::filter(df.index,  index.4vars.qrt ==1)$ID
dplyr::filter(df.index,  index.4vars.qrt ==2)$ID 
dplyr::filter(df.index,  index.4vars.qrt ==3)$ID
dplyr::filter(df.index,  index.4vars.qrt ==4)$ID

#vars
# ID MMR ANC SBA PNC index.4vars index.3vars index.MMR index.4vars.qrt index.3vars.qrt index.MMR.qrt 
# ID index.MMR.qrt MMR ANC SBA PNC  

df.index2 <-  df.index %>% 
              dplyr::select(ID, index.MMR.qrt, MMR, ANC, SBA, PNC ) %>%
              dplyr::arrange( index.MMR.qrt    )

# export data and index

write.xlsx(df.index2, "ghana.index.by.Region.xlsx")



