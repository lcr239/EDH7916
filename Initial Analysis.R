## -----------------------------------------------------------------------------
##
##' [PROJ: Final Project]
##' [FILE: Initial Analysis]
##' [INIT: 3/24/24]
##' [AUTH: Lindsey Ragsdale] 
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' libraries 
## ---------------------------

library(tidyverse)
library(haven)
library(dtplyr)
library(ggplot2)
library(knitr)
library(patchwork)

## ---------------------------
##' [Input]

getwd()

df<-read_csv(file.path("data", "gr2022_pell_ssl.csv"))

#Select columns to work with. Best practice to keep UNITID, then compare
df_pvt <- df |>
  select(UNITID, PSGRTYPE, PGADJCT, NRADJCT, PGCMTOT, TTCMTOT) 

#filter out 2 and 3 from PSGRTYPE
df_pvt <- df_pvt |>
  filter(PSGRTYPE %in% c(1, 4))

#create percentages:
#1. Pell graduates/ total *100 = PGCMTOT/(PGADJCT+NRADJCT) *100
#2. Total graduates/ total *100 = TTCMTOT/(PGADJCT+NRADJCT) *100

df_pvt <- df_pvt |>
  mutate(Pell_percent = (PGCMTOT / (PGADJCT + NRADJCT)) * 100)

print(df_pvt)

df_pvt <- df_pvt |>
  mutate(NonPell_percent = (TTCMTOT / (PGADJCT + NRADJCT)) * 100)

print (df_pvt)

  
 
## ---------------------------
#Histogram showing Pell v. Non-Pell graduates 
df_percent <- df_pvt |> 
  pivot_longer(cols = c(Pell_percent, NonPell_percent),
               names_to = "percentage_type",
               values_to = "percentage_value")
ggplot(df_percent) +
  geom_histogram(aes(x = percentage_value,
                     fill = percentage_type),
                 alpha = 0.5,
                 color = "black")

    labs(title = "Percentage of Pell Grant and Non-Pell Grant Graduates",
         subtitle = NULL,
         caption = NULL,
         x = "Percentage",
         y = "Fequency") +
    theme_minimal()
  
    
    
## ---------------------------
#Box plot
df_box <- df_pvt |> 
      pivot_longer(cols = c(Pell_percent, NonPell_percent),
                   names_to = "percentage_type",
                   values_to = "percentage_value")  
    
ggplot(df_box)+
  geom_boxplot(mapping= aes(x= percentage_type,
                            y= percentage_value,
                            fill=percentage_type ), 
               alpha = 0.66) 

    
## ---------------------------
##
#Scatter Plot
ggplot(data = df_pvt,
       mapping = aes(x = Pell_percent,
                     y = NonPell_percent)) +
  geom_point() +
  labs(title = "Scatter Plot of Pell Graduates vs Total Graduates",
       x = "Pell Graduates Percentage",
       y = "Non-Pell Graduates Percentage")




## ---------------------------

## ---------------------------
##' [Analysis]
## ---------------------------

## ---------------------------
##' [Output]
## ---------------------------

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------