# Preliminaries ----
options("scipen" = 100, "digits" = "4")
rm(list=ls())
library(tidyverse)
library(readr)
library(xts)
library(stringr)
library(rebus)
library(PNWColors)


# Importing ----

KZN <- read_csv("./Data/KZN/KZN-20200327.csv")


# Initial inspection ----
str(KZN)
head(KZN)
tail(KZN)
summary(KZN)

# Getting Column Keyes ----

keys <- list("Econ_class" = unique(KZN$Column_Label_Level_2_Econ_Class),
"Voted" = unique(KZN$Row_Label_Level_1_Voted),
"Cluster" = unique(KZN$Row_Label_Level_2_Cluster),
"Department" = unique(KZN$Row_Label_Level_3_Department),
"Item_level_2" = unique(KZN$Row_Label_Item_Level_2),
"Item_level_3" =unique(KZN$Row_Label_Item_Level_3),
"Item_level_4" = unique(KZN$Row_Label_Item_Level_4),
"Item_level_5" = unique(KZN$Row_Label_Item_Level_5),
"Item_level_lowest" = unique(KZN$Row_Label_Item_Lowest_Level)
)


# Tax receipts ----
    #filtering
KZN_tax_receipts <- KZN %>% filter(Row_Label_Item_Level_2 == "TAX RECEIPTS") %>%
                        select(-Report_Filter_Fund, -(Column_Label_Level_2_Econ_Class:Row_Label_Level_2_Cluster), -Row_Label_Item_Level_2, -(Row_Label_Item_Level_4:Row_Label_Item_Lowest_Level))   
    # Names
names(KZN_tax_receipts) <- c("Date", "Department", "Receipts", "Receipts Source")

    # Dates
date_character <- data.frame( map_chr(KZN_tax_receipts$Date, as.character), stringsAsFactors = FALSE)
KZN_tax_receipts$Date<- date_character$map_chr.KZN_tax_receipts.Date..as.character.
KZN_tax_receipts$Date <- as.yearmon(KZN_tax_receipts$Date, "%Y%m")

    # Title case
KZN_tax_receipts$`Receipts Source` <- str_to_title(KZN_tax_receipts$`Receipts Source`)

  # Receipts to positives

KZN_tax_receipts <- KZN_tax_receipts %>% mutate(Receipts = if_else(Receipts < 0,  Receipts * -1,  Receipts * -1))

 # Removing vote strings

vote_string <- one_or_more(WRD) %R% SPC %R% one_or_more(DGT) %R% char_class(":")
KZN_tax_receipts$Department <-  KZN_tax_receipts$Department %>%  str_remove_all(vote_string)



#Initial exporting ----

write.csv(KZN_tax_receipts, "KZN Receipts.csv", row.names = FALSE)





