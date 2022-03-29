#################################################################################
# Purpose: Transform formatted USGS water use data into the appropriate "All_use"
# structure for use with Sankey code and other visualizations.

# Last updated: 12 May 2021 by Sophia Bryson

#################################################################################

#load libraries
library(readxl); #useful for reading excel files if you have some
library(rstudioapi); #I think you need this to run R on Duke folders
library(tidyverse); #great for manipulating data
library(dplyr); #needed for filter function
library(tidyr);
library(sf); #used to transform and work with data similar to dataframes (piping)

source_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(source_path))

##################################### DATA #####################################

#Pull in cleaned, formatted data from excel workbook
   y1985 <- read_excel("AllYearsFormatted.xlsx", sheet = 1)
   y1990 <- read_excel("AllYearsFormatted.xlsx", sheet = 2)  
   y1995 <- read_excel("AllYearsFormatted.xlsx", sheet = 3) 
   y2000 <- read_excel("AllYearsFormatted.xlsx", sheet = 4) 
   y2005 <- read_excel("AllYearsFormatted.xlsx", sheet = 5) 
   y2010 <- read_excel("AllYearsFormatted.xlsx", sheet = 6)
   y2015 <- read_excel("AllYearsFormatted.xlsx", sheet = 7) 
   
   #Add future years to Excel in proper format and replicate code chunks to add to dataset. 

##################################### CODE #####################################
   
#Desired Output Structure:
   #Have County, Name, State, District, Division, Year, and Total Population for each record.
   #Want to extract Type and Category from column heads and MGD from row values. 
      # 1. Use pivot_longer() to pull and separate column headings into rows for
      #    category and type. Retain values in MGD column. 

   # Final dataframe structure:   
      UseCol <- c("County", "Name", "State", "District", "Division", "Year", "TotalPop",
                  "Public_Pop", "Type", "Category", "MGD")
      AllUse <- as.data.frame(matrix(nrow = 0, ncol = 11)) 
      colnames(AllUse) <- UseCol
      
      
      #Category
      Public <- c("Public-SW", "Public-GW", "Public-Total")
      Domestic <- c("Domestic-SW", "Domestic-GW", "Domestic-Total")
      Industrial <- c("Industrial-SW", "Industrial-GW", "Industrial-Total")
      Mining <- c("Mining-SW", "Mining-GW", "Mining-Total")
      Irrigation <- c("Irrigation-SW", "Irrigation-GW", "Irrigation-Total")
      Power <- c("Power-SW", "Power-GW", "Power-Total")
      Livestock <- c("Livestock-SW", "Livestock-GW", "Livestock-Total")
      TotalCat <- c("Total-SW", "Total-GW", "Total-Total")
      
      #Type
      SW <- c("Public-SW", "Domestic-SW", "Industrial-SW", "Mining-SW", "Irrigation-SW", "Power-SW", "Livestock-SW", "Total-SW")
      GW <- c("Public-GW", "Domestic-GW", "Industrial-GW", "Mining-GW", "Irrigation-GW", "Power-GW", "Livestock-GW", "Total-GW")
      TotalType <- c("Public-Total", "Domestic-Total", "Industrial-Total", "Mining-Total", "Irrigation-Total", "Power-Total", "Livestock-Total", "Total-Total")
      
      #All category-type combinations
      AllComb <- c(Public, Domestic, Industrial, Mining, Irrigation, Power, Livestock, TotalCat)

      
      #Pivots - pivot_longer() converts columns to rows. Do for each year, then combine. 
         #Want to gather all columns related to type/ category of water into rows ('names')
         #Retain associated values as MGD ('values')
         #Change Public-Pop to Public_Pop to match 'AllUse'
      
         piv1985 <- pivot_longer(data = y1985, cols = AllComb, names_to = c("Category", "Type"), 
                              names_sep = "-", values_to = "MGD")
            
         
         piv1990 <- pivot_longer(data = y1990, cols = AllComb, names_to = c("Category", "Type"), 
                                 names_sep = "-", values_to = "MGD")
            
         
         piv1995 <- pivot_longer(data = y1995, cols = AllComb, names_to = c("Category", "Type"), 
                                 names_sep = "-", values_to = "MGD")

         
         piv2000 <- pivot_longer(data = y2000, cols = AllComb, names_to = c("Category", "Type"), 
                                 names_sep = "-", values_to = "MGD")

         
         piv2005 <- pivot_longer(data = y2005, cols = AllComb, names_to = c("Category", "Type"), 
                                 names_sep = "-", values_to = "MGD")

         
         piv2010 <- pivot_longer(data = y2010, cols = AllComb, names_to = c("Category", "Type"), 
                                 names_sep = "-", values_to = "MGD")

         piv2015 <- pivot_longer(data = y2015, cols = AllComb, names_to = c("Category", "Type"), 
                                   names_sep = "-", values_to = "MGD")

         
   # Merge pivots into final dataframe. Get column headings right and in correct order. 
   Compile <- rbind(AllUse, piv1985) %>% rbind(piv1990) %>% rbind(piv1995) %>% rbind(
               piv2000) %>% rbind(piv2005) %>% rbind(piv2010) %>% rbind(piv2015) %>% rename(
               Public_Pop = 'Public-Pop') %>% relocate(Category, .after= Type)
                                                                       
   #Save out
   write.csv(Compile, "UpdatedAllUse.csv")
      #Once checked, this should replace 'All_use.csv' for integration with other
      #visualization code. 