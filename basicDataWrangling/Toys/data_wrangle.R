#load packages
install.packages("readxl")
library(readxl)
library(tidyr)
library(dplyr)

#import file and turn into local data frame
toy_df<-read_excel("refine_original.xlsx")
toy<-tibble::as.tibble(toy_df)
toy

#find the distinct spellings of the brand names. Re-run to double check.
toy%>%
  distinct(company)
  
#corrects mispellings and creates lowercase versions
toy$company<-gsub("Phillips|phillips|phillipS|phllips|phillps|phlips|fillips", "philips", toy$company, ignore.case = FALSE )   
toy$company<-gsub("Akzo|AKZO|akz0|ak zo","akzo", toy$company, ignore.case = FALSE )   
toy$company<-gsub("Van Houten|van Houten", "van houten", toy$company, ignore.case = FALSE )   
toy$company<-gsub("unilver|Unilever", "philips", toy$company, ignore.case = FALSE ) 

#separate product code and number
?separate
toy<-toy%>%
  separate(`Product code / number`,c("product_code", "product_number"), sep="-")

#create product category column
toy<-toy%>%
  mutate(product_category=product_code)

toy$product_category<-gsub("p","Smartphone",toy$product_category,ignore.case = F)
toy$product_category<-gsub("v","TV",toy$product_category,ignore.case = F)
toy$product_category<-gsub("x","Laptop",toy$product_category,ignore.case = F)
toy$product_category<-gsub("q","Tablet",toy$product_category,ignore.case = F)

#reorder the columns for easier reading
toy<-toy[c(1:3,8,4:7)]

#add full address for geocoding
toy<-toy %>%
  unite(full_address, address, city, country, sep = ",", remove=T)

#add in company binaries
toy<-toy %>%
  mutate(company_philips= if_else(grepl("philips",company),1,0)) %>%
  mutate(company_akzo= if_else(grepl("akzo",company),1,0)) %>%
  mutate(company_van_houten= if_else(grepl("van_houten",company),1,0)) %>%
  mutate(company_unilever= if_else(grepl("unilever",company),1,0))

#add in product binaries
toy<-toy %>%
  mutate(product_smartphone= if_else(grepl("Smartphone", product_category),1,0)) %>%
  mutate(product_tv= if_else(grepl("TV", product_category),1,0)) %>%
  mutate(product_laptop= if_else(grepl("Laptop", product_category),1,0)) %>%
  mutate(product_tablet= if_else(grepl("Tablet", product_category),1,0))

#check to see how data looks
glimpse(toy)

#save as .csv
write.csv(toy, file="refine_clean.csv")
