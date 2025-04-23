# This script checks category codes

library(tidyverse) 


# load the Bobb coding file and match up the codes (from paper https://www.ncbi.nlm.nih.gov/pubmed/25536257)
# downloaded from https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp#examples
setwd('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/Single_Level_CCS_2015/')
code.lookup = read.csv('$dxref 2015.csv',skip=1)
names(code.lookup) = c('icd9','ccs_category','ccs_category_description','icd9_code_description')
code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)

# get rid of all the weird quotation marks in look-up file
for(i in seq(dim(code.lookup)[2])){
  code.lookup[,i] = gsub("'","",code.lookup[,i])
  code.lookup[,i] = as.character(code.lookup[,i])
}

code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)



if(year%in%c(2015:2020)){
  # load crosswalk
  ccs_codes = readRDS('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/ccs_icd_crosswalk/ccs_icd_crosswalk.rds')
}

setwd('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/')
code.lookup2 = readRDS('CCS_DX.rds')

# filter to ICD10 codes
code.lookup2_10 <- code.lookup2 %>% filter(vocabulary_id == "ICD10CM")


if(year >= 2015){
  setwd('/n/dominici_nsaph_l3/projects/floods-hospitalizations-glm/medicare_processing/data/Single_Level_CCS_2015/')
  code.lookup = read.csv('$dxref 2015.csv',skip=1)
  names(code.lookup) = c('icd9','ccs_category','ccs_category_description','icd9_code_description')
  code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)
  
  # get rid of all the weird quotation marks in look-up file
  for(i in seq(dim(code.lookup)[2])){
    code.lookup[,i] = gsub("'","",code.lookup[,i])
    code.lookup[,i] = as.character(code.lookup[,i])
  }
  
  code.lookup$icd9 = trimws(code.lookup$icd9) ; code.lookup$icd9 = trimws(code.lookup$icd9)
}

#match up the cause of hospitalization groupings with the DIAG1 coding
test = merge(dat_admissions_sum,code.lookup,by.x=c('DIAG1'),by.y=c('icd9'),all.x=TRUE)
