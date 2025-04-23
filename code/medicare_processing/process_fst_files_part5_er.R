# this code will take all processed years and load them
# calculate rates then match up SSA codings with FIPS

#edited from RMP 6/20 by SA 
#edited from SA 5/17/23 by SV 


# expand grid of stuff temporary
years = c(2006:2016)

# PREPARE ADMISSIONS FILES

# load processed admissions file
dir.input.local = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/in_progress/')
dat.admissions = readRDS(paste0(dir.input.local,'medicare_admissions_er_',years[1],'_',years[length(years)],'.rds'))

rownames(dat.admissions)=1:nrow(dat.admissions)

print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

# attach SSA state codes 
state.names = read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/ssa_standard_state_code_lookup.csv')
state.names = state.names[,c(1:2,4)]
dat.admissions = merge(dat.admissions,state.names,by.x='SSA_STATE_CD',by.y='code',all.x=TRUE)

#we are not restricted to coastal storm states so no need to keep this variable 
drops1 <- c("coastal_storm_state")
dat.admissions = dat.admissions[,!names(dat.admissions) %in% drops1]

print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

# PREPARE DENOM FILES

# load processed denom file
years = c(2006:2016)
dir.input = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/in_progress_denom_FFS/')
dat.denom = readRDS(paste0(dir.input,'medicare_denom_',years[1],'_',years[length(years)],'.rds'))
rownames(dat.denom)=1:nrow(dat.denom)
dat.denom$fips = paste0('0',dat.denom$fips)
dat.denom$fips = substr(dat.denom$fips, nchar(dat.denom$fips)-5+1, nchar(dat.denom$fips))
dat.denom$state.fips = substr(dat.denom$fips, 1,2)

#attach state recognized fips codes 
fips.lookup <- read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/state_fips_lookup.csv')
#remove Hawaii (Alaska has wildfires)
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(15)),]
#remove third column with two letter state abbreviations 
fips.lookup = fips.lookup[,c(1:2)]
dat.denom$state.fips = as.numeric(dat.denom$state.fips)
dat.denom = merge(dat.denom,fips.lookup,by.x='state.fips',by.y='fips',all.x=TRUE)
dat.denom$full_name = as.character(dat.denom$full_name)
dat.denom$full_name[is.na(dat.denom$full_name)==TRUE] <- 'Died within year'

# attach SSA state codes 
state.names = read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/ssa_standard_state_code_lookup.csv')
state.names = state.names[,c(2:4)]
dat.denom = merge(dat.denom,state.names,by.x='full_name',by.y='name',all.x=TRUE)
#we don't need these variables because we are not isolating coastal states
drops2 <- c("state","coastal_storm_state")
dat.denom = dat.denom[,!names(dat.denom) %in% drops2]

head(dat.denom)

# 1. REMOVE SSA CODES CASES NOT IN FIPS DIRECTORY



# make all SSD_CNTY_CD three characters long, adding zeroes at the beginning of values that aren't three characters long
dat.admissions$SSA_CNTY_CD = paste0('00',dat.admissions$SSA_CNTY_CD)
dat.admissions$SSA_CNTY_CD = substr(dat.admissions$SSA_CNTY_CD, nchar(dat.admissions$SSA_CNTY_CD)-3+1, nchar(dat.admissions$SSA_CNTY_CD))

# make unique ssa state county code to merge with fips lookup
dat.admissions$ssacounty = with(dat.admissions,paste0(SSA_STATE_CD,SSA_CNTY_CD))
dat.admissions$ssacounty = as.numeric(dat.admissions$ssacounty)

# ssa fips lookup
fips.ssa.lookup = read.csv('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data/ssa_fips_state_county2017/ssa_fips_state_county2017.csv')
fips.ssa.lookup = fips.ssa.lookup[,c(1:4)]

# merge admissions file with ssa lookup file
dat.admissions = merge(dat.admissions,fips.ssa.lookup,by.x='ssacounty',by.y='ssacounty',all.x=TRUE)

# remove cases with SSA county codes that are not in the ssa fips lookup 
dat.admissions = na.omit(dat.admissions)

print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

head(dat.admissions)

# 2. REMOVE 999 FIPS CODES
## 999 probably indicates the county is unknown county or a PO box or some missing information 
dat.admissions = subset(dat.admissions,SSA_CNTY_CD!=999)

print(paste0('Total number of cases is currently ',sum(dat.admissions$cases)))

# 3. MERGE CASES AND DENOM FILES

dat.denom$fips = as.numeric(dat.denom$fips)
dat.denom = dat.denom[,c('year','fips','population')]
dat.merged = merge(dat.admissions, dat.denom[,c('year','fips','population')], by.x=c('year','fipscounty'),by.y=c('year','fips'),all.x=TRUE)

# only take years from beginning of actual intended dataset
# years in study
years = c(2006:2016)
dat.merged = subset(dat.merged,year%in%years)

print(paste0('Total number of cases is currently ',sum(dat.merged$cases)))

# Look at rows which are matched and which ones are missing and why
dat.merged.not.na = dat.merged[rowSums(is.na(dat.merged))==0,]
dat.merged.na = dat.merged[rowSums(is.na(dat.merged))>0,]

dat.merged.na$fipscounty = paste0('0',dat.merged.na$fipscounty)
dat.merged.na$fipscounty = substr(dat.merged.na$fipscounty, nchar(dat.merged.na$fipscounty)-5+1, nchar(dat.merged.na$fipscounty))
dat.merged.na$state.fips = substr(dat.merged.na$fipscounty, 1,2)

#We obtain the following list: 02230, 13007, 13037, 13061, 13087, 13099, 13131, 13201, 13239, 13243, 13253, 13273, 51595
#print('Unmatched county FIPS codes')
#print(unique(dat.merged.na$fipscounty))



print(paste0('Total number of cases is currently ',sum(dat.merged.not.na$cases)))

library(plyr)
# summary of all and missing values
dat.merged.summary = ddply(dat.merged,.(year,fipscounty),summarise,cases=sum(cases))
print("dat.merged.summary")
head(dat.merged.summary)
dat.merged.na.summary = ddply(dat.merged.na,.(year,fipscounty),summarise,cases=sum(cases))

# 4. EXCLUDE REMAINING (HOPEFULLY TEMPORARY) POST-MERGE PROBLEMS (CURRENTLY IN GEORGIA AND VIRGINIA) 
# This won't affect WF since we are only concerned about West US
#For county level, it is fine to remove all problematic ones 
# examine the problematic Virginia counties in isolation (FIPS 51595 and 51081)
#51595 was collapsed into 51081 (https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013710)
#was supposed to fix FIPS 51595 to go into FIPS 51081 but not clear if this was ever done 
virginia_excluded_counties = c(51595,51081)
dat.merged.summary.virginia = subset(dat.merged.summary,fipscounty%in%virginia_excluded_counties)
dat.denom.virginia = subset(dat.denom,fips%in%virginia_excluded_counties)
dat.denom.virginia = dat.denom.virginia[order(dat.denom.virginia$year),]

# summary of count of denom files per year and per FIPS
dat.denom.summary = ddply(dat.denom,.(fips),nrow)
print('Following FIPS codes have less than complete records')
#Looking for FIPS codes that aren't present in all of the years (a row for each FIPS code and how many times they appear)
print(subset(dat.denom.summary,V1<length(years)))

#We obtain the following list: 2105, 2195, 2198, 2230, 2232, 2275, 13007, 13037, 13061, 13087, 13099, 
# 13131, 13201, 13239, 13243, 13253, 13273, 68020, NA 
# The 13's are Georgia and are missing in early denominator files 1999-2001 (RMP)
# The 2's are Alaska (https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes1990-present.pdf)
# 68020 is Marshall Islands and NA probably just means the county wasn't listed for whatever reason 

# temporarily get rid of Georgia and Virginia FIPS codes which are missing in denom file for 1999-2001

dat.merged = subset(dat.merged,!(fipscounty %in% virginia_excluded_counties))

#rewrote from RMP for consistency with virginia counties 
georgia_excluded_counties = c(13007, 13037, 13061, 13087, 13099,13131, 13201, 13239, 13243, 13253, 13273)
dat.merged = subset(dat.merged,!(fipscounty %in% georgia_excluded_counties & year %in% c(2000:2001)))

#remove Alaska unmatched county 
alaska_excluded_counties = c(02230)
dat.merged = subset(dat.merged, !(fipscounty %in% alaska_excluded_counties))

#alaska_excluded_counties1 = c(2105, 2195, 2198, 2230, 2275)
#alaska_excluded_counties2 = c(2232)

#this must not be the correct years because they don't appear at all anymore except for 2230 -- not worry about 
#dat.merged = subset(dat.merged, !(fipscounty %in% alaska_excluded_counties1 & year %in% c(2000:2008)))
#dat.merged = subset(dat.merged, !(fipscounty %in% alaska_excluded_counties2 & year %in% c(2000:2004)))

#some confusion over what RMP removed and what he didn't, but assuming we will not be following this protocol for zipcodes, it doesn't matter 

#This code works but I think we don't have to worry about removing because they were 'matched' (?)
#misc_excluded_counties = c(68020, 'NA')
#dat.merged = subset(dat.merged, !(fipscounty %in% misc_excluded_counties))


#Also note that dat.merged includes fips codes from Puerto Rico 
#print(unique(dat.merged$fipscounty))

# are any of the prevalences greater than 1? Not currently phew
dat.merged$rate = with(dat.merged,cases/population)
dat.merged.not.na$rate = with(dat.merged.not.na,cases/population)

print(paste0('Total number of cases is currently ',sum(dat.merged$cases)))

print("dat.merged")
head(dat.merged)

# save processed rates file
dir.output.local = paste0('/n/dominici_nsaph_l3/Lab/projects/wildfires-hospitalizations-glm/medicare_processing/data_er/')  
ifelse(!dir.exists(dir.output.local), dir.create(dir.output.local, recursive=TRUE), FALSE)
saveRDS(dat.merged, paste0(dir.output.local,'medicare_rates_',years[1],'_',years[length(years)],'_FFS.rds'))





