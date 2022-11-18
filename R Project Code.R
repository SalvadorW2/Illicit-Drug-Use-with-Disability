### Importing Libraries & Data ###

library(gmodels)
library(dplyr)
library(table1)
library(rgdal)
library(glmtoolbox)
library(RSQLite)
library(sqldf)
library(zipcodeR)
library(stringi)
library(maps)
library(maptools)
library(caTools)
library(pROC)
library(gtsummary)
library(DescTools)
library(starter)
library(broom)
library(mctest)
library(ggplot2)
library(gridExtra)
library(sjPlot)
library(sjlabelled)
library(sjmisc)

# Importing relevant variables as .csv

disability = read.csv("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU\\R\\R Project\\R Project Data - Cleaned.csv")

### Data Cleaning ###

# Sorting Strings Alphabetically

disabilityNew = unname(sapply(disability$Disability, function(disability) {
  paste(sort(trimws(strsplit(disability[1], ',')[[1]])), collapse=',')} ))

drugTypeNew = unname(sapply(disability$Drug_Type, function(disability) {
  paste(sort(trimws(strsplit(disability[1], ',')[[1]])), collapse=',')} ))

healthInsnNew = unname(sapply(disability$Health_Ins, function(disability) {
  paste(sort(trimws(strsplit(disability[1], ',')[[1]])), collapse=',')} ))
### Source: https://stackoverflow.com/questions/47337732/sort-a-string-of-comma-separated-items-alphabetically

# Adding Sorted Columns to Data Frame

disabilitySorted = cbind(disability, disabilityNew, drugTypeNew, healthInsnNew)

## Removing Unnecessary Variables

disabilityReplace = subset(disabilitySorted, select = -c(Disability, Drug_Type, Health_Ins))


# Marking missing values with "NA"

disabilityClean = disabilityReplace %>% 
  mutate_all(na_if, "")

# Creating Categorical Variables

## Race

disabilityClean$raceNew = ifelse(disabilityClean$Ethnicity == "Hispanic or Latino", "Hispanic",
                          ifelse(disabilityClean$Race == "White", "White",
                          ifelse(disabilityClean$Race == "Black or African American", "Black",
                          ifelse(disabilityClean$Race == "Asian" | disabilityClean$Race == "Other", "Other",
                                 "Other"))))

## Employment

disabilityClean$employment = ifelse(disabilityClean$Employed == "Disabled, not able to work", "Disabled",
                             ifelse(disabilityClean$Employed ==
                                             "Employed, working 1-39 hours per week", "Employed: 1-39 hours/week",
                             ifelse(disabilityClean$Employed ==
                                                    "Employed, working 40 hours per week", "Employed: 40 hours/week",
                             ifelse(disabilityClean$Employed == "Not employed, looking for work" |
                             disabilityClean$Employed == "Not employed, not looking for work", "Not Employed",
                             ifelse(disabilityClean$Employed == "Retired", "Retired",
                             ifelse(disabilityClean$Employed == "Student", "Student", "Missing"))))))

head(disabilityClean$employment)

## Education

disabilityClean$education = ifelse(disabilityClean$Education == "High School" |
                                     disabilityClean$Education == "Some High School", "High School or Lower",
                            ifelse(disabilityClean$Education == "Some College", "Some College",
                            ifelse(disabilityClean$Education == "Bachelor's", "Bachelor's",
                            ifelse(disabilityClean$Education == "Master's" |
                            disabilityClean$Education == "Dctoral Degree" |
                            disabilityClean$Education == "Professional Degree", "Advanced Degree", "Other"))))

## Insurance

disabilityClean$hasInsurance = ifelse(is.na(disabilityClean$healthInsnNew), "No", "Yes")

## Disability

disabilityClean$hasDisability = ifelse(is.na(disabilityClean$disabilityNew), "No", "Yes")
disabilityClean$hasAllergies = ifelse(grepl("Allergies", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")
disabilityClean$hasBadBack = ifelse(grepl("Back", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")
disabilityClean$hasBoneProblems = ifelse(grepl("Bone or Joint", disabilityClean$disabilityNew, fixed = TRUE),
                                         "Yes", "No")
disabilityClean$hasHeartProblems = ifelse(grepl("Heart", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")
disabilityClean$hasEyeProblems = ifelse(grepl("Eyes", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")
disabilityClean$hasEarProblems = ifelse(grepl("Ears or Language", disabilityClean$disabilityNew, fixed = TRUE),
                                        "Yes", "No")
disabilityClean$hasLimbProblems = ifelse(grepl("Limbs", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")
disabilityClean$hasLungProblems = ifelse(grepl("Lung", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")
disabilityClean$hasNeuroProblems = ifelse(grepl("Neuro", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")
disabilityClean$hasOtherProblems = ifelse(grepl("Other", disabilityClean$disabilityNew, fixed = TRUE), "Yes", "No")

## Drugs

disabilityClean$usesDrugs = ifelse(is.na(disabilityClean$drugTypeNew), "No", "Yes")
disabilityClean$usesCoca = ifelse(grepl("Coca", disabilityClean$drugTypeNew, fixed = TRUE), "Yes", "No")
disabilityClean$usesHallucinogens = ifelse(grepl("Hallucinogens", disabilityClean$drugTypeNew, fixed = TRUE),
                                           "Yes", "No")
disabilityClean$usesHeroin = ifelse(grepl("Heroin", disabilityClean$drugTypeNew, fixed = TRUE), "Yes", "No")
disabilityClean$usesTHC = ifelse(grepl("THC", disabilityClean$drugTypeNew, fixed = TRUE), "Yes", "No")
disabilityClean$usesPainkillers = ifelse(grepl("Painkillers", disabilityClean$drugTypeNew, fixed = TRUE),
                                         "Yes", "No")
disabilityClean$usesSedatives = ifelse(grepl("Sedatives", disabilityClean$drugTypeNew, fixed = TRUE), "Yes", "No")
disabilityClean$usesStimulants = ifelse(grepl("Stimulants", disabilityClean$drugTypeNew, fixed = TRUE), "Yes", "No")
disabilityClean$usesTranquilizers = ifelse(grepl("Tranquilizers", disabilityClean$drugTypeNew, fixed = TRUE),
                                           "Yes", "No")
disabilityClean$usesInhalents = ifelse(grepl("Inhalents or Solvents", disabilityClean$drugTypeNew,
                                             fixed = TRUE), "Yes", "No")
disabilityClean$usesOther = ifelse(grepl("Other", disabilityClean$drugTypeNew, fixed = TRUE), "Yes", "No")

## Removing Unwanted Values

disabilityClean = filter(disabilityClean, Sex != "Other")
disabilityClean = filter(disabilityClean, Sex != "Prefer not to say")
disabilityClean = filter(disabilityClean, employment != "Missing")
disabilityClean = filter(disabilityClean, Income_2021 != "Don't know or prefer not to say")
disabilityClean = filter(disabilityClean, Depression != "Missing")
disabilityClean = filter(disabilityClean, Anxiety != "Missing")
disabilityClean = filter(disabilityClean, Birth_Year > 1900)
disabilityClean = filter(disabilityClean, Birth_Year < 2022)

### Dropping Unused Columns ###

disabilityClean = subset(disabilityClean, select = -c(Disability_2, Most_Serious2, Onset_Age, Dementia,
                                                      Vascular_Disease, Diabetes, Gastro_Disease, Obesity,
                                                      Diabetes_Type, Diabetes_Type2, Birth_Year, Ethnicity,
                                                      Race, Education, Employed))

### table1 ###

## P-value Creation Function

pvalue = function(x, ...) {
  y = unlist(x)
  g = factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p = t.test(y ~ g)$p.value
  } else {
    p = chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}
### Source: https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

## Creating Binary Yes/No Variables

disabilityClean$isMale = ifelse(disabilityClean$Sex == "Male", "Yes", "No")
disabilityClean$isFemale = ifelse(disabilityClean$Sex == "Female", "Yes", "No")

disabilityClean$isWhite = ifelse(disabilityClean$raceNew == "White", "Yes", "No")
disabilityClean$isBlack = ifelse(disabilityClean$raceNew == "Black", "Yes", "No")
disabilityClean$isBlack = ifelse(disabilityClean$raceNew == "Hispanic", "Yes", "No")
disabilityClean$isBlack = ifelse(disabilityClean$raceNew == "Other", "Yes", "No")
disabilityClean$isNotWhite = ifelse(disabilityClean$raceNew != "White", "Yes", "No")

disabilityClean$isUnemployed = ifelse(disabilityClean$employment == "Not Employed", "Yes", "No")

disabilityClean$inPoverty = ifelse(disabilityClean$Income_2021 == "$0 to $14,999" |
                                     disabilityClean$Income_2021 == "$15,000 to $24,999", "Yes", "No")

disabilityClean$collegeDegree = ifelse(disabilityClean$education == "High School or Lower" |
                                         disabilityClean$education == "Some COllege", "No", "Yes")

disabilityClean$hasDepression = ifelse(disabilityClean$Depression == "Don't Have", "No", "Yes")
disabilityClean$hasAnxiety = ifelse(disabilityClean$Anxiety == "Don't Have", "No", "Yes")

## Drug Usage by Demographics

### Creating Labels

label(disabilityClean$usesDrugs) = "Uses Illicit Drugs?"
label(disabilityClean$hasDisability) = "Has Physical Disability"
label(disabilityClean$isMale) = "Is Male"
label(disabilityClean$isWhite) = "Is White"
label(disabilityClean$inPoverty) = "Is in Poverty"
label(disabilityClean$isUnemployed) = "Is Unemployed"
label(disabilityClean$collegeDegree) = "Has College Degree"
label(disabilityClean$hasDepression) = "Has Depression"
label(disabilityClean$hasAnxiety) = "Has Anxiety"

### Creating Table

table1( ~ isMale + isWhite + inPoverty + isUnemployed + collegeDegree + hasDepression + hasAnxiety |
          usesDrugs, data = disabilityClean, extra.col = list("P-value" = pvalue))

## Drug Usage by Disability

### Creating Labels

label(disabilityClean$hasAllergies) = "Allergies"
label(disabilityClean$hasBadBack) = "Back Problems"
label(disabilityClean$hasBoneProblems) = "Bone Problems"
label(disabilityClean$hasHeartProblems) = "Heart Problems"
label(disabilityClean$hasEyeProblems) = "Eye Problems"
label(disabilityClean$hasEarProblems) = "Ear Problems"
label(disabilityClean$hasLimbProblems) = "Limb Loss/Impairment"
label(disabilityClean$hasLungProblems) = "Lung Problems"
label(disabilityClean$hasNeuroProblems) = "Neurological Problems"
label(disabilityClean$hasOtherProblems) = "Other Disability"

### Creating Table

table1( ~ hasAllergies + hasBadBack + hasBoneProblems + hasHeartProblems + hasEyeProblems + hasEarProblems +
          hasLimbProblems + hasLungProblems + hasNeuroProblems +  hasOtherProblems | usesDrugs,
        data = disabilityClean, extra.col = list("P-value" = pvalue)) ### not significant

## Back Problems by Disability Type

### Creating Labels

label(disabilityClean$usesCoca) = "Uses Coca Derivatives"
label(disabilityClean$usesHallucinogens) = "Uses Hallucinogens"
label(disabilityClean$usesTHC) = "Uses THC Products"
label(disabilityClean$usesPainkillers) = "Uses Painkillers"
label(disabilityClean$usesSedatives) = "Uses Sedatives"
label(disabilityClean$usesStimulants) = "Uses Stimulants"
label(disabilityClean$usesTranquilizers) = "Uses Tranquilizers"

### Creating Table

table1( ~ usesCoca + usesHallucinogens + usesTHC + usesPainkillers + usesSedatives +
          usesStimulants + usesTranquilizers | hasBadBack,
        data = disabilityClean, extra.col = list("P-value" = pvalue))


## Creating Binary Variables to Use in Regression

disabilityClean$drugUse = ifelse(disabilityClean$usesDrugs == "Yes", 1, 0)

disabilityClean$isMale = ifelse(disabilityClean$Sex == "Male", 1, 0)
disabilityClean$isFemale = ifelse(disabilityClean$Sex == "Female", 1, 0)

disabilityClean$isWhite = ifelse(disabilityClean$raceNew == "White", 1, 0)
disabilityClean$isBlack = ifelse(disabilityClean$raceNew == "Black", 1, 0)
disabilityClean$isBlack = ifelse(disabilityClean$raceNew == "Hispanic", 1, 0)
disabilityClean$isBlack = ifelse(disabilityClean$raceNew == "Other", 1, 0)
disabilityClean$isNotWhite = ifelse(disabilityClean$raceNew != "White", 1, 0)

disabilityClean$isUnemployed = ifelse(disabilityClean$employment == "Not Employed", 1, 0)

disabilityClean$inPoverty = ifelse(disabilityClean$Income_2021 == "$0 to $14,999" |
                                     disabilityClean$Income_2021 == "$15,000 to $24,999", 1, 0)

disabilityClean$collegeDegree = ifelse(disabilityClean$education == "High School or Lower" |
                                         disabilityClean$education == "Some COllege", 0, 1)

disabilityClean$hasDepression = ifelse(disabilityClean$Depression == "Don't Have", 0, 1)
disabilityClean$hasAnxiety = ifelse(disabilityClean$Anxiety == "Don't Have", 0, 1)

disabilityClean$hasAllergies = ifelse(disabilityClean$hasAllergies == "Yes", 1, 0)
disabilityClean$hasBadBack = ifelse(disabilityClean$hasBadBack == "Yes", 1, 0)
disabilityClean$hasBoneProblems = ifelse(disabilityClean$hasBoneProblems == "Yes", 1, 0)
disabilityClean$hasHeartProblems = ifelse(disabilityClean$hasHeartProblems == "Yes", 1, 0)
disabilityClean$hasEyeProblems = ifelse(disabilityClean$hasEyeProblems == "Yes", 1, 0)
disabilityClean$hasEarProblems = ifelse(disabilityClean$hasEarProblems == "Yes", 1, 0)
disabilityClean$hasLimbProblems = ifelse(disabilityClean$hasLimbProblems == "Yes", 1, 0)
disabilityClean$hasLungProblems = ifelse(disabilityClean$hasLungProblems == "Yes", 1, 0)
disabilityClean$hasNeuroProblems = ifelse(disabilityClean$hasNeuroProblems == "Yes", 1, 0)
disabilityClean$hasOtherProblems = ifelse(disabilityClean$hasOtherProblems == "Yes", 1, 0)

disabilityClean$usesCoca = ifelse(disabilityClean$usesCoca == "Yes", 1, 0)
disabilityClean$usesHallucinogens = ifelse(disabilityClean$usesHallucinogens == "Yes", 1, 0)
disabilityClean$usesTHC = ifelse(disabilityClean$usesTHC == "Yes", 1, 0)
disabilityClean$usesPainkillers = ifelse(disabilityClean$usesPainkillers == "Yes", 1, 0)
disabilityClean$usesSedatives = ifelse(disabilityClean$usesSedatives == "Yes", 1, 0)
disabilityClean$usesStimulants = ifelse(disabilityClean$usesStimulants == "Yes", 1, 0)
disabilityClean$usesTranquilizers = ifelse(disabilityClean$usesTranquilizers == "Yes", 1, 0)

## Function for Getting Counties

findCounty = function(pointsDF) {
  
  counties = map('county', fill = TRUE, col = "transparent", plot = FALSE)
  IDs = sapply(strsplit(counties$names, ':'), function(x) x[1])
  counties_sp = map2SpatialPolygons(counties, IDs = IDs,
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  pointsSP = SpatialPoints(pointsDF, 
                           proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  indices = over(pointsSP, counties_sp)
  
  countyNames = sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}
### Source: https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county

## Cleaning County Data

coordinates = data.frame(longitude = disabilityClean$Longitude, latitude = disabilityClean$Latitude)
disabilityClean$county = findCounty(coordinates)

disabilityClean$state = gsub(",.*$", "", disabilityClean$county)
disabilityClean$county = sub(".*,", "", disabilityClean$county)

## Importing Rural/Urban Counties Data

countiesData = read.csv("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU\\R\\R Project\\ruralurbancodes2013.csv")

## Preparing to Join

disabilityClean$state = stri_trans_general(disabilityClean$state, id = "Title")
disabilityClean$county = stri_trans_general(disabilityClean$county, id = "Title")
disabilityClean$County_Name = paste(disabilityClean$county, "County", sep = ' ')

disabilityClean$stateAbb = ifelse(disabilityClean$state == "Alabama", "AL",
                           ifelse(disabilityClean$state == "Alaska", "AK",
                           ifelse(disabilityClean$state == "Arizona", "AZ",
                           ifelse(disabilityClean$state == "Arkansas", "AR",
                           ifelse(disabilityClean$state == "California", "CA",
                           ifelse(disabilityClean$state == "Colorado", "CO",
                           ifelse(disabilityClean$state == "Connecticut", "CT",
                           ifelse(disabilityClean$state == "Delware", "DE",
                           ifelse(disabilityClean$state == "Florida", "FL",
                           ifelse(disabilityClean$state == "Georgia", "GA",
                           ifelse(disabilityClean$state == "Hawaii", "HW",
                           ifelse(disabilityClean$state == "Idaho", "ID",
                           ifelse(disabilityClean$state == "Illinois", "IL",
                           ifelse(disabilityClean$state == "Indiana", "IN",
                           ifelse(disabilityClean$state == "Iowa", "IA",
                           ifelse(disabilityClean$state == "Kansas", "KS",
                           ifelse(disabilityClean$state == "Kentucky", "KY",
                           ifelse(disabilityClean$state == "Louisiana", "LA",
                           ifelse(disabilityClean$state == "Maine", "ME",
                           ifelse(disabilityClean$state == "Maryland", "MD",
                           ifelse(disabilityClean$state == "Massachusetts", "MA",
                           ifelse(disabilityClean$state == "Michigan", "MI",
                           ifelse(disabilityClean$state == "Minnesota", "MN",
                           ifelse(disabilityClean$state == "Mississippi", "MS",
                           ifelse(disabilityClean$state == "Missouri", "MO",
                           ifelse(disabilityClean$state == "Montana", "MT",
                           ifelse(disabilityClean$state == "Nebraska", "NE",
                           ifelse(disabilityClean$state == "Nevada", "NV",
                           ifelse(disabilityClean$state == "New Hampshire", "NH",
                           ifelse(disabilityClean$state == "New Jersey", "NJ",
                           ifelse(disabilityClean$state == "New Mexico", "NM",
                           ifelse(disabilityClean$state == "New York", "NY",
                           ifelse(disabilityClean$state == "North Carolina", "NC",
                           ifelse(disabilityClean$state == "North Dakota", "ND",
                           ifelse(disabilityClean$state == "Ohio", "OH",
                           ifelse(disabilityClean$state == "Oklahoma", "OK",
                           ifelse(disabilityClean$state == "Oregon", "OR",
                           ifelse(disabilityClean$state == "Pennsylvania", "PA",
                           ifelse(disabilityClean$state == "Rhode Island", "RI",
                           ifelse(disabilityClean$state == "South Carolina", "SC",
                           ifelse(disabilityClean$state == "South Dakota", "SD",
                           ifelse(disabilityClean$state == "Tennessee", "TN",
                           ifelse(disabilityClean$state == "Texas", "TX",
                           ifelse(disabilityClean$state == "Utah", "UT",
                           ifelse(disabilityClean$state == "Vermont", "VT",
                           ifelse(disabilityClean$state == "Virginia", "VA",
                           ifelse(disabilityClean$state == "Washington", "WA",
                           ifelse(disabilityClean$state == "West Virginia", "WV",
                           ifelse(disabilityClean$state == "Wisconsin", "WI",
                           ifelse(disabilityClean$state == "Wyoming", "WY", "Other"
                           ))))))))))))))))))))))))))))))))))))))))))))))))))

## Joining with SQL

disabilityMap = sqldf("SELECT D.*, C.Population_2010, C.RUCC_2013
                       FROM disabilityClean AS D
                       LEFT JOIN countiesData AS C
                         ON D.County_Name = C.County_Name
                           AND D.stateAbb = C.State")

## Distinguishing between Metro and Rural

disabilityMap$isRural = ifelse((disabilityMap$RUCC_2013 == 5 |
                                  disabilityMap$RUCC_2013 == 6 |
                                  disabilityMap$RUCC_2013 == 7), 1, 0)

disabilityMap$isUrban = ifelse((disabilityMap$RUCC_2013 == 1 |
                                  disabilityMap$RUCC_2013 == 2 |
                                  disabilityMap$RUCC_2013 == 3), 1, 0)

## Getting Census Regions

disabilityMap$censusRegion = ifelse(disabilityMap$state == "West Virginia" |
                                      disabilityMap$state == "Maryland" |
                                      disabilityMap$state == "Delaware" |
                                      disabilityMap$state == "Virginia" |
                                      disabilityMap$state == "North Carolina" |
                                      disabilityMap$state == "South Carolina" |
                                      disabilityMap$state == "Georgia" |
                                      disabilityMap$state == "Florida", "Southeastern",
                             ifelse(disabilityMap$state == "New York" |
                                      disabilityMap$state == "Pennsylvania" |
                                      disabilityMap$state == "Maryland" |
                                      disabilityMap$state == "New Jersey", "Middle Atlantic",
                             ifelse(disabilityMap$state == "Maine" |
                                      disabilityMap$state == "Vermont" |
                                      disabilityMap$state == "New Hampshire" |
                                      disabilityMap$state == "Massachusetts" |
                                      disabilityMap$state == "Connecticut" |
                                      disabilityMap$state == "Rhode Island", "New England",
                             ifelse(disabilityMap$state == "Michigan" |
                                      disabilityMap$state == "Wisconsin" |
                                      disabilityMap$state == "Illinois" |
                                      disabilityMap$state == "Indiana" |
                                      disabilityMap$state == "Ohio", "East North Central",
                             ifelse(disabilityMap$state == "Kentucky" |
                                      disabilityMap$state == "Tennessee" |
                                      disabilityMap$state == "Alabama" |
                                      disabilityMap$state == "Mississippi", "East South Central",
                             ifelse(disabilityMap$state == "North Dakota" |
                                      disabilityMap$state == "South Dakota" |
                                      disabilityMap$state == "Minnesota" |
                                      disabilityMap$state == "Iowa" |
                                      disabilityMap$state == "Nebraska" |
                                      disabilityMap$state == "Kansas" |
                                      disabilityMap$state == "Missouri", "West North Central",
                             ifelse(disabilityMap$state == "Oklahoma" |
                                      disabilityMap$state == "Arkansas" |
                                      disabilityMap$state == "Texas" |
                                      disabilityMap$state == "Louisiana", "West South Central",
                             ifelse(disabilityMap$state == "Montana" |
                                      disabilityMap$state == "Wyoming" |
                                      disabilityMap$state == "Idaho" |
                                      disabilityMap$state == "Nevada" |
                                      disabilityMap$state == "Utah" |
                                      disabilityMap$state == "Colorado" |
                                      disabilityMap$state == "Arizona" |
                                      disabilityMap$state == "New Mexico", "Mountain",
                             ifelse(disabilityMap$state == "Washington" |
                                      disabilityMap$state == "Oregon" |
                                      disabilityMap$state == "California" |
                                      disabilityMap$state == "Alaska" |
                                       disabilityMap$state == "Hawaii", "Pacific", NA)))))))))

## Creating Census Binaries

disabilityMap$isSoutheast = ifelse(disabilityMap$censusRegion == "Southeast", "Yes", "No")
disabilityMap$isMiddleAtlantic = ifelse(disabilityMap$censusRegion == "Middle Atlantic", "Yes", "No")
disabilityMap$isNewEngland = ifelse(disabilityMap$censusRegion == "New England", "Yes", "No")
disabilityMap$isEastNorthCentral = ifelse(disabilityMap$censusRegion == "East North Central", "Yes", "No")
disabilityMap$isEastSouthCentral = ifelse(disabilityMap$censusRegion == "East South Central", "Yes", "No")
disabilityMap$isWestNorthCentral = ifelse(disabilityMap$censusRegion == "West North Central", "Yes", "No")
disabilityMap$isWestSouthCentral = ifelse(disabilityMap$censusRegion == "West South Central", "Yes", "No")
disabilityMap$isMountain = ifelse(disabilityMap$censusRegion == "Mountain", "Yes", "No")
disabilityMap$isPacific = ifelse(disabilityMap$censusRegion == "Pacific", "Yes", "No")

disabilityMap$notSoutheastern = ifelse(disabilityMap$censusRegion != "Southeast", 1, 0)

### Geography table1 ###

## Drug Use by Geography

### Creating Labels

label(disabilityMap$isRural) = "Rurality"
label(disabilityMap$isSoutheast) = "Southeast Census Region"
label(disabilityMap$isMiddleAtlantic) = "Middle Atlantic Census Region"
label(disabilityMap$isNewEngland) = "New England Census Region"
label(disabilityMap$isEastNorthCentral) = "East North Central Census Region"
label(disabilityMap$isEastSouthCentral) = "East South Central Region"
label(disabilityMap$isWestNorthCentral) = "West North Central Census Region"
label(disabilityMap$isWestSouthCentral) = "West South Central Census Region"
label(disabilityMap$isMountain) = "Mountain Census Region"
label(disabilityMap$isPacific) = "Pacific Census Region"

### Creating Table

table1( ~ isSoutheast + isMiddleAtlantic + isNewEngland + isEastNorthCentral + isEastSouthCentral +
          isWestNorthCentral + isWestSouthCentral + isMountain + isPacific | usesDrugs,
        data = disabilityMap, extra.col = list("P-value" = pvalue))

## Rurality by Census Region

table1( ~ isSoutheast + isMiddleAtlantic + isNewEngland + isEastNorthCentral + isEastSouthCentral +
          isWestNorthCentral + isWestSouthCentral + isMountain + isPacific | isRural,
        data = disabilityMap, extra.col = list("P-value" = pvalue))

### Logistic Regression ###

## Removing Variables Unnecessary to Model

disabilityFinal = subset(disabilityMap, select = -c(Latitude, Longitude, Drug_Use, Drug_Type2, ZIP,
                                                    drugTypeNew, healthInsnNew, hasInsurance, hasDisability))

# Training Dataset

split = sample.split(disabilityFinal, SplitRatio = 0.8) 

train = subset(disabilityFinal, split == "TRUE") 
test = subset(disabilityFinal, split == "FALSE")
### Source: https://www.projectpro.io/recipes/plot-auc-roc-curve-r

## Executing Regression

drugUseModel = glm(drugUse ~ isMale + isNotWhite + inPoverty + isUnemployed + hasDepression + hasAnxiety,
                   data = disabilityFinal, family = "binomial")

## Regression Summary

summary(drugUseModel)

## ANOVA

anova(drugUseModel, test = "Chisq")

## Prediction

pred_test = predict(drugUseModel, test, type = "response")
### Source: https://www.projectpro.io/recipes/plot-auc-roc-curve-r

## Pearson Residuals

pearsonResiduals = residuals(drugUseModel, "pearson")

## Hosmer-Lemeshow Goodness of Fit Test

hltest(drugUseModel)

## Cook's Distance

plot(drugUseModel, which = 4, id.n = 5)

## VIF's

mc.plot(drugUseModel, Inter = FALSE, vif = 10, ev = 0.01)

### ROC Curve

Cstat(drugUseModel)

test_prob = predict(drugUseModel, test, type = "response")
test_roc = smooth(roc(test$usesDrugs ~ test_prob, plot = TRUE, print.auc = TRUE))
plot.roc(test_roc, identity.col = "black", identity.lwd = 2, col = "blue", lwd = 3, main = "ROC Curve")
as.numeric(test_roc$auc)
### Source: https://www.projectpro.io/recipes/plot-auc-roc-curve-r

### Forest Plot for Regression Output ####

plot_model(drugUseModel, show.values = TRUE, vline.color = "red",
           title = "Forest Plot for Regression Model",
           value.offset = 0.5, line.size = 1.5, dot.size = 3, width = 0.5, colors = "blue")
### Source: https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html


### It appears that drug abuse among those with physical disabilities isn't correlated with any specific type
### of disability, but rather with certain demographic characteristics.

### I was sure it would be correlated with back pain or other disabilities, but it turns out that it isn't.

### Urban vs. rural proved to be insignificant.

#####################################################################################################################





### Creating Printable Output ###

printable = tbl_regression(drugUseModel, label = list(isMale ~ "Is Male",
                                                      isNotWhite ~ "Is Not White",
                                                      inPoverty ~ "In Poverty",
                                                      isUnemployed ~ "Is Unemployed",
                                                      hasDepression ~ "Has Depression",
                                                      hasAnxiety ~ "Has Anxiety"),
                           exponentiate = TRUE, intercept = TRUE)

printable

### Back Problems by Drug Type Regression ###

BadBackModel = glm(hasBadBack ~ usesPainkillers, data = disabilityFinal, family = "binomial")

summary(BadBackModel)

### Drug Usage by Disability Regression ###

drugUseModel2 = glm(drugUse ~ hasAllergies + hasBadBack + hasBoneProblems + hasHeartProblems + hasEyeProblems +
                      hasEarProblems + hasLimbProblems + hasLungProblems + hasNeuroProblems + hasOtherProblems,
                    data = disabilityFinal, family = "binomial")

summary(drugUseModel2)























