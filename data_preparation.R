# Load packages
library(dplyr)

# All Census datasets
# accomodation <- read.csv("data/accomodation_type.csv", skip = 8, nrows = 631)
age_structure <- read.csv("data/age_structure.csv", skip = 8, nrows = 631)
birth_country <- read.csv("data/birth_country.csv", skip = 8, nrows = 631)
cars_availible <- read.csv("data/cars_available.csv", skip = 8, nrows = 631)
economic_activity <- read.csv("data/economic_activity.csv", skip = 8, nrows = 631)
ethnic_group <- read.csv("data/ethnic_group.csv", skip = 8, nrows = 631)
household_language <- read.csv("data/household_language.csv", skip = 8, nrows = 631)
martial_status <- read.csv("data/martial_status.csv", skip = 8, nrows = 631)
qualifications <- read.csv("data/qualifications.csv", skip = 8, nrows = 631)
religion <- read.csv("data/religion.csv", skip = 8, nrows = 631)
tenure <- read.csv("data/tenure.csv", skip = 8, nrows = 631)
sex <- read.csv("data/usual_resident.csv", skip = 8, nrows = 631)


# Data manipulation
age_structure$Age_0_17 <- age_structure$Age.0.to.4 + 
  age_structure$Age.5.to.7 + 
  age_structure$Age.8.to.9 + 
  age_structure$Age.10.to.14 + 
  age_structure$Age.15 + 
  age_structure$Age.16.to.17

age_structure$Age_18_29 <- age_structure$Age.18.to.19 +
  age_structure$Age.20.to.24 +
  age_structure$Age.25.to.29

age_structure$Age_30_44 <- age_structure$Age.30.to.44

age_structure$Age_45_64 <- age_structure$Age.45.to.59 +
  age_structure$Age.60.to.64

age_structure$Age_65_up <- age_structure$Age.65.to.74 +
  age_structure$Age.75.to.84 +
  age_structure$Age.85.to.89 +
  age_structure$Age.90.and.over

age_structure <- age_structure %>% 
  select(., "X2011.output.area", "Age_0_17", "Age_18_29", "Age_30_44", "Age_45_64", "Age_65_up")
colnames(age_structure) <- c("OA", "Age_0_17", "Age_18_29", "Age_30_44", "Age_45_64", "Age_65_up")

birth_country <- birth_country %>% 
  select(., "X2011.output.area", "United.Kingdom", "Ireland", "Other.EU", "Other.countries")
colnames(birth_country) <- c("OA", "united_kingdom", "ireland", "other_eu", "other_countries")

cars_availible$summing <- cars_availible$X3.cars.or.vans.in.household + cars_availible$X4.or.more.cars.or.vans.in.household
cars_availible$mnemonic <- NULL
cars_availible$X3.cars.or.vans.in.household <- NULL
cars_availible$X4.or.more.cars.or.vans.in.household <- NULL
colnames(cars_availible) <- c("OA", "no_cars", "one_car", "two_cars", "three_and_more_cars")

economic_activity <- economic_activity %>% 
  select(., "X2011.output.area", "Economically.active..In.employment", "Economically.active..Unemployed", "Economically.inactive..Retired")
colnames(economic_activity) <- c("OA", "employed", "unemployed", "retired")

ethnic_group <- ethnic_group %>% 
  select(., "X2011.output.area", "White", "Asian.Asian.British", "Black.African.Caribbean.Black.British", "Other.ethnic.group..Arab")
colnames(ethnic_group) <- c("OA", "white", "asian", "black_african", "other_arab")

household_language <- household_language %>% 
  select(., "X2011.output.area", 
         "All.people.aged.16.and.over.in.household.have.English.as.a.main.language..English.or.Welsh.in.Wales.",
         "At.least.one.but.not.all.people.aged.16.and.over.in.household.have.English.as.a.main.language..English.or.Welsh.in.Wales.",
         "No.people.aged.16.and.over.in.household.but.at.least.one.person.aged.3.to.15.has.English.as.a.main.language..English.or.Welsh.in.Wales.", 
         "No.people.in.household.have.English.as.a.main.language..English.or.Welsh.in.Wales.")
colnames(household_language) <- c("OA", "all_english", "notall_english", "kids_english", "no_english")

martial_status <- martial_status %>% 
  select(., "X2011.output.area", 
         "Single..never.married.or.never.registered.a.same.sex.civil.partnership.",
         "Married",
         "Divorced.or.formerly.in.a.same.sex.civil.partnership.which.is.now.legally.dissolved")
colnames(martial_status) <- c("OA", "single", "married", "divorced")

qualifications <- qualifications %>% 
  select(., "X2011.output.area", 
         "Highest.level.of.qualification..Level.1.qualifications",
         "Highest.level.of.qualification..Level.4.qualifications.and.above")
colnames(qualifications) <- c("OA", "lowest_quali", "highest_quali")

religion <- religion %>% 
  select(., "X2011.output.area", 
         "Christian",
         "Jewish",
         "Muslim",
         "No.religion")
colnames(religion) <- c("OA", "christian", "jewish", "muslim", "no_religion")

tenure <- tenure %>% 
  select(., "X2011.output.area", 
         "Owned",
         "Shared.ownership..part.owned.and.part.rented.",
         "Social.rented",
         "Private.rented",
         "Living.rent.free")
colnames(tenure) <- c("OA", "owned", "shared", "social_rent", "private_rent", "rent_free")

sex <- sex %>% 
  select(., "X2011.output.area", 
         "Males",
         "Females")
colnames(sex) <- c("OA", "males", "females")


dfList <- list(age_structure,
               birth_country,
               cars_availible,
               economic_activity,
               ethnic_group,
               household_language,
               martial_status,
               qualifications,
               religion,
               tenure,
               sex)

census_data <- Reduce(function(x, y) merge(x, y, all=TRUE), dfList)

rm(list = c("age_structure",
            "birth_country",
            "cars_availible",
            "economic_activity",
            "ethnic_group",
            "household_language",
            "martial_status",
            "qualifications",
            "religion",
            "tenure",
            "sex",
            "dfList"))


# House price dataset
house_prices <- read.csv("data/house_prices.csv")

# Load house prices and decode location
postcodeRdr <- read.csv("data/NSPL_MAY_2020_UK_SW.csv")
postcodeRdr2 <- read.csv("data/NSPL_MAY_2020_UK_W.csv")
postRdr <- rbind(postcodeRdr, postcodeRdr2)
houses_merged <- house_prices %>% inner_join(postRdr[ , c("oseast1m", "osnrth1m", "pcds", "oa11")], by = c("postcode" = "pcds"))
houses <- houses_merged[,c(1, 2, 5, 6, 7, 32, 33, 34)]



write.csv(census_data, "census_data.csv", row.names=F)
write.csv(houses, "house_data.csv", row.names=F)
