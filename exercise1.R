library(dplyr)
library(tidyr)
library(stringr)

#1: Clean up brand names
refine_original$company <- str_to_lower(refine_original$company)
philips_SpellCheck <- c("phillips", "phllips", "phillps", "fillips", "phlips")
for(sp in philips_SpellCheck){
  refine_original$company <- gsub(sp, "philips", refine_original$company)
}

akzo_SpellCheck <- c("ak zo", "akz0")
for(sp in akzo_SpellCheck){
  refine_original$company <- gsub(sp, "akzo", refine_original$company)
}

refine_original$company <- gsub("unilver", "unilever", refine_original$company)

#2: Separate product code and number
refine_original <- mutate(refine_original, product_code = substr(refine_original$Product.code...number, 1, 1))
refine_original <- mutate(refine_original, product_number = substr(refine_original$Product.code...number, 3, 3))

#3: Add product categories
refine_original <- mutate(refine_original, product_category = product_code)
category_check <- c("p", "v", "x", "q")
for(check in category_check){
  if(check == "p")
  {
    refine_original$product_category <- gsub(check, "Smartphone", refine_original$product_category)
  }
  else if(check == "v")
  {
    refine_original$product_category <- gsub(check, "TV", refine_original$product_category)
  }
  else if(check == "x")
  {
    refine_original$product_category <- gsub(check, "Laptop", refine_original$product_category)
  }
  else if(check == "q")
  {
    refine_original$product_category <- gsub(check, "Tablet", refine_original$product_category)
  }
}

#4: Add full address for geocoding
refine_original <- mutate(refine_original, full_address = paste(refine_original$address, refine_original$city, refine_original$country, sep=", "))

#5: Create dummy variables for company and product category
dummy_philips = c(1:25)
dummy_akzo = c(1:25)
dummy_van_houten = c(1:25)
dummy_unilever = c(1:25)
dummy_smartphone = c(1:25)
dummy_tv = c(1:25)
dummy_laptop = c(1:25)
dummy_tablet = c(1:25)
for(i in dummy_philips){
  if(refine_original$company[i] == "philips"){
    dummy_philips[i] = 1
  }
  else{
    dummy_philips[i] = 0
  }
}
for(i in dummy_akzo){
  if(refine_original$company[i] == "akzo"){
    dummy_akzo[i] = 1
  }
  else{
    dummy_akzo[i] = 0
  }
}
for(i in dummy_van_houten){
  if(refine_original$company[i] == "van houten"){
    dummy_van_houten[i] = 1
  }
  else{
    dummy_van_houten[i] = 0
  }
}
for(i in dummy_unilever){
  if(refine_original$company[i] == "unilever"){
    dummy_unilever[i] = 1
  }
  else{
    dummy_unilever[i] = 0
  }
}
for(i in dummy_smartphone){
  if(refine_original$product_category[i] == "Smartphone"){
    dummy_smartphone[i] = 1
  }
  else{
    dummy_smartphone[i] = 0
  }
}
for(i in dummy_tv){
  if(refine_original$product_category[i] == "TV"){
    dummy_tv[i] = 1
  }
  else{
    dummy_tv[i] = 0
  }
}
for(i in dummy_laptop){
  if(refine_original$product_category[i] == "Laptop"){
    dummy_laptop[i] = 1
  }
  else{
    dummy_laptop[i] = 0
  }
}
for(i in dummy_tablet){
  if(refine_original$product_category[i] == "Tablet"){
    dummy_tablet[i] = 1
  }
  else{
    dummy_tablet[i] = 0
  }
}
refine_original <- mutate(refine_original, company_philips = dummy_philips)
refine_original <- mutate(refine_original, company_akzo = dummy_akzo)
refine_original <- mutate(refine_original, company_van_houten = dummy_van_houten)
refine_original <- mutate(refine_original, company_unilever = dummy_unilever)
refine_original <- mutate(refine_original, product_smartphone = dummy_smartphone)
refine_original <- mutate(refine_original, product_tv = dummy_tv)
refine_original <- mutate(refine_original, product_laptop = dummy_laptop)
refine_original <- mutate(refine_original, product_tablet = dummy_tablet)

refine_clean <- select(refine_original, company, name, product_category, full_address, company_philips, company_akzo, company_van_houten, company_unilever, product_smartphone, product_tv, product_laptop, product_tablet)
write.csv(refine_clean, file = "refine_clean.csv")
