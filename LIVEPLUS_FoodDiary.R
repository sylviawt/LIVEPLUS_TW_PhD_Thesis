#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('LIVEPLUS_FoodDiary.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_event_name)="Event Name"
label(data$treatment)="Treatment"


label(data$weight_g)="Weight (g)"
label(data$energydf_kj)="Energy (kJ)"
label(data$protein_g)="Protein (g)"
label(data$total_fat_g)="Total fat (g)"
label(data$saturated_fat_g)="Saturated fat (g)"
label(data$trans_fatty_acids_g)="Trans fatty acids (g)"
label(data$polyunsaturated_fat_g)="Polyunsaturated fat (g)"
label(data$monounsaturated_fat_g)="Monounsaturated fat (g)"
label(data$cholesterol_mg)="Cholesterol (mg)"
label(data$carbohydrate_available_g)="Carbohydrate (g)"
label(data$sugars_g)="Sugars (g)"
label(data$added_sugars_g)="Added sugars (g)"
label(data$free_sugars_g)="Free sugars (g)"
label(data$starch_g)="Starch (g)"
label(data$water_g)="Water (g)"
label(data$alcohol_g)="Alcohol (g)"
label(data$dietary_fibre_g)="Dietary fibre (g)"
label(data$ash_g)="Ash (g)"
label(data$thiamin_mg)="Thiamin (mg)"
label(data$riboflavin_mg)="Riboflavin (mg)"
label(data$niacin_mg)="Niacin (mg)"
label(data$niacin_equivalents_mg)="Niacin equivalents (mg)"
label(data$vitamin_c_mg)="Vitamin C (mg)"
label(data$vitamin_e_mg)="Vitamin E (mg)"
label(data$tocopherol_alpha_mg)="Tocopherol alpha (mg)"
label(data$vitamin_b6_by_analysis_mg)="Vitamin B6 (mg)"
label(data$vitamin_b12_g)="Vitamin B12 (µg)"
label(data$total_folate_g)="Total folate (µg)"
label(data$folate_total_dfe_g)="Folate total DFE (µg)"
label(data$folic_acid_g)="Folic acid (µg)"
label(data$folate_food_g)="Folate food (µg)"
label(data$total_vit_a_eq_g)="Total Vit A eq (µg)"
label(data$retinol_g)="Retinol (µg)"
label(data$beta_carotene_equivalents_g)="Beta carotene equivalents (µg)"
label(data$beta_carotene_g)="Beta carotene (µg)"
label(data$sodium_mg)="Sodium (mg)"
label(data$potassium_mg)="Potassium (mg)"
label(data$magnesium_mg)="Magnesium (mg)"
label(data$calcium_mg)="Calcium (mg)"
label(data$phosphorus_mg)="Phosphorus (mg)"
label(data$iron_mg)="Iron (mg)"
label(data$zinc_mg)="Zinc (mg)"
label(data$selenium_g)="Selenium (µg)"
label(data$iodine_g)="Iodine (µg)"
label(data$kj_from_protein_percent)="kJ from protein (%)"
label(data$kj_from_fat_percent)="kJ from fat (%)"
label(data$kj_from_saturated_fat_percent)="kJ from saturated fat (%)"
label(data$kj_from_trans_fat_percent)="kJ from trans fat (%)"
label(data$kj_from_carbohydrate_percent)="kJ from carbohydrate (%)"
label(data$kj_from_alcohol_percent)="kJ from alcohol (%)"
label(data$kj_from_fibre_percent)="kJ from fibre (%)"
label(data$kj_from_others_percent)="kJ from others (%)"
label(data$fat_as_mono_percent)="Fat as mono (%)"
label(data$fat_as_poly_percent)="Fat as poly (%)"
label(data$fat_as_saturated_percent)="Fat as saturated (%)"
label(data$vlc_n3_g)="VLC N3 (g)"
label(data$f18d2cn6_linoleic_g)="Linoleic (g)"
label(data$f18d3n3_alpha_linolenic_ala_g)="Alpha linolenic_ALA (g)"
label(data$f20d5n3_eicosapentaenoic_epa_g)="Eicosapentaenoic_EPA (g)"
label(data$f22d5n3_docosapentaenoic_dpa_g)="Docosapentaenoic_DPA (g)"
label(data$f22d6n3_docosahexaenoic_dha_g)="Docosahexaenoic_DHA (g)"
label(data$tryptophan_g)="Tryptophan (g)"
label(data$salicylates)="Salicylates"
label(data$amines)="Amines"
label(data$glutamates)="Glutamates"
label(data$grains_serve)="Grains (serve)"
label(data$refined_serve)="Refined grains (serve)"
label(data$wholegrains_serve)="Wholegrains (serve)"
label(data$ratio_wholegrains)="Ratio wholegrains"
label(data$fruit_serve)="Fruit (serve)"
label(data$citrus_melons_berries_serve)="Citrus/melons/berries (serve)"
label(data$other_fruit_serve)="Other fruit (serve)"
label(data$fruit_juice_serve)="Fruit juice (serve)"
label(data$ratio_fruit_juice)="Ratio fruit juice"
label(data$vegetables_serve)="Vegetables (serve)"
label(data$dark_green_vegetables_serve)="Dark green vegetables (serve)"
label(data$red_orange_vegetables_serve)="Red orange vegetables (serve)"
label(data$tomatoes_serve)="Tomatoes (serve)"
label(data$other_red_orange_veg_serve)="Other red orange veg (serve)"
label(data$starchy_vegetables_serve)="Starchy vegetables (serve)"
label(data$potatoes_serve)="Potatoes (serve)"
label(data$other_starchy_veg_serve)="Other starchy veg (serve)"
label(data$ratio_starchy_veg)="Ratio starchy veg"
label(data$legumes_veg_serve)="Legumes veg (serve)"
label(data$other_vegetables_serve)="Other vegetables (serve)"
label(data$protein_foods_serve)="Protein foods (serve)"
label(data$red_meats_serve)="Red meats (serve)"
label(data$poultry_serve)="Poultry (serve)"
label(data$eggs_serve)="Eggs (serve)"
label(data$processed_meats_serve)="Processed meats (serve)"
label(data$organ_meats_serve)="Organ meats (serve)"
label(data$seafood_high_lc_n3_serve)="Seafood_high_LC_N3 (serve)"
label(data$seafood_low_lc_n3_serve)="Seafood_low_LC_N3 (serve)"
label(data$nuts_seeds_serve)="Nuts/seeds (serve)"
label(data$legumes_protein_serve)="Legumes protein (serve)"
label(data$soy_products_serve)="Soy products (serve)"
label(data$dairy_serve)="Dairy (serve)"
label(data$milk_serve)="Milk (serve)"
label(data$cheese_serve)="Cheese (serve)"
label(data$yoghurt_serve)="Yoghurt (serve)"
label(data$milk_alternatives_serve)="Milk alternatives (serve)"
label(data$oil_equivalents_tsp)="Oil equivalents (tsp)"
label(data$solid_fat_equivalents_tsp)="Solid fat equivalents (tsp)"
label(data$added_sugars_tsp)="Added sugars (tsp)"
label(data$energy_from_added_sugar)="Energy from added sugar"
label(data$energy_from_added_sugar_percent)="Energy from added sugar (%)"
label(data$alcoholic_drinks_sd)="Alcoholic drinks sd"
label(data$unclassified_weight_g)="Unclassified weight (g)"
label(data$unclassified_weight_percent)="Unclassified weight (%)"
label(data$unclassified_kj_kj)="Unclassified_kJ"
label(data$unclassified_kj_percent)="Unclassified_kJ (%)"
label(data$caffeine_mg)="Caffeine (mg)"
label(data$food_record_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("baseline_arm_1","month_1_arm_1","month_6_arm_1","month_12_arm_1"))

data$food_record_complete.factor = factor(data$food_record_complete,levels=c("0","1","2"))
data$treatment.factor = factor(data$treatment,levels=c("1","2"))

levels(data$redcap_event_name.factor)=c("Baseline","Month 1","Month 6","Month 12")

levels(data$food_record_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$treatment.factor)=c("Control","Intervention")

data$energy_kcal <- data$energydf_kj/4.18 

#To create a function that generate mean and standard deviation
calculate_mean_sd <- function(data, timepoint_column, variables, timepoints = NULL, overweight_value = NULL) {
          if (!is.null(timepoints)) {
                    data <- subset(data, data[[timepoint_column]] %in% timepoints)
          }
          
          if (!is.null(overweight_value)) {
                    data <- subset(data, overweight == overweight_value)
          }
          
          stats_result <- list()
          
          for (variable in variables) {
                    mean_value <- mean(data[[variable]], na.rm = TRUE)
                    sd_value <- sd(data[[variable]], na.rm = TRUE)
                    
                    stats_result[[variable]] <- list(mean = mean_value, sd = sd_value)
          }
          
          return(stats_result)
}


#To create a function that generate mean and standard deviation by subgroups
calculate_stats_by_subgroup <- function(data, subgroup_column, timepoint_column, variables, timepoints) {
          stats_result <- list()
          
          for (variable in variables) {
                    for (timepoint in timepoints) {
                              subset_data <- subset(data, data[, timepoint_column] == timepoint)
                              
                              mean_by_subgroup <- aggregate(subset_data[, variable] ~ subset_data[, subgroup_column], data = subset_data, FUN = mean)
                              sd_by_subgroup <- aggregate(subset_data[, variable] ~ subset_data[, subgroup_column], data = subset_data, FUN = sd)
                              median_by_subgroup <- aggregate(subset_data[, variable] ~ subset_data[, subgroup_column], data = subset_data, FUN = median)
                              iqr_by_subgroup <- aggregate(subset_data[, variable] ~ subset_data[, subgroup_column], data = subset_data, FUN = IQR)
                              
                              stats_result[[paste(variable, timepoint, sep = "_")]] <- list(mean = mean_by_subgroup, sd = sd_by_subgroup)
                    }
          }
          
          return(stats_result)
}

nutrient <- calculate_stats_by_subgroup(data = data, subgroup_column = "treatment.factor", 
                                      timepoint_column = "redcap_event_name", 
                                      variables = c("energydf_kj", "energy_kcal", "protein_g", "total_fat_g",
                                                    "saturated_fat_g", "trans_fatty_acids_g", "polyunsaturated_fat_g",
                                                    "monounsaturated_fat_g", "carbohydrate_available_g",
                                                    "sugars_g", "added_sugars_g", "alcohol_g", "dietary_fibre_g",
                                                    "thiamin_mg", "riboflavin_mg", "niacin_mg", "vitamin_c_mg",
                                                    "vitamin_e_mg", "tocopherol_alpha_mg", "vitamin_b6_by_analysis_mg",
                                                    "vitamin_b12_g", "total_folate_g", "folic_acid_g",
                                                    "total_vit_a_eq_g", "retinol_g", "beta_carotene_g",
                                                    "sodium_mg", "potassium_mg", "magnesium_mg", "calcium_mg",
                                                    "phosphorus_mg", "iron_mg", "zinc_mg", "selenium_g",
                                                    "iodine_g", "kj_from_protein_percent", "kj_from_fat_percent",
                                                    "kj_from_saturated_fat_percent"), 
                                      timepoints = c("baseline_arm_1", "month_1_arm_1"))


foods <- calculate_stats_by_subgroup(data = data, subgroup_column = "treatment.factor", 
                                      timepoint_column = "redcap_event_name", 
                                      variables = c("kj_from_trans_fat_percent", "kj_from_carbohydrate_percent",
                                      "grains_serve", "refined_serve",
                                                    "wholegrains_serve", "fruit_serve", "vegetables_serve", "starchy_vegetables_serve",
                                                    "potatoes_serve", "legumes_veg_serve", "red_meats_serve",
                                                    "poultry_serve", "eggs_serve", "processed_meats_serve",
                                                    "organ_meats_serve", "seafood_high_lc_n3_serve", "seafood_low_lc_n3_serve",
                                                    "nuts_seeds_serve", "legumes_protein_serve", "soy_products_serve",
                                                    "milk_serve", "cheese_serve", "yoghurt_serve", "milk_alternatives_serve",
                                                    "oil_equivalents_tsp", "solid_fat_equivalents_tsp", "added_sugars_tsp",
                                                    "alcoholic_drinks_sd", "caffeine_mg"), 
                                      timepoints = c("baseline_arm_1", "month_1_arm_1"))


monofat <- calculate_stats_by_subgroup(data = data, subgroup_column = "treatment.factor", 
                                        timepoint_column = "redcap_event_name", 
                                        variables = c(
                                                      "monounsaturated_fat_g"), 
                                        timepoints = c("baseline_arm_1", "month_1_arm_1"))

calculate_median_by_subgroup <- function(data, subgroup_column, timepoint_column, variables, timepoints) {
          stats_result <- list()
          
          for (variable in variables) {
                    for (timepoint in timepoints) {
                              subset_data <- subset(data, data[, timepoint_column] == timepoint)
                              
                              median_by_subgroup <- aggregate(subset_data[, variable], by = list(subset_data[, subgroup_column]), FUN = median)
                              names(median_by_subgroup) <- c(subgroup_column, "Median")
                              
                              iqr_by_subgroup <- aggregate(subset_data[, variable], by = list(subset_data[, subgroup_column]), FUN = IQR)
                              names(iqr_by_subgroup) <- c(subgroup_column, "IQR")
                              
                              stats_result[[paste(variable, timepoint, sep = "_")]] <- list(median = median_by_subgroup, iqr = iqr_by_subgroup)
                    }
          }
          
          return(stats_result)
}

median <- calculate_median_by_subgroup(data = data, subgroup_column = "treatment.factor", 
                                       timepoint_column = "redcap_event_name", 
                                       variables = c("folic_acid_g",
                                                     "beta_carotene_g"), 
                                       timepoints = c("baseline_arm_1"))

#Subset baseline dietary intake
baseline_diet <- subset(data, redcap_event_name == "baseline_arm_1")

#To calculate mean and standard deviation of foods and nutrient intakes
foods_baseline_diet <- calculate_mean_sd(data = baseline_diet, 
                                                    timepoint_column = "redcap_event_name", 
                                                    variables = c("kj_from_trans_fat_percent", "kj_from_carbohydrate_percent",
                                                                  "grains_serve", "refined_serve",
                                                                  "wholegrains_serve", "fruit_serve", "vegetables_serve", "starchy_vegetables_serve",
                                                                  "potatoes_serve", "legumes_veg_serve", "red_meats_serve",
                                                                  "poultry_serve", "eggs_serve", "processed_meats_serve",
                                                                  "organ_meats_serve", "seafood_high_lc_n3_serve", "seafood_low_lc_n3_serve",
                                                                  "nuts_seeds_serve", "legumes_protein_serve", "soy_products_serve",
                                                                  "milk_serve", "cheese_serve", "yoghurt_serve", "milk_alternatives_serve",
                                                                  "oil_equivalents_tsp", "solid_fat_equivalents_tsp", "added_sugars_tsp",
                                                                  "alcoholic_drinks_sd", "caffeine_mg"))

nutrients_baseline_diet <- calculate_mean_sd(data = baseline_diet, 
                                                        timepoint_column = "redcap_event_name", 
                                                        variables = c("energydf_kj", "energy_kcal", "protein_g", "total_fat_g",
                                                                      "saturated_fat_g", "trans_fatty_acids_g", "polyunsaturated_fat_g",
                                                                      "monounsaturated_fat_g", "carbohydrate_available_g",
                                                                      "sugars_g", "added_sugars_g", "sodium_mg", "potassium_mg", "dietary_fibre_g",
                                                                      "kj_from_protein_percent", "kj_from_fat_percent",
                                                                      "kj_from_saturated_fat_percent"))

#Subset baseline dietary intake in people with overweight
baseline_diet_overweight <- subset(baseline_diet, Overweight == 1)

#To calculate mean and standard deviation of foods and nutrient intakes in people with overweight
foods_baseline_diet_overweight <- calculate_mean_sd(data = baseline_diet_overweight, 
                                                    timepoint_column = "redcap_event_name", 
                                                    variables = c("kj_from_trans_fat_percent", "kj_from_carbohydrate_percent",
                                                                  "grains_serve", "refined_serve",
                                                                  "wholegrains_serve", "fruit_serve", "vegetables_serve", "starchy_vegetables_serve",
                                                                  "potatoes_serve", "legumes_veg_serve", "red_meats_serve",
                                                                  "poultry_serve", "eggs_serve", "processed_meats_serve",
                                                                  "organ_meats_serve", "seafood_high_lc_n3_serve", "seafood_low_lc_n3_serve",
                                                                  "nuts_seeds_serve", "legumes_protein_serve", "soy_products_serve",
                                                                  "milk_serve", "cheese_serve", "yoghurt_serve", "milk_alternatives_serve",
                                                                  "oil_equivalents_tsp", "solid_fat_equivalents_tsp", "added_sugars_tsp",
                                                                  "alcoholic_drinks_sd", "caffeine_mg"))

nutrients_baseline_diet_overweight <- calculate_mean_sd(data = baseline_diet_overweight, 
                                                    timepoint_column = "redcap_event_name", 
                                                    variables = c("energydf_kj", "energy_kcal", "protein_g", "total_fat_g",
                                                                  "saturated_fat_g", "trans_fatty_acids_g", "polyunsaturated_fat_g",
                                                                  "monounsaturated_fat_g", "carbohydrate_available_g",
                                                                  "sugars_g", "added_sugars_g", "sodium_mg", "potassium_mg", "dietary_fibre_g",
                                                                  "kj_from_protein_percent", "kj_from_fat_percent",
                                                                  "kj_from_saturated_fat_percent"))

#Subset baseline dietary intake in people with hypertension
baseline_diet_HTN <- subset(baseline_diet, Hypertension == 1)

#To calculate mean and standard deviation of foods and nutrient intakes in people with HTN
foods_baseline_diet_HTN <- calculate_mean_sd(data = baseline_diet_HTN, 
                                                    timepoint_column = "redcap_event_name", 
                                                    variables = c("kj_from_trans_fat_percent", "kj_from_carbohydrate_percent",
                                                                  "grains_serve", "refined_serve",
                                                                  "wholegrains_serve", "fruit_serve", "vegetables_serve", "starchy_vegetables_serve",
                                                                  "potatoes_serve", "legumes_veg_serve", "red_meats_serve",
                                                                  "poultry_serve", "eggs_serve", "processed_meats_serve",
                                                                  "organ_meats_serve", "seafood_high_lc_n3_serve", "seafood_low_lc_n3_serve",
                                                                  "nuts_seeds_serve", "legumes_protein_serve", "soy_products_serve",
                                                                  "milk_serve", "cheese_serve", "yoghurt_serve", "milk_alternatives_serve",
                                                                  "oil_equivalents_tsp", "solid_fat_equivalents_tsp", "added_sugars_tsp",
                                                                  "alcoholic_drinks_sd", "caffeine_mg"))

nutrients_baseline_diet_HTN <- calculate_mean_sd(data = baseline_diet_HTN, 
                                                        timepoint_column = "redcap_event_name", 
                                                        variables = c("energydf_kj", "energy_kcal", "protein_g", "total_fat_g",
                                                                      "saturated_fat_g", "trans_fatty_acids_g", "polyunsaturated_fat_g",
                                                                      "monounsaturated_fat_g", "carbohydrate_available_g",
                                                                      "sugars_g", "added_sugars_g", "sodium_mg", "potassium_mg", "dietary_fibre_g",
                                                                      "kj_from_protein_percent", "kj_from_fat_percent",
                                                                      "kj_from_saturated_fat_percent"))    

#------------------------------------------------------- 
# To label intervention and control group
#------------------------------------------------------- 

data$treatment <- ifelse(data$treatment == "2", "intervention", "control")

#------------------------------------------------------- 
# To calculate changes in nutrients intake at one month using jamovi
#------------------------------------------------------- 

jmv::ttestIS(
              formula = `Change in energy_kJ` + `Change in energy_kcal` + `Change in protein` + `Change in protein %` + 
                `Change in total fat` + `Change in fat` + `Change in saturated fat` + `Change in sat fat` + `Change in trans fat` + 
                `Change in trans fat %` + `Change in PUFA` + `Change in MUFA` + `Change in CHO` + `Change in CHO %E` + 
                `Change in sugars` + `Change in dietary fibre` + `Change in sodium` + `Change in potassium` + `Change in grains` + 
                `Change in refined` + `Change in wholegrains` + `Change in fruit` + `Change in veg` + `Change in starchy` + 
                `Change in potatoes` + `Change in legumes` + `Change in red meats` + `Change in poultry` + `Change in seafood high` + 
                `Change in seafood low` + `Change in eggs` + `Change in legumes veg` + `Change in nuts` + `Change in processed meat` + 
                `Change in soy products` + `Change in milk` + `Change in cheese` + `Change in yoghurt` + `Change in milk alter` + 
                `Change in oil equi` + `Change in solid` + `Change in added sugars` + `Change in alcoholic drink` + `Change in caffeine` 
              ~ treatment,
              data = data,
              vars = vars(Change in energy_kJ, Change in energy_kcal, Change in protein, Change in 'protein %', Change in 'total fat', 
                          Change in fat, Change in 'saturated fat', Change in 'sat fat', Change in 'trans fat', Change in 'trans fat %', 
                          Change in PUFA, Change in MUFA, Change in CHO, Change in CHO %E, Change in sugars, Change in dietary fibre, 
                          Change in sodium, Change in potassium, Change in grains, Change in refined, Change in wholegrains, 
                          Change in fruit, Change in veg, Change in starchy, Change in potatoes, Change in legumes, 
                          Change in red meats, Change in poultry, Change in seafood high, Change in seafood low, Change in eggs, 
                          Change in legumes veg, Change in nuts, Change in processed meat, Change in soy products, Change in milk, 
                          Change in cheese, Change in yoghurt, Change in milk alter, Change in oil equi, Change in solid, 
                          Change in added sugars, Change in alcoholic drink, Change in caffeine),
              meanDiff = TRUE,
              ci = TRUE,
              desc = TRUE)


jmv::ttestIS(
              formula = `Change in dairy` + `Change in protein serve` + `Change in Vit C` + `Change in Vit E` + 
                `change in total folate` + `change in Vit a` + `change in iron` + `change in zinc` + `change in iodine` ~ treatment,
              data = data,
              vars = vars(Change in dairy, Change in protein serve, Change in Vit C, Change in Vit E, change in total folate, 
                          change in Vit a, change in iron, change in zinc, change in iodine),
              meanDiff = TRUE,
              ci = TRUE,
              desc = TRUE)


#------------------------------------------------------- 
# To calculate changes in food groups intake at one month using jamovi
#------------------------------------------------------- 

jmv::ttestIS(
              formula = alcohol_g.baseline_arm_1 + vitamin_c_mg.baseline_arm_1 + vitamin_e_mg.baseline_arm_1 + 
                total_folate_g.baseline_arm_1 + total_vit_a_eq_g.baseline_arm_1 + iron_mg.baseline_arm_1 + 
                zinc_mg.baseline_arm_1 + iodine_g.baseline_arm_1 + grains_serve.baseline_arm_1 + refined_serve.baseline_arm_1 + 
                wholegrains_serve.baseline_arm_1 + fruit_serve.baseline_arm_1 + vegetables_serve.baseline_arm_1 + 
                starchy_vegetables_serve.baseline_arm_1 + potatoes_serve.baseline_arm_1 + legumes_veg_serve.baseline_arm_1 + 
                red_meats_serve.baseline_arm_1 + poultry_serve.baseline_arm_1 + seafood_high_lc_n3_serve.baseline_arm_1 + 
                seafood_low_lc_n3_serve.baseline_arm_1 + eggs_serve.baseline_arm_1 + legumes_protein_serve.baseline_arm_1 + 
                nuts_seeds_serve.baseline_arm_1 + soy_products_serve.baseline_arm_1 + milk_serve.baseline_arm_1 + 
                cheese_serve.baseline_arm_1 + yoghurt_serve.baseline_arm_1 + milk_alternatives_serve.baseline_arm_1 + 
                processed_meats_serve.baseline_arm_1 + alcoholic_drinks_sd.baseline_arm_1 + caffeine_mg.baseline_arm_1 + 
                oil_equivalents_tsp.baseline_arm_1 + solid_fat_equivalents_tsp.baseline_arm_1 + added_sugars_g.baseline_arm_1 + 
                protein_foods_serve.baseline_arm_1 + protein_foods_serve.month_1_arm_1 + dairy_serve.baseline_arm_1 + 
                dairy_serve.month_1_arm_1 ~ treatment,
                data = data,
                vars = vars(alcohol_g.baseline_arm_1, vitamin_c_mg.baseline_arm_1, vitamin_e_mg.baseline_arm_1, 
                            total_folate_g.baseline_arm_1, total_vit_a_eq_g.baseline_arm_1, iron_mg.baseline_arm_1, 
                            zinc_mg.baseline_arm_1, iodine_g.baseline_arm_1, grains_serve.baseline_arm_1, 
                            refined_serve.baseline_arm_1, wholegrains_serve.baseline_arm_1, fruit_serve.baseline_arm_1, 
                            vegetables_serve.baseline_arm_1, starchy_vegetables_serve.baseline_arm_1, potatoes_serve.baseline_arm_1, 
                            legumes_veg_serve.baseline_arm_1, red_meats_serve.baseline_arm_1, poultry_serve.baseline_arm_1, 
                            seafood_high_lc_n3_serve.baseline_arm_1, seafood_low_lc_n3_serve.baseline_arm_1, 
                            eggs_serve.baseline_arm_1, legumes_protein_serve.baseline_arm_1, nuts_seeds_serve.baseline_arm_1, 
                            soy_products_serve.baseline_arm_1, milk_serve.baseline_arm_1, cheese_serve.baseline_arm_1, 
                            yoghurt_serve.baseline_arm_1, milk_alternatives_serve.baseline_arm_1, 
                            processed_meats_serve.baseline_arm_1, alcoholic_drinks_sd.baseline_arm_1, caffeine_mg.baseline_arm_1, 
                            oil_equivalents_tsp.baseline_arm_1, solid_fat_equivalents_tsp.baseline_arm_1, 
                            added_sugars_g.baseline_arm_1, protein_foods_serve.baseline_arm_1, protein_foods_serve.month_1_arm_1, 
                            dairy_serve.baseline_arm_1, dairy_serve.month_1_arm_1),
                meanDiff = TRUE,
                desc = TRUE)
