#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
library(dplyr)
#Read Data
data=read.csv('LIVEPLUS_M1')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_event_name)="Event Name"

label(data$medical_history_complete)="Complete?"
label(data$anthro_bpdiffer)="Do the first and second BP measurements differ by more than 20 unites? "
label(data$anthro_weightdiff)="Are the two weight measurements more than 0.1 kg apart? "
label(data$anthropometrics_complete)="Complete?"
label(data$bloods_com)="Have any abnormal results been communicated with the cardiologist and/or participant? "
label(data$bloods_glucometer_done)="Was Glucometer test done? "
label(data$bloods_glucometer_reason)="Reason Glucometer test was not done"
label(data$screening_family___1)="Family medical history  (choice=Diabetes)"
label(data$screening_family___2)="Family medical history  (choice=High Blood Pressure)"
label(data$screening_family___3)="Family medical history  (choice=High Cholesterol)"
label(data$screening_family___4)="Family medical history  (choice=Angina, MI, other)"
label(data$screening_family___5)="Family medical history  (choice=Stroke)"
label(data$socio_marital)="What is your current marital status?"
label(data$socio_home___1)="Who else lives in your home?  (choice=No one, I live alone)"
label(data$socio_home___2)="Who else lives in your home?  (choice=Wife/husband/partner)"
label(data$socio_home___3)="Who else lives in your home?  (choice=Daughter(s))"
label(data$socio_home___4)="Who else lives in your home?  (choice=Son(s))"
label(data$socio_home___5)="Who else lives in your home?  (choice=Brother(s))"
label(data$socio_home___6)="Who else lives in your home?  (choice=Sister(s))"
label(data$socio_home___7)="Who else lives in your home?  (choice=Grandchildren)"
label(data$socio_home___8)="Who else lives in your home?  (choice=House share, friends)"
label(data$socio_home___9)="Who else lives in your home?  (choice=House share, acquaintances)"
label(data$socio_home___10)="Who else lives in your home?  (choice=Other (please specify))"
label(data$socio_housing)="What is your housing arrangement? Are you:"
label(data$socio_raceethnicity)="Race/Ethnicity"
label(data$socio_born)="In which country were you born?"
label(data$socio_english)="When did you first learn to speak English?"
label(data$socio_language)="What language do you usually speak at home?"
label(data$socio_schoollevel)="Since leaving school have you obtained a trade qualification, certificate, diploma or any other qualification?"
label(data$socio_qualification)="If yes, what is your highest qualification?"
label(data$socio_employ)="Are you currently in paid employment?"
label(data$socio_retired)="If no, how old were you when you retired completely?"
label(data$socio_income___1)="Which of the following are sources of income for you?  (choice=Age pension)"
label(data$socio_income___2)="Which of the following are sources of income for you?  (choice=Repatriation pension, Veterans pension)"
label(data$socio_income___3)="Which of the following are sources of income for you?  (choice=Superannuation or other private income)"
label(data$socio_income___4)="Which of the following are sources of income for you?  (choice=Own business/farm/partnership)"
label(data$socio_income___5)="Which of the following are sources of income for you?  (choice=Wage or salary)"
label(data$socio_income___6)="Which of the following are sources of income for you?  (choice=Other (please specify))"
label(data$socio_income___99)="Which of the following are sources of income for you?  (choice=Missing)"
label(data$sociodemographics_complete)="Complete?"
label(data$age_category_randomisation)="Age category for randomisation"
label(data$bmi_category_randomisation)="BMI category for randomisation"
label(data$treatment)="Treatment"
label(data$randomisation_allocation_complete)="Complete?"
label(data$start_date)="Official intervention start date "
label(data$app_int)="Is the participant in the 5:2 PV group? "
label(data$start_date_and_app_id_complete)="Complete?"
label(data$sex)="Gender"
label(data$height)="Height "
label(data$weight)="Weight "
label(data$bmi)="BMI"
label(data$bmi_calc)="Calculated BMI category"
label(data$age_calc)="Calculated age category"
label(data$enrollment)="Has the participant been formally enrolled in the study? "
label(data$demographics_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("baseline_arm_1","month_1_arm_1","month_6_arm_1","month_12_arm_1"))

data$medical_history_complete.factor = factor(data$medical_history_complete,levels=c("0","1","2"))
data$anthro_bpdiffer.factor = factor(data$anthro_bpdiffer,levels=c("1","0"))
data$anthro_weightdiff.factor = factor(data$anthro_weightdiff,levels=c("1","0"))
data$anthropometrics_complete.factor = factor(data$anthropometrics_complete,levels=c("0","1","2"))
data$bloods_com.factor = factor(data$bloods_com,levels=c("1","2","3"))
data$bloods_glucometer_done.factor = factor(data$bloods_glucometer_done,levels=c("1","0"))
data$bloods_glucometer_reason.factor = factor(data$bloods_glucometer_reason,levels=c("1","2","3"))
data$screening_family___1.factor = factor(data$screening_family___1,levels=c("0","1"))
data$screening_family___2.factor = factor(data$screening_family___2,levels=c("0","1"))
data$screening_family___3.factor = factor(data$screening_family___3,levels=c("0","1"))
data$screening_family___4.factor = factor(data$screening_family___4,levels=c("0","1"))
data$screening_family___5.factor = factor(data$screening_family___5,levels=c("0","1"))
data$socio_marital.factor = factor(data$socio_marital,levels=c("1","2","3","4","5","6","7","8","99"))
data$socio_home___1.factor = factor(data$socio_home___1,levels=c("0","1"))
data$socio_home___2.factor = factor(data$socio_home___2,levels=c("0","1"))
data$socio_home___3.factor = factor(data$socio_home___3,levels=c("0","1"))
data$socio_home___4.factor = factor(data$socio_home___4,levels=c("0","1"))
data$socio_home___5.factor = factor(data$socio_home___5,levels=c("0","1"))
data$socio_home___6.factor = factor(data$socio_home___6,levels=c("0","1"))
data$socio_home___7.factor = factor(data$socio_home___7,levels=c("0","1"))
data$socio_home___8.factor = factor(data$socio_home___8,levels=c("0","1"))
data$socio_home___9.factor = factor(data$socio_home___9,levels=c("0","1"))
data$socio_home___10.factor = factor(data$socio_home___10,levels=c("0","1"))
data$socio_housing.factor = factor(data$socio_housing,levels=c("1","2","3","4","5","6","7","99"))
data$socio_raceethnicity.factor = factor(data$socio_raceethnicity,levels=c("1","2","3","4","5","6","7","8","9","10","11"))
data$socio_born.factor = factor(data$socio_born,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99"))
data$socio_english.factor = factor(data$socio_english,levels=c("1","2","99"))
data$socio_language.factor = factor(data$socio_language,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","666","999"))
data$socio_schoollevel.factor = factor(data$socio_schoollevel,levels=c("1","2","99"))
data$socio_qualification.factor = factor(data$socio_qualification,levels=c("1","2","3","4","55","99"))
data$socio_employ.factor = factor(data$socio_employ,levels=c("1","2","99"))
data$socio_income___1.factor = factor(data$socio_income___1,levels=c("0","1"))
data$socio_income___2.factor = factor(data$socio_income___2,levels=c("0","1"))
data$socio_income___3.factor = factor(data$socio_income___3,levels=c("0","1"))
data$socio_income___4.factor = factor(data$socio_income___4,levels=c("0","1"))
data$socio_income___5.factor = factor(data$socio_income___5,levels=c("0","1"))
data$socio_income___6.factor = factor(data$socio_income___6,levels=c("0","1"))
data$socio_income___99.factor = factor(data$socio_income___99,levels=c("0","1"))
data$sociodemographics_complete.factor = factor(data$sociodemographics_complete,levels=c("0","1","2"))
data$age_category_randomisation.factor = factor(data$age_category_randomisation,levels=c("1","2"))
data$bmi_category_randomisation.factor = factor(data$bmi_category_randomisation,levels=c("1","2","3"))
data$treatment.factor = factor(data$treatment,levels=c("1","2"))
data$randomisation_allocation_complete.factor = factor(data$randomisation_allocation_complete,levels=c("0","1","2"))
data$app_int.factor = factor(data$app_int,levels=c("1","0"))
data$start_date_and_app_id_complete.factor = factor(data$start_date_and_app_id_complete,levels=c("0","1","2"))
data$sex.factor = factor(data$sex,levels=c("1","2","3"))
data$enrollment.factor = factor(data$enrollment,levels=c("1","0"))
data$demographics_complete.factor = factor(data$demographics_complete,levels=c("0","1","2"))

levels(data$redcap_event_name.factor)=c("Baseline","Month 1","Month 6","Month 12")

levels(data$medical_history_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$anthro_bpdiffer.factor)=c("Yes","No")
levels(data$anthro_weightdiff.factor)=c("Yes","No")
levels(data$anthropometrics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$bloods_com.factor)=c("Yes","No","N/A")
levels(data$bloods_glucometer_done.factor)=c("Yes","No")
levels(data$bloods_glucometer_reason.factor)=c("Equipment related","Non-analyzable","Other")
levels(data$screening_family___1.factor)=c("Unchecked","Checked")
levels(data$screening_family___2.factor)=c("Unchecked","Checked")
levels(data$screening_family___3.factor)=c("Unchecked","Checked")
levels(data$screening_family___4.factor)=c("Unchecked","Checked")
levels(data$screening_family___5.factor)=c("Unchecked","Checked")
levels(data$socio_marital.factor)=c("Married","Living with a partner/de facto","Widowed","Divorced","Separated","Never married","Prefer not to answer","Other","Missing")
levels(data$socio_home___1.factor)=c("Unchecked","Checked")
levels(data$socio_home___2.factor)=c("Unchecked","Checked")
levels(data$socio_home___3.factor)=c("Unchecked","Checked")
levels(data$socio_home___4.factor)=c("Unchecked","Checked")
levels(data$socio_home___5.factor)=c("Unchecked","Checked")
levels(data$socio_home___6.factor)=c("Unchecked","Checked")
levels(data$socio_home___7.factor)=c("Unchecked","Checked")
levels(data$socio_home___8.factor)=c("Unchecked","Checked")
levels(data$socio_home___9.factor)=c("Unchecked","Checked")
levels(data$socio_home___10.factor)=c("Unchecked","Checked")
levels(data$socio_housing.factor)=c("The outright owner of your home","Paying off your home","Leasing, purchasing (or other financial plan) in a retirement village","Paying rent or board to a private landlord","Paying rent to the government for public housing","Living rent or board free","Other","Missing")
levels(data$socio_raceethnicity.factor)=c("Aboriginal","Aboriginal and Torres Strait Islander","African","Anglo-Celtic (English, Irish, Scottish, Welsh)","Asian","European","Latin American","Middle-Eastern/North African","Pacific Islander","Other","Choose not to disclose")
levels(data$socio_born.factor)=c("Australia","Africa undefined","Albania","Argentina","Asia undefined","Austria","Bahamas, Caymans (West Indies and Carribean)","Bangladesh","Belgium","Brazil","Bulgaria","Burma","Cambodia","Canada","Caribbean","Chile","China (Republic)","Columbia","Cyprus","Czech Republic","Denmark","Ecuador","Egypt","El Salvador","England + UK undefined","Estonia","Europe undefined","Fiji","Finland","France","Germany","Greece","Hong Kong","Hungary","India","Indonesia","Iran","Iraq","Ireland undefined","Israel, Palestine","Italy","Japan","Jordan, Kuwait, Muscat and Oman, Saudi Arabia, Yemen","Kampuchea","Kenya, Rhodesia","Korea","Laos","Latvia","Lebanon","Lithuania","Malaysia","Malta","Mauritius","Mexico","Montenegro","Netherlands","New Caledonia","New Zealand","Northern Ireland","Norway","Oceania undefined","Pakistan","Papua New Guinea","Peru","Philippines","Poland","Portugal","Romania","Scotland","Serbia","Singapore","Slovakia","South Africa (Republic)","South America undefined","Southern Europe undefined","Spain","Sri lanka","Sweden","Switzerland","Syria","Taiwan","Thailand","Timor (Portugese, East)","Turkey","Uganda, Zambia","Ukraine","Uruguay","USA + America, undefined","USSR Former undefined","Venezuela","Vietnam","Wales","Zimbabwe","Croatia","Slovenia","Bosnia and Herzegovina","Yugoslavia","Palestine","Missing")
levels(data$socio_english.factor)=c("Before 12 years of age","After or equal to 12 years of age","Missing")
levels(data$socio_language.factor)=c("English","Aboriginal","African (undefined)","Afrikaans","Albanian","Arabic","Armenian","Asian (undefined)","Benglai","Bulgarian","Burmese","Chinese","Croatian","Czech","Danish","Dutch","Estonian","European (undefined)","Fijian","Filipino","Finnish","French","Gaelic","German","Greek","Hindi","Hungarian","Indonesian","Italian","Japanese","Khmer","Korean","Lao","Latvian","Lebanese","Lithuanian","Macedonian","Malay","Maltese","Maori","Norwegian","Oceanic (undefined)","Polish","Portuguese","Romanian","Romany","Russian","Serbian","Serbo-Croatian","Sinhalese","Slovak","Slovenian","Spanish","Swedish","Tamil","Tetum","Thai","Tongan","Turkish","Ukrainian","Urdu","Vietnamese","Welsh","Yiddish","Yugoslav (undefined)","Cantonese","Other","Missing")
levels(data$socio_schoollevel.factor)=c("Yes","No","Missing")
levels(data$socio_qualification.factor)=c("Bachelor Degree or higher","Trade/Apprenticeship","Certificate/Diploma","Other (please specify)","Not applicable","Missing")
levels(data$socio_employ.factor)=c("Yes","No","Missing")
levels(data$socio_income___1.factor)=c("Unchecked","Checked")
levels(data$socio_income___2.factor)=c("Unchecked","Checked")
levels(data$socio_income___3.factor)=c("Unchecked","Checked")
levels(data$socio_income___4.factor)=c("Unchecked","Checked")
levels(data$socio_income___5.factor)=c("Unchecked","Checked")
levels(data$socio_income___6.factor)=c("Unchecked","Checked")
levels(data$socio_income___99.factor)=c("Unchecked","Checked")
levels(data$sociodemographics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$age_category_randomisation.factor)=c("1","2")
levels(data$bmi_category_randomisation.factor)=c("1 (BMI under 30)","2","3")
levels(data$treatment.factor)=c("Control","Intervention")
levels(data$randomisation_allocation_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$app_int.factor)=c("Yes","No")
levels(data$start_date_and_app_id_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$sex.factor)=c("Female","Male","Other")
levels(data$enrollment.factor)=c("Yes","No")
levels(data$demographics_complete.factor)=c("Incomplete","Unverified","Complete")

#Group data by intervention and control group
grouped_data <- data %>% group_by(Group)

#Proportion of race
ethinicty <- grouped_data %>% summarize(
        ethinicty = prop.table(table(socio_raceethnicity.factor))
)

category_table <- table(data$socio_raceethnicity.factor)
category_proportions <- prop.table(category_table)
category_df <- as.data.frame(category_proportions)

library(dplyr)

# Calculate the proportions of each ethnicity within each group
ethinicity_group <- data.frame(Category = data$socio_raceethnicity.factor, Group = data$Group)
ethinicity_group_table <- table(ethinicity_group)
ethinicity_proportion <- prop.table(ethinicity_group_table, margin = 2)
ethinicity_proportion_df <- as.data.frame(ethinicity_proportion)
colnames(ethinicity_proportion_df) <- c("ethinicity", "group", "proportion")

library(dplyr)

# Create a 2x2 contingency table
contingency_table <- data %>%
        group_by(socio_raceethnicity.factor, Group) %>%
        summarize(count = n()) %>%
        pivot_wider(names_from = Group, values_from = count) %>%
        replace(is.na(.), 0)  # Replace NA values with 

# Perform Fisher's exact test for each ethnicity
ethnicity_test_results <- contingency_table %>%
        mutate(p_value = apply(contingency_table[, c("intervention", "control")], 1, function(row) {
                fisher.test(matrix(row, ncol = 2))$p.value
        })) %>%
        select(socio_raceethnicity.factor, p_value)

# Display the results
print(ethnicity_test_results)


#Proportion of education
edu <- table(Category=data$socio_qualification.factor, Subgroup=data$treatment.factor)
edu_proportion <- prop.table(edu, margin=2)
edu_proportion_df <- as.data.frame(edu_proportion)

#Proportion of sex
sex <- table(Category=data$sex.factor, Subgroup=data$treatment.factor)
sex_proportion <- prop.table(sex, margin=2)
sex_proportion_df <- as.data.frame(sex_proportion)

#Average of age
age <- data.frame(age=data$age, Subgroup=data$treatment.factor)
mean_age_by_subgroup <- aggregate(age~Subgroup, data = age, FUN=mean)
sd_age_by_subgroup <- aggregate(age~Subgroup, data = age, FUN=sd)

#Write function to calculate median and interquartile range
custom_fun <- function(x) {
        median_value <- median(x)
        iqr_value <- IQR(x)
        return(c(median = median_value, IQR = iqr_value))
}

#Average of bmi
bmi <- data.frame(bmi=data$bmi, Subgroup=data$treatment.factor)
mean_bmi_by_subgroup <- aggregate(bmi~Subgroup, data = bmi, FUN=mean)
sd_bmi_by_subgroup <- aggregate(bmi~Subgroup, data = bmi, FUN=sd)

#Average of SBP
data$anthro_sbpav <- (data$anthro_sbp1 + data$anthro_sbp2)/2
sbp <- data.frame(sbp=data$anthro_sbpav, Subgroup=data$treatment.factor)
sbp
histogram(data$anthro_sbpav)
boxplot(data$anthro_sbpav)
boxplot(data$anthro_dbpav)
boxplot(data$bloods_cholesterol)
boxplot(data$bloods_ldl)
boxplot(data$bloods_hdl)
boxplot(data$bloods_tag)
mean_sbp_by_subgroup <- aggregate(sbp~Subgroup, data = sbp, FUN=mean)
sd_sbp_by_subgroup <- aggregate(sbp~Subgroup, data = sbp, FUN=sd)
median_sbp_by_subgroup <- aggregate(sbp~Subgroup, data = sbp, FUN=median)
iqr_sbp_by_subgroup <- aggregate(sbp~Subgroup, data = sbp, FUN=IQR)

#Average of DBP
data$anthro_dbpav <- (data$anthro_dbp1 + data$anthro_dbp2)/2
dbp <- data.frame(dbp=data$anthro_dbpav, Subgroup=data$treatment.factor)
mean_dbp_by_subgroup <- aggregate(dbp~Subgroup, data = dbp, FUN=mean)
sd_dbp_by_subgroup <- aggregate(dbp~Subgroup, data = dbp, FUN=sd)

#Average of TC
tc <- data.frame(tc=data$bloods_cholesterol, Subgroup=data$treatment.factor)
mean_tc_by_subgroup <- aggregate(tc~Subgroup, data = tc, FUN=mean)
sd_tc_by_subgroup <- aggregate(tc~Subgroup, data = tc, FUN=sd)
median_tc_by_subgroup <- aggregate(tc~Subgroup, data = tc, FUN=median)
iqr_tc_by_subgroup <- aggregate(tc~Subgroup, data = tc, FUN=IQR)

#Average of LDL-C
ldl <- data.frame(ldl=data$bloods_ldl, Subgroup=data$treatment.factor)
mean_ldl_by_subgroup <- aggregate(ldl~Subgroup, data = ldl, FUN=mean)
sd_ldl_by_subgroup <- aggregate(ldl~Subgroup, data = ldl, FUN=sd)
median_ldl_by_subgroup <- aggregate(ldl~Subgroup, data = ldl, FUN=median)
iqr_ldl_by_subgroup <- aggregate(ldl~Subgroup, data = ldl, FUN=IQR)

#Average of HDL-C
hdl <- data.frame(hdl=data$bloods_hdl, Subgroup=data$treatment.factor)
mean_hdl_by_subgroup <- aggregate(hdl~Subgroup, data = hdl, FUN=mean)
sd_hdl_by_subgroup <- aggregate(hdl~Subgroup, data = hdl, FUN=sd)
median_hdl_by_subgroup <- aggregate(hdl~Subgroup, data = hdl, FUN=median)
iqr_hdl_by_subgroup <- aggregate(hdl~Subgroup, data = hdl, FUN=IQR)

#Average of TG
tg <- data.frame(tg=data$bloods_tag, Subgroup=data$treatment.factor)
mean_tg_by_subgroup <- aggregate(tg~Subgroup, data = tg, FUN=mean)
sd_tg_by_subgroup <- aggregate(tg~Subgroup, data = tg, FUN=sd)

#Average of HbA1c
hba1c <- data.frame(hba1c=data$bloods_haemoglobin, Subgroup=data$treatment.factor)
mean_hba1c_by_subgroup <- aggregate(hba1c~Subgroup, data = hba1c, FUN=mean)
sd_hba1c_by_subgroup <- aggregate(hba1c~Subgroup, data = hba1c, FUN=sd)



#------------------------------------------------------- 
# To calculate the mean of clinical data after one month of intervention using jamovi
#------------------------------------------------------- 

jmv::descriptives(
                    formula = bmi_calculated.month_1_arm_1 + SBP_mean.month_1_arm_1 + DBP_mean.month_1_arm_1 + 
                        bloods_cholesterol.month_1_arm_1 + bloods_ldl.month_1_arm_1 + bloods_hdl.month_1_arm_1 + 
                        bloods_haemoglobin.month_1_arm_1 + bloods_tag.month_1_arm_1 + anthro_weight1.month_1_arm_1 
                        ~ Treatment,
                    data = data,
                    ci = TRUE)



#------------------------------------------------------- 
# To calculate changes in clinical data after one month of intervention using jamovi
#------------------------------------------------------- 

jmv::ttestIS(
                formula = `Change in weight` + `Change in BMI` + `Change in SBP` + `Change in DBP` + 
                `Change in TC` + `Change in LDL` + `Change in HDL` + `Change in TG` + `Change in HbA1c` 
                ~ Treatment,
                    data = data,
                    vars = vars(Change in weight, Change in BMI, Change in SBP, Change in DBP, Change in TC, 
                                Change in LDL, Change in HDL, Change in TG, Change in HbA1c),
                    meanDiff = TRUE,
                    ci = TRUE,
                    desc = TRUE)
