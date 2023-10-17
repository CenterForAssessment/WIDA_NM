#######################################################################
###
### Output format for WIDA NM summaries
###
#######################################################################

### Load packages
require(data.table)

### Load data
load("Data/WIDA_NM_SGP_LONG_Data.Rdata")

### Variables to create summaries for
variable.names.2023.demo <- c("Female", "Male", "Hispanic", "White", "Black", "Asian", "Native", "Multirace", "FRL", "NotFRL", "DirectCert", "NotDirectCert", "SwD", "NotSwD", "EL", "NotEL", "Migrant", "Homeless", "Military", "Foster", "Any_Native", "Any_Black")

### Create variables
WIDA_NM_Summaries_LIST <- list()

### Utility function
percent_in_category <- function(my.variable, in.categories, all.categories) {
    my.table <- table(my.variable)
    return(round(100*sum(my.table[in.categories])/sum(my.table[all.categories]), digits=1))
}



### ALL Students 
WIDA_NM_Summaries_LIST[["All_Students"]] <- data.table(Group="All Students", Percent_Meeting_or_Exceeding_Target=percent_in_category(WIDA_NM_SGP_LONG_Data[['NEW_MEXICO_GROWTH_MODEL_TARGET_MET']], "Met New Mexico Growth Target" , c("Did not meet New Mexico Growth Target", "Met New Mexico Growth Target")), Count=sum(!is.na(WIDA_NM_SGP_LONG_Data[['NEW_MEXICO_GROWTH_MODEL_TARGET_MET']])))

### Loop over variable names
for (group.iter in variable.names.2023.demo) {
    WIDA_NM_Summaries_LIST[[group.iter]] <- data.table(Group=group.iter, Percent_Meeting_or_Exceeding_Target=percent_in_category(WIDA_NM_SGP_LONG_Data[get(group.iter)==1][['NEW_MEXICO_GROWTH_MODEL_TARGET_MET']], "Met New Mexico Growth Target" , c("Did not meet New Mexico Growth Target", "Met New Mexico Growth Target")), Count=sum(!is.na(WIDA_NM_SGP_LONG_Data[get(group.iter)==1][['NEW_MEXICO_GROWTH_MODEL_TARGET_MET']])))
}

### Create table and output
WIDA_NM_Growth_Model_2023_OUTPUT <- rbindlist(WIDA_NM_Summaries_LIST)

save(WIDA_NM_Growth_Model_2023_OUTPUT, file="Data/WIDA_NM_Growth_Model_2023_OUTPUT.Rdata")
fwrite(WIDA_NM_Growth_Model_2023_OUTPUT, file="Data/WIDA_NM_Growth_Model_2023_OUTPUT.csv")
