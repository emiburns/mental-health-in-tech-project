library(tidyverse)

#prepping data
SelectSigFeatures <- function(data){
    data <- data %>% select(mh_discussion_supervisor, ph_discussion_negative, mh_ph_serious,
                            mh_id_coworkers_impact, mh_discussion_coworkers, mh_medical_leave,
                            mh_id_career_impact, mh_interview, previous_employers_mh_discuss_impact,
                            mh_unsupportive_workplace, mh_discussion_negative)
    
    return(data)
}