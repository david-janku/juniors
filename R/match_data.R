#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param db_path
#' @param ids
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
match_data <- function(matched_obj_unfunded, matched_obj_funded) {

   
    matched_data_unfunded <- get_matches(matched_obj_unfunded) # this actually spits out exactly the list of treated nad mtached untreated people in a way that I can easily use it!
    
    matched_data_funded <- get_matches(matched_obj_funded) # this actually spits out exactly the list of treated nad mtached untreated people in a way that I can easily use it!
    
   
    
    # # Convert 'subclass' to numeric in both dataframes
    # matched_data_unfunded$subclass <- as.numeric(as.character(matched_data_unfunded$subclass))
    # matched_data_funded$subclass <- as.numeric(as.character(matched_data_funded$subclass))
    # 
    # # Now find max and add offset
    # max_subclass_unfunded <- max(matched_data_unfunded$subclass, na.rm = TRUE)
    # matched_data_funded$subclass <- matched_data_funded$subclass + max_subclass_unfunded
    # 
    # matched_data_unfunded$subclass <- as.factor(matched_data_unfunded$subclass)
    # matched_data_funded$subclass <- as.factor(matched_data_funded$subclass)
    
    ## joining the two control groups into one table
    ##the funded will have code 2,treatment will have code 1 and unfunded will have code 0 in the "treatment" variable
    
    
    matched_data_unfunded$funded_control <- ifelse(matched_data_unfunded$vedidk %in% matched_data_funded$vedidk, 1, 0)
    matched_data_unfunded$funded_control <- ifelse(matched_data_unfunded$treatment == 1, 0, matched_data_unfunded$funded_control)
    matched_data_unfunded$first_grant <- NA
    # matched_data_unfunded <- matched_data_unfunded %>% 
    #     filter(treatment != 1)
    matched_data_funded$funded_control <- ifelse(matched_data_funded$treatment == 0, 1, 0)
    matched_data_funded$treatment <- ifelse(matched_data_funded$treatment == 0, 2, matched_data_funded$treatment)
    
    
    full_data_1 <- rbind2(matched_data_unfunded, matched_data_funded)
    
    #doubling data so we can calculate indepedence indicator independently from the pubs before the intevention and after the intervention
    full_data_2 <- full_data_1 
    full_data_2$independence_timing <- "before_intervention"
    full_data_1$independence_timing <- "after_intervention"
    
    full_data <-  bind_rows(full_data_2, full_data_1) 
    
    full_data <- dplyr::rename(full_data, career_length = length)
    
    
    full_data$id <- seq_along(full_data$vedidk)
    
    full_data
    
   
}
