#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param topic_model
#' @return
#' @author fatal: unable to access 'C:/Users/David Jank?/Documents/.config/git/config': Invalid argument
#' @export
calc_ind_topics <- function(topic_model, two_authors) {

    all_pubs <- as_tibble(t(topic_model)) %>% #in this step, the identificators of pubs gets deleted and substituted with numbers
        rownames_to_column("pub_id") %>% 
        mutate(pub_id = as.integer(pub_id)) %>% 
        left_join(two_authors %>% 
                      select(pub_id = ID_core_pubs, list)) #this should be matched with "Kód.výstupu" not ID_core_ 
    
    
    all_pubs <- mutate(all_pubs, str_detect(all_pubs$list, "Binter"))
    
    all_pubs <- mutate(all_pubs, str_detect(all_pubs$list, "Klapilová"))
    
   # colnames(all_pubs) <- paste0("topic_", colnames(topics))
    
    #tahle funkce říká: pokud v daném tématu nemá sledovaný výzkumník ani jednu publikaci s loadingem  více než 0,1, pak mi vytiskni "0", pokud má alespoň jednu, ale zároveň i jeho supervisors má alsepoň jednu, tak mi vytiskni "2", a pokud má alespoň jednu ale supervisor nemá žádnou, tak vytiskni "1". Takhle funkce bohužel není škálovatelná, takže se bude muset přepsat.   
    topics$topic_one <- if(sum(ifelse(data[1,1:20]>0.1, 1, 0))<1){print("0")
    } else if(sum(ifelse(data[1,21:77]>0.1, 1, 0))>1){print("2")
    } else {
        print("1")
    }
    
    topics$topic_two <- if(sum(ifelse(data[2,1:20]>0.1, 1, 0))<1){print("0")
    } else if(sum(ifelse(data[2,21:77]>0.1, 1, 0))>1){print("2")
    } else {
        print("1")
    }
    
    topics$topic_three <- if(sum(ifelse(data[3,1:20]>0.1, 1, 0))<1){print("0")
    } else if(sum(ifelse(data[3,21:77]>0.1, 1, 0))>1){print("2")
    } else {
        print("1")
    }
    
    topics$topic_four<- if(sum(ifelse(data[4,1:20]>0.1, 1, 0))<1){print("0")
    } else if(sum(ifelse(data[4,21:77]>0.1, 1, 0))>1){print("2")
    } else {
        print("1")
    }
    
    topics$topic_five <- if(sum(ifelse(data[5,1:20]>0.1, 1, 0))<1){print("0")
    } else if(sum(ifelse(data[5,21:77]>0.1, 1, 0))>1){print("2")
    } else {
        print("1")
    }
    
    topics <- rm(topics$topics)
    
    topic_independence_categories = sum(ifelse(topics==1, 1, 0))/(sum(ifelse(topics==2, 1, 0)))+(sum(ifelse(topics==1, 1, 0)))
    
}
