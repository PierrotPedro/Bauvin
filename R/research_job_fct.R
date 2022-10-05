# From https://www.r-bloggers.com/2022/09/optimizing-my-search-for-data-scientist-jobs-by-scraping-indeed-with-r/?fbclid=IwAR0clJlan1VeY3fgnnhd5aEjQvQANk-V7IYuo2As-uy6_AGKdhZhcFotTTY

library(rvest)
library(stringr)
library(httr)
library(tidystopwords)
library(textcat)

# Function to tidy the data related to the company
tidy_comploc <- function(text){
    lst <- str_split(text, pattern = "\n", simplify =T)
    ext_str <- substr(lst[1], nchar(lst[1])-2, nchar(lst[1]))
    res <- suppressWarnings(as.numeric(gsub(',', '.', ext_str)))
    lst[1] <- ifelse(is.na(res), lst[1], substr(lst[1], 1, nchar(lst[1])-3))
    lst[3] <- res
    t(as.matrix(lst))
}

# Function to tidy the short job description provided with the job post
tidy_job_desc <- function(text){
    stopwords <- c("Candidature facile", "Employeur réactif")
    text <- str_remove_all(text, paste(stopwords, collapse = "|"))
    stopwords_2 <- "(Posted|Employer).*"
    text <- str_remove_all(text, stopwords_2)
    text
}

# Function to tidy the salary data if provided
tidy_salary <- function(text){
    if(is.na(text)){
        others <- NA
        sal_low <- NA
        sal_high <- NA
    }else{
        text <- str_split(text, "\n", simplify = T)
        others <- paste(text[str_detect(text, "€", negate = T)], collapse = " | ")
        sal <- text[str_detect(text, "€", negate = F)]
        if(rlang::is_empty(sal)){
            sal_low <- NA
            sal_high <- NA
        }else{
            range_sal <- as.numeric(str_split(str_remove_all(str_replace(sal, "à", "-"), "[^0-9.-]"), "-", simplify = TRUE))
            sal_low <- sort(range_sal)[1]
            sal_high <- sort(range_sal)[2]
            
            if(str_detect(sal, "an")){
                sal_low <- floor(sal_low/12)
                sal_high <- floor(sal_high/12)
            }
        }
    }
    return(c(as.numeric(sal_low), as.numeric(sal_high), others))
}

# Function to tidy the location of the job (Remote/Hybrid/Onsite) + homogenize 
# location and zip code
tidy_location <- function(final_df){
    final_df$Job_type <- ifelse(final_df$Location == "Télétravail", "Full Remote", ifelse(str_detect(final_df$Location, "Télétravail"), "Hybrid", "On site"))
    final_df$Loc_possibility <- ifelse(str_detect(final_df$Location, "lieu"), "Plusieurs lieux", NA)
    stopwords <- c("Télétravail à", "Télétravail", "à", "hybride")
    final_df$Loc_tidy <- str_remove_all(final_df$Location, paste(stopwords, collapse = "|"))
    final_df$Loc_tidy <- str_remove_all(final_df$Loc_tidy, "[+].*")
    final_df$Loc_tidy <- str_trim(final_df$Loc_tidy)
    final_df$Loc_tidy <-  sapply(final_df$Loc_tidy,
                                 function(x){
                                     if(!is.na(suppressWarnings(as.numeric(substr(x, 1, 5))))){
                                         return(paste(substr(x, 7, 30), paste0('(', substr(final_df$Loc_tidy[2], 1, 2), ')')))
                                     }else{
                                         return(x)
                                     }})
    return(final_df)
}

# Function to keep only certain words in text
keep_words <- function(text, keep) {
    words <- strsplit(text, " ")[[1]]
    txt <- paste(words[words %in% keep], collapse = " ")
    return(txt)
}

# Homogenize the job title and class them in a few categories
clean_job_title <- function(job_titles){
    job_titles <- tolower(job_titles)
    job_titles <- gsub("[[:punct:]]", " ", job_titles, perl=TRUE)
    
    words_to_keep <- c("data", "scientist", "junior", "senior", "engineer", "nlp",
                       "analyst", "analytics", "analytic", "science", "sciences",
                       "computer", "vision", "ingenieur", "données", "analyste",
                       "analyses", "lead", "leader", "dataminer", "mining", "chief",
                       "miner", "analyse", 'head')
    job_titles_c <- unlist(sapply(job_titles, function(x){keep_words(x, words_to_keep)}, USE.NAMES = F))
    job_titles_c <- unlist(sapply(job_titles_c, function(x){paste(unique(unlist(str_split(x, " "))), collapse = " ")}, USE.NAMES = F))
    table(job_titles_c)
    
    data_analytics_ind <-  job_titles_c %in% c("analyses data", "analyst data", "analyste data", "analyste data scientist", "data analyse",
                                               "analyste données", "analytic data scientist", "analytics data", "analytics data engineer", "data analyst engineer",
                                               "data analyst données", "data analyst scientist", "data analyst scientist données", "data analyste", "data analyst analytics",
                                               "data analytics", "data analytics engineer", "data engineer analyst", "data scientist analyst", "data scientist analytics")
    job_titles_c[data_analytics_ind] <- "data analyst"
    
    data_analytics_j_ind <-  job_titles_c %in% c("junior data analyst", "junior data analytics", "junior data scientist analyst")
    job_titles_c[data_analytics_j_ind] <- "data analyst junior"
    
    data_scientist_ind <- job_titles_c %in% c("data computer science", "data science", "data science scientist", "data sciences",
                                              "data sciences scientist", "data scientist données", "data scientist sciences",
                                              "données data scientist", "scientist data", "science données", "scientist data",
                                              "scientist data science", "computer data science", "data science données", "data scientist science")
    job_titles_c[data_scientist_ind] <- "data scientist"
    
    data_scientist_j_ind <- job_titles_c %in% c("junior data scientist")
    job_titles_c[data_scientist_j_ind] <- "data scientist junior"
    
    data_engineer_ind <- job_titles_c %in% c("data engineer scientist", "data science engineer", "data miner", "data scientist engineer",
                                             "dataminer", "engineer data scientist", "senior data scientist engineer", "ingenieur data scientist")
    job_titles_c[data_engineer_ind] <- "data engineer"
    
    nlp_data_scientist_ind <- job_titles_c %in% c("data scientist nlp", "nlp data science",
                                                  "nlp data scientist", "senior data scientist nlp")
    job_titles_c[nlp_data_scientist_ind] <- "data scientist NLP"
    
    cv_data_scientist_ind <- job_titles_c %in% c("computer vision data scientist", "data science computer vision",
                                                 "data scientist computer vision")
    job_titles_c[cv_data_scientist_ind] <- "data scientist CV"
    
    lead_data_scientist_ind <- job_titles_c %in% c("chief data", "chief data scientist", "data scientist leader", "lead data scientist",
                                                   "data chief scientist", "lead data scientist senior", "head data science")
    job_titles_c[lead_data_scientist_ind] <- "data scientist lead or higher"
    senior_data_scientist_ind <- job_titles_c %in% c("senior data scientist")
    job_titles_c[senior_data_scientist_ind] <- "data scientist senior"
    
    senior_data_analytics_ind <- job_titles_c %in% c("senior analytics data scientist", "senior data analyst", "senior data scientist analytics")
    job_titles_c[senior_data_analytics_ind] <- "data analyst senior"
    
    
    lead_data_analyst_ind <- job_titles_c %in% c("lead data analyst senior", "lead data analyst")
    job_titles_c[lead_data_analyst_ind] <- "data analyst lead"
    return(job_titles_c)
}

# Function to clean the full job description before word annotation
clean_job_desc <- function(text){
    text <- tolower(text)
    text <- str_replace_all(text, "\n", " ")
    text <- str_remove(text, pattern = "dé.*du poste ")
    text <- str_remove(text, pattern = "analyse de recr.*")
    text <- gsub("(?!&)[[:punct:]+’+…+»+«]", " ", text, perl=TRUE)
    
    language <- textcat(text)
    
    if(language == "french"){
        text <- str_replace_all(text, "œ", "oe")
        stopwords <- c("détails", "poste", "description", "informations", "complémentaires", "c", generate_stoplist(language = "French"))
    }else{
        stopwords <- c("description", generate_stoplist(language = "English"))
    }
    
    text <- str_replace_all(text, paste(stopwords, collapse = " | "), " ")
    text <- str_replace_all(text, paste(stopwords, collapse = " | "), " ")
    text <- str_replace_all(text, paste(stopwords, collapse = " | "), " ")
    
    return(c(language, text))
}