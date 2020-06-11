#' Convert ImmuneSpace conditon-studied to a curated version
#'
#' @param pd phenotypic meta-data data.table with condtion from ImmuneSpace
#' @export
#'
addMappedCondition <- function(pd){
  
  pd$newCondition <- sapply(pd$condition, function(x) {
    if( grepl("healthy|normal|naive", x, ignore.case = TRUE)){
      return("Healthy")
    }else if(grepl("influenza|H1N1", x, ignore.case = TRUE)){
      return("Influenza")
    }else if(grepl("CMV", x, ignore.case = TRUE)){
      return("CMV")
    }else if(grepl("TB|tuberculosis", x, ignore.case = TRUE)){
      return("Tuberculosis")
    }else if(grepl("Yellow Fever", x, ignore.case = TRUE)){
      return("Yellow_Fever")
    }else if(grepl("Mening", x, ignore.case = TRUE)){
      return("Meningitis")
    }else if(grepl("Malaria", x, ignore.case = TRUE)){
      return("Malaria")
    }else if(grepl("HIV", x, ignore.case = TRUE)){
      return("HIV")
    }else if(grepl("Dengue", x, ignore.case = TRUE)){
      return("Dengue")
    }else if(grepl("ZEBOV", x, ignore.case = TRUE)){
      return("Ebola")
    }else if(grepl("Hepatitis", x, ignore.case = TRUE)){
      return("Hepatitis")
    }else if(grepl("Smallpox|vaccinia", x, ignore.case = TRUE)){
      return("Smallpox")
    }else if(grepl("JDM|Dermatomyositis", x, ignore.case = TRUE)){
      return("Dermatomyositis")
    }else if(grepl("West Nile", x, ignore.case = TRUE)){
      return("West_Nile")
    }else if(grepl("Zika", x, ignore.case = TRUE)){
      return("Zika")
    }else if(grepl("Varicella", x, ignore.case = TRUE)){
      return("Varicella_Zoster")
    }else{
      return("Unknown")
    }
  })
  
  return(pd)
}

#' Extract study id from participant id and add as new field
#'
#' @param pd phenotypic meta-data data.table with participant_id from ImmuneSpace
#' @export
#'
addStudy <- function(pd){
  pd$study <- paste0("SDY", sapply(strsplit(pd$participant_id, "\\."), "[[", 2))
  return(pd)
}

#' Convert 'Hours' or 'Months' based study times to 'Days'
#'
#' @param dt meta-data data.table
#' @import data.table
#' @export
#'
correctTimeUnits <- function(dt){
  dt <- apply(dt, 1, function(row){
    if (row[["study_time_collected_unit"]] == "Hours"){
      row[["study_time_collected"]] <- as.numeric(row[["study_time_collected"]]) / 24
      row[["study_time_collected_unit"]] <- "Days"
    } else if (row[["study_time_collected_unit"]] == "Months"){
      row[["study_time_collected"]] <- as.numeric(row[["study_time_collected"]]) * 30
      row[["study_time_collected_unit"]] <- "Days"
    }
    return(row)
  })
  dt <- data.table(t(dt))
  dt$study_time_collected <- gsub(" ", "", dt$study_time_collected)
  dt$study_time_collected <- gsub("\\.00", "", dt$study_time_collected)
  dt$study_time_collected <- as.numeric(dt$study_time_collected)
  return(dt)
}

#' Create dt with subject * analyte matched columns for baseline and max(post) values
#'
#' @param dt meta-data data.table
#' @param analyteCol analyte column name
#' @param valueCol value column name
#' @export
#'
matchBaselineAndPostTimepoints <- function(dt, analyteCol, valueCol){
  dt <- data.table(dt)
  setnames(dt, c(analyteCol, valueCol), c("analyte", "value"))
  keepCols <- c("ParticipantId", "analyte", "value")
  groupingCols <- c("ParticipantId", "analyte")
  
  baseline <- dt[ dt$study_time_collected == 0, ..keepCols ]
  setnames(baseline, "value", "baseline_value")
  
  post <- dt[ dt$study_time_collected > 0, ..keepCols ]
  rowsToUse <- post[, .I[which.max(value)], by = groupingCols]
  post <- post[ rowsToUse$V1 ]
  setnames(post, "value", "post_value")
  
  final <- merge(baseline, post, by = groupingCols)
  final <- final[ !duplicated(final) ]
}


#' Add fold change assuming no log transform upstream
#'
#' @param dt meta-data data.table
#' @export
#'
addFoldChange <- function(dt){
  dt$baseline_value <- as.numeric(dt$baseline_value)
  dt$post_value <- as.numeric(dt$post_value)
  dt$fc <- (dt$post_value - dt$baseline_value) / dt$baseline_value
  dt$fc <- ifelse(is.infinite(dt$fc), dt$post_value, dt$fc)
  return(dt)
}
