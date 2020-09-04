#' Convert ImmuneSpace conditon-studied to a curated version
#'
#' @param pd phenotypic meta-data data.table with condtion from ImmuneSpace
#' @export
#'
mapCondition <- function(pd){
  
  unmarkedInfluenzaStudies <- c("301", "144", "224", "180",
                                "80", "296", "364", "368", "387")
  unmarkedInfluenzaStudies <- paste0("SDY", unmarkedInfluenzaStudies)
  unmarkedHepBStudies <- c("SDY690", "SDY89", "SDY299")
  unmarkedSmallpoxStudies <- c("SDY1370")
  unmarkedPPPStudies <- c("SDY667")
  unmarkedHerpesZosterStudies <- c("SDY984")
  
  pd$mappedCondition <- apply(pd, 1, function(x) {
    study <- x[["study_accession"]]
    condition <- x[["condition"]]
    
    if(study %in% unmarkedInfluenzaStudies | 
      grepl("influenza|H1N1", condition, ignore.case = TRUE)){
      return("Influenza")
    }else if(study %in% unmarkedHepBStudies |
             grepl("Hepatitis", condition, ignore.case = TRUE)){
      return("Hepatitis")
    }else if(study %in% unmarkedSmallpoxStudies | 
             grepl("Smallpox|vaccinia", condition, ignore.case = TRUE)){
      return("Smallpox")
    }else if(study %in% unmarkedPPPStudies){
      return("Palmoplantar_Pustulosis")
    }else if(study %in% unmarkedHerpesZosterStudies){
        return("Herpes_Zoster")
    }else if( grepl("healthy|normal|naive", condition, ignore.case = TRUE)){
      return("Healthy")
    }else if(grepl("CMV", condition, ignore.case = TRUE)){
      return("CMV")
    }else if(grepl("TB|tuberculosis", condition, ignore.case = TRUE)){
      return("Tuberculosis")
    }else if(grepl("Yellow Fever", condition, ignore.case = TRUE)){
      return("Yellow_Fever")
    }else if(grepl("Mening", condition, ignore.case = TRUE)){
      return("Meningitis")
    }else if(grepl("Malaria", condition, ignore.case = TRUE)){
      return("Malaria")
    }else if(grepl("HIV", condition, ignore.case = TRUE)){
      return("HIV")
    }else if(grepl("Dengue", condition, ignore.case = TRUE)){
      return("Dengue")
    }else if(grepl("ZEBOV", condition, ignore.case = TRUE)){
      return("Ebola")
    }else if(grepl("JDM|Dermatomyositis", condition, ignore.case = TRUE)){
      return("Dermatomyositis")
    }else if(grepl("West Nile", condition, ignore.case = TRUE)){
      return("West_Nile")
    }else if(grepl("Zika", condition, ignore.case = TRUE)){
      return("Zika")
    }else if(grepl("Varicella", condition, ignore.case = TRUE)){
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

#' Create a summary data object for easy use with app
#'
#' @param eset expressionSet object with cohort column and gene or btm expression
#' @export
#'
createSummaryData <- function(eset){
  eset$study_cohort <- paste(eset$study_accession, eset$cohort, sep = "_")
  
  # Create data frame with summary statistics for each cohort*timepoint
  res <- lapply(unique(eset$study_cohort), function(study_cohort){
    tmp <- eset[, eset$study_cohort == study_cohort ]
    cell_type <- unique(tmp$cell_type)
    mappedCondition <- unique(tmp$mappedCondition)
    study <- unique(tmp$study_accession)
    cohort <- unique(tmp$cohort)
    
    timepoints <- table(tmp$study_time_collected)
    timepoints <- as.numeric(names(timepoints)[ timepoints > 2 ])
    baseline <- tmp[, tmp$study_time_collected == 0 ]
    subres <- lapply(timepoints, function(timepoint){
      print(paste(study_cohort, timepoint))
      if(timepoint == 0){
        df <- data.frame(cohort = cohort,
                         cell_type = cell_type,
                         study = study,
                         mappedCondition = mappedCondition,
                         timepoint = timepoint,
                         analyte = rownames(baseline),
                         mean_fold_change = 0,
                         sd_fold_change = 0)
      }else{
        curr <- tmp[, tmp$study_time_collected == timepoint ]
        smplCount <- table(curr$participant_id)
        dupes <- names(smplCount)[ smplCount > 1]
        if(length(dupes) > 0){
          bsToRemove <- sapply(dupes, function(pid){
            dupEntries <- curr[, curr$participant_id == pid ]
            maxDay <- max(dupEntries$study_time_collected)
            toKeep <- dupEntries$biosample_accession[ dupEntries$study_time_collected == maxDay ][[1]]
            currRm <- dupEntries$biosample_accession[ dupEntries$biosample_accession != toKeep]
          })
          bsToRemove <- unlist(unname(bsToRemove))
          curr <- curr[, !curr$biosample_accession %in% bsToRemove ]
        }
        shared <- intersect(baseline$participant_id, curr$participant_id)
        if(length(shared) < 3){
          return()
        }
        curr <- curr[, curr$participant_id %in% shared ]
        base <- baseline[, baseline$participant_id %in% shared]
        curr <- curr[, order(match(curr$participant_id, base$participant_id))]
        currEm <- exprs(curr)
        baseEm <- exprs(base)
        currEm <- currEm[ order(match(rownames(currEm), rownames(baseEm))), ]
        if(!all.equal(dim(currEm), dim(baseEm))){
          stop()
        }
        log_fc <- currEm - baseEm
        mean_log_fc <- rowMeans(log_fc)
        sd_log_fc <- apply(log_fc, 1, sd)
        df <- data.frame(cohort = cohort,
                         cell_type = cell_type,
                         study = study,
                         mappedCondition = mappedCondition,
                         timepoint = timepoint,
                         analyte = rownames(log_fc),
                         mean_fold_change = mean_log_fc,
                         sd_fold_change = sd_log_fc)
      }
      return(df)
    })
    subresDF <- do.call("rbind", subres)
  })
  allRes <- data.table::rbindlist(res)
}
