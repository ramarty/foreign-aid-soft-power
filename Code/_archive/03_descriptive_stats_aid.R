# Descriptive Stats of Aid
# Balance table comparing completed vs. planned projects, but using sample for 
# which we have both US and China data, dropping active projects.

# Load Data --------------------------------------------------------------------
planned_type <- "plnnbeforesurvey"
buffer <- 30
planned_cutoff <- "08"
remove_nodata <- TRUE
countries_subset <- "usaid_countries"
round_subset <- ".r6"

africa <- readRDS(file.path(rawdata_file_path, "GADM_Africa", "africa.Rds"))
africa_usaid <- africa[africa$NAME_0 %in% c("Malawi", "Nigeria", "Uganda", "Senegal", "Burundi", "Sierra Leone"),]
africa_usaid$id <- 1
africa_usaid <- raster::aggregate(africa_usaid, by="id")

for(planned_type in c("plannedbeforeagreement", "plnnbeforesurvey")){
  for(buffer in c(30)){
    for(planned_cutoff in c("08","09","10")){ 
      for(remove_nodata in c(TRUE, FALSE)){
        for(countries_subset in c("usaid_countries", "all_countries")){
          for(round_subset in c(".r6","")){
        
        print(paste0("planned_type: ",planned_type,
                     " buffer: ", buffer,
                     " planned_cutoff: ", planned_cutoff,
                     " remove_nodata: ", remove_nodata,
                     " countries_subset: ", countries_subset,
                     " round_subset: ", round_subset))
        
        # Load and Prep Data -----------------------------------------------------------
        chinese_aid <- read.csv(file.path(intermediatedata_file_path, "Aid Level Datasets","Aid Merged Project Status", paste0("chinese_aid_complplnnimpl_",planned_type,".csv")))
        us_aid <- read.csv(file.path(intermediatedata_file_path, "Aid Level Datasets","Aid Merged Project Status", paste0("usaid_aid_complplnnimpl_",planned_type,".csv")))
        
        #### Rename Variables
        planned_cutoff_year <- paste0("20", planned_cutoff)
        
        names(chinese_aid)[names(chinese_aid) == paste0("completed_near_china.pl",planned_cutoff,".",buffer,"km",round_subset)] <- "aid.completed"
        names(chinese_aid)[names(chinese_aid) == paste0("planned_near_china.pl",planned_cutoff,".",buffer,"km",round_subset)] <- "aid.planned"
        names(chinese_aid)[names(chinese_aid) == paste0("noyeardata_near_china.pl",planned_cutoff,".",buffer,"km",round_subset)] <- "aid.nodata"
        names(chinese_aid)[names(chinese_aid) == paste0("implementing_near_china.pl",planned_cutoff,".",buffer,"km",round_subset)] <- "aid.active"
        
        names(us_aid)[names(us_aid) == paste0("completed_near_usaid.",buffer,"km",round_subset)] <- "aid.completed"
        names(us_aid)[names(us_aid) == paste0("planned_near_usaid.",buffer,"km",round_subset)] <- "aid.planned"
        names(us_aid)[names(us_aid) == paste0("noyeardata_near_usaid.",buffer,"km",round_subset)] <- "aid.nodata"
        names(us_aid)[names(us_aid) == paste0("implementing_near_usaid.",buffer,"km",round_subset)] <- "aid.active"
        
        #### Subset Data
        ### Precision Code
        chinese_aid <- chinese_aid[chinese_aid$precision_code %in% 1:2,]
        us_aid <- us_aid[us_aid$precision_code %in% 1:2,]
        

        
        ### Countries
        if(countries_subset %in% "usaid_countries"){
          print(nrow(chinese_aid))
          chinese_aid_sp <- chinese_aid
          coordinates(chinese_aid_sp) <- ~longitude+latitude
          chinese_aid$distance_usaid_countries <- as.numeric(gDistance(chinese_aid_sp, africa_usaid, byid=T)) * 111.12
          chinese_aid <- chinese_aid[chinese_aid$distance_usaid_countries <= buffer,]
          print(nrow(chinese_aid))
        }
        
        ### Drop Active Projects
        chinese_aid <- chinese_aid[!(chinese_aid$aid.active %in% 1),]
        us_aid <- us_aid[!(us_aid$aid.active %in% 1),]
        
        ### Drop No Data Projects
        if(remove_nodata == TRUE){
          chinese_aid <- chinese_aid[!(chinese_aid$aid.nodata %in% 1),]
          us_aid <- us_aid[!(us_aid$aid.nodata %in% 1),]
        }
        
        # Adjust Sectors ---------------------------------------------------------------
        chinese_aid$sector <- chinese_aid$crs_sector_name
        chinese_aid$sector <- as.character(chinese_aid$sector)
        chinese_aid$sector[chinese_aid$sector %in% "Support to Non-governmental Organizations (NGOs) and Government Organizations"] <- "Support to NGOs"
        chinese_aid$sector[chinese_aid$sector %in% "Population Policies / Programmes and Reproductive Health"] <- "Poluation / Reproductive Health"
        chinese_aid$sector[chinese_aid$sector %in% "Developmental Food Aid/Food Security Assistance"] <- "Food Security Assistance"
        chinese_aid$sector[grepl("Social|social", chinese_aid$sector)] <- "Social Services"
        chinese_aid$sector[chinese_aid$sector %in% "Poluation / Reproductive Health"] <- "Reproductive Health"
        chinese_aid$sector[grepl("Other Multisector|Unallocated / Unspecified", chinese_aid$sector)] <- "Other"
        chinese_aid$sector <- tools::toTitleCase(chinese_aid$sector)
        
        us_aid$sector <- us_aid$ad_sector_names %>% as.character
        us_aid$sector[us_aid$sector %in% "Post-secondary education"] <- "Education"
        us_aid$sector[us_aid$sector %in% "Secondary education"] <- "Education"
        us_aid$sector[us_aid$sector %in% "Education policy and administrative management"] <- "Education"
        us_aid$sector[us_aid$sector %in% "Family planning"] <- "Reproductive Health"
        us_aid$sector[grepl("Population policies|Population Policies", us_aid$sector)] <- "Reproductive Health"
        us_aid$sector[us_aid$sector %in% "Basic health"] <- "Health"
        us_aid$sector[us_aid$sector %in% "Health personnel development"] <- "Health"
        us_aid$sector[us_aid$sector %in% "Health, general, combinations of activities "] <- "Health"
        us_aid$sector[us_aid$sector %in% "STD control including HIV/AIDS"] <- "Health"
        us_aid$sector[grepl("Agriculture|Forestry", us_aid$sector)] <- "Agriculture, Forestry and Fishing"
        us_aid$sector[grepl("Government and civil society", us_aid$sector)] <- "Government and civil society"
        us_aid$sector[us_aid$sector %in% "Sectors not specified "] <- "Other"
        us_aid$sector[us_aid$sector %in% "Social/ welfare services"] <- "Social Services"
        us_aid$sector[us_aid$sector %in% "Multisector aid for social services"] <- "Social Services"
        us_aid$sector[us_aid$sector %in% "Other social infrastructure and services"] <- "Social Services"
        us_aid$sector[us_aid$sector %in% "Banking and financial services, combinations of activities "] <- "Banking/Financial Services"
        us_aid$sector <- tools::toTitleCase(us_aid$sector)
        
        # Table ------------------------------------------------------------------------
        ROUND_NUM_1 <- 2
        ROUND_NUM_2 <- 3
        
        for(donor in c("china", "usa")){
          print(donor)
          if(donor == "china"){
            df_aid <- chinese_aid
          } 
          if(donor == "usa"){
            df_aid <- us_aid
            df_aid$usd_current <- df_aid$total_commitments %>% as.character() %>% as.numeric
          } 
          
          round_subset_name <- round_subset %>% str_replace_all("\\.","")
          
          #if(planned_type %in% "plannedbeforeagreement") planned_type <- "plnnbeforeagreement"
          
          sink(file.path(tables_file_path, paste0("aid_sumstats_paper1_",donor,"_",planned_cutoff,"_removenodata_",remove_nodata,"_",buffer,"km_",planned_type,"_",countries_subset,"_",round_subset_name,"_noactive.tex")))
          
          cat("\\begin{tabular}{lcc | cc | cc} ")
          cat(" \\hline ")
          cat(" & \\multicolumn{2}{c|}{Median Amount} & \\multicolumn{2}{c|}{N} & \\multicolumn{2}{c}{Proportion} \\\\ ")
          cat(" & \\multicolumn{2}{c|}{(Millions USD)} & \\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{} \\\\ ")
          cat(" \\hline ")
          cat(" & Completed & Planned & Completed & Planned & Completed & Planned \\\\ ")
          cat(" \\hline ")
          
          sector_list <- sort(unique(df_aid$sector[(df_aid$aid.completed %in% 1) | (df_aid$aid.planned %in% 1)]))
          for(sector in sector_list){
            completed_usd <- median(df_aid$usd_current[df_aid$sector %in% sector & df_aid$aid.completed %in% 1], na.rm=T)/1000000
            planned_usd <- median(df_aid$usd_current[df_aid$sector %in% sector & df_aid$aid.planned %in% 1], na.rm=T)/1000000
            
            completed_N <- length(df_aid$usd_current[df_aid$sector %in% sector & df_aid$aid.completed %in% 1])
            planned_N <- length(df_aid$usd_current[df_aid$sector %in% sector & df_aid$aid.planned %in% 1])
            
            completed_prop <- completed_N / sum(df_aid$aid.completed %in% 1)
            planned_prop <- planned_N / sum(df_aid$aid.planned %in% 1)
            
            cat(sector, " & ")
            
            if(length(completed_usd) %in% 0) completed_usd <- NA
            if(length(planned_usd) %in% 0) planned_usd <- NA
            
            if(is.na(completed_usd)){
              cat("n/a", " & ", sep="")
            } else{
              cat("\\$", round(completed_usd, ROUND_NUM_1), " & ", sep="")
            }
            
            
            if(is.na(planned_usd)){
              cat("n/a", " & ", sep="")
            } else{
              cat("\\$", round(planned_usd, ROUND_NUM_1), " & ", sep="")
            }
            
            #cat("\\$", round(completed_usd, ROUND_NUM_1), " & ", sep="")
            #cat("\\$", round(planned_usd, ROUND_NUM_1), " & ", sep="")
            cat(completed_N, " & ")
            cat(planned_N, " & ")
            cat(round(completed_prop, ROUND_NUM_2), " & ")
            cat(round(planned_prop, ROUND_NUM_2), " \\\\ ")
          }
          
          completed_usd <- median(df_aid$usd_current[df_aid$aid.completed %in% 1], na.rm=T)/1000000
          planned_usd <- median(df_aid$usd_current[df_aid$aid.planned %in% 1], na.rm=T)/1000000
          
          completed_N <- length(df_aid$usd_current[df_aid$aid.completed %in% 1])
          planned_N <- length(df_aid$usd_current[df_aid$aid.planned %in% 1])
          
          cat(" \\hline ")
          cat("Median Amount / N Projects & ")
          cat("\\$", round(completed_usd, ROUND_NUM_1), " & ", sep="")
          cat("\\$", round(planned_usd, ROUND_NUM_1), " & ", sep="")
          cat(completed_N, " & ")
          cat(planned_N, " & ")
          cat(" & \\\\")
          
          cat(" \\hline ")
          cat(" \\end{tabular}")
          
          sink()
          
        }
        
        
        
      }
    }
  }
  }
  }
}


