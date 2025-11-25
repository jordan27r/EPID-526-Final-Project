# --- Load libraries ---
library(dplyr)

# --- Load the firearm-related dataset ---
data <- read.csv("C://Users//jorda//OneDrive//Documents//EPID 526//polRecArrestData.csv",
                 stringsAsFactors = FALSE)

set.seed(2025)
rows <- list()
master_counter <- 1
master_pool <- character()  # store previously created MasterIDs

incs <- unique(data$INCI_ID)

for (inc in incs) {
  # Random number of people per incident
  n_people <- sample(1:3, 1)
  
  # Some people are new, some are returning (10–20%)
  n_returning <- rbinom(1, n_people, prob = 0.4)
  
  # Choose returning MasterIDs from pool if available
  returning <- if (length(master_pool) > 0 && n_returning > 0) {
    sample(master_pool, min(n_returning, length(master_pool)))
  } else character(0)
  
  # Create new MasterIDs for new people
  n_new <- n_people - length(returning)
  new_people <- paste0("M", sprintf("%06d", master_counter:(master_counter + n_new - 1)))
  master_counter <- master_counter + n_new
  
  # Combine returning + new
  people <- c(returning, new_people)
  master_pool <- unique(c(master_pool, people))
  
  # Base rows for participants
  incident_rows <- data.frame(
    MasterID = people,
    INCI_ID = inc,
    ArrestID = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Add arrests for this incident
  sub <- data %>% filter(INCI_ID == inc)
  arrest_ids <- unique(na.omit(sub$ArrestID))
  if (length(arrest_ids) > 0) {
    for (aid in arrest_ids) {
      mid <- sample(people, 1)
      incident_rows <- rbind(
        incident_rows,
        data.frame(MasterID = mid, INCI_ID = inc, ArrestID = aid, stringsAsFactors = FALSE)
      )
    }
  }
  
  rows[[length(rows) + 1]] <- incident_rows
}

mock_links <- bind_rows(rows) %>% 
  mutate(INCI_ID = as.numeric(INCI_ID))
mock_data <- data %>% left_join(mock_links, by = c("INCI_ID", "ArrestID"))
write.csv(mock_data, "polRecArrestData_withMasterID.csv", row.names = FALSE)


