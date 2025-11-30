library(dplyr)

data <- read.csv("polRecArrestData.csv", stringsAsFactors = FALSE)
data$INCI_ID <- as.character(data$INCI_ID)

incs <- unique(data$INCI_ID)

set.seed(2026)

rows <- list()
master_counter <- 1

# Track per-person attributes
shot_status <- logical()         # named logical vector
incident_count <- integer()      # number of incidents per person
names(shot_status) <- character(0)
names(incident_count) <- character(0)

# Probability functions
shot_prob <- function(n_incidents) {
  # increases as person participates more
  min(0.10 + (n_incidents - 1) * 0.15, 0.95)
}

arrest_prob <- 0.85  # constant high probability

for (inc in incs) {
  
  # ---- Determine who is available (not shot yet) ----
  available_people <- names(shot_status)[shot_status == FALSE]
  
  # Number of participants this incident
  n_people <- sample(1:3, 1)
  
  # Returning participants
  n_returning <- rbinom(1, n_people, prob = 0.2)
  
  returning <- character(0)
  if (length(available_people) > 0 && n_returning > 0) {
    returning <- sample(available_people, min(n_returning, length(available_people)))
  }
  
  # New participants
  n_new <- n_people - length(returning)
  new_people <- character(0)
  if (n_new > 0) {
    new_people <- paste0("M", sprintf("%06d", master_counter:(master_counter + n_new - 1)))
    master_counter <- master_counter + n_new
    
    # initialize attributes
    shot_status[new_people] <- FALSE
    incident_count[new_people] <- 0L
  }
  
  # Combine
  people <- c(returning, new_people)
  
  # Update incident counts
  incident_count[people] <- incident_count[people] + 1L
  
  # Add the basic participant rows
  incident_rows <- data.frame(
    MasterID = people,
    INCI_ID = inc,
    shot = FALSE,
    arrested = FALSE,
    stringsAsFactors = FALSE
  )
  
  # ---- Determine shooting ----
  for (p in people) {
    if (!shot_status[p]) {
      p_shot <- rbinom(1, 1, shot_prob(incident_count[p])) == 1
      if (p_shot) {
        shot_status[p] <- TRUE
        incident_rows$shot[incident_rows$MasterID == p] <- TRUE
      }
    }
  }
  
  # ---- Determine arrests ----
  for (p in people) {
    p_arrest <- rbinom(1, 1, arrest_prob) == 1
    if (p_arrest) {
      incident_rows$arrested[incident_rows$MasterID == p] <- TRUE
    }
  }
  
  rows[[length(rows) + 1]] <- incident_rows
  
  # ---- Remove shot people from availability ----
  # (ensures they do not appear in future incidents)
  # shot_status always contains the correct boolean values now
}

mock_links <- bind_rows(rows)

# Merge with original data (optional)
mock_data <- data %>%
  left_join(mock_links, by = "INCI_ID")

write.csv(mock_data, "polRecArrestData_withMasterID.csv", row.names = FALSE)
