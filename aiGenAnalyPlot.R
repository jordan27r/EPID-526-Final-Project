# --- Load libraries ---
library(dplyr)
library(igraph)
library(tidyr)

# --- Load your enriched dataset ---
data <- read.csv("C://Users//jorda//OneDrive//Documents//EPID-526-Final-Project//polRecArrestData_withMasterID.csv", stringsAsFactors = FALSE)

# Keep just the key identifiers
df <- data %>%
  select(MasterID, INCI_ID, ArrestID, WEAPON_CATEGORY, CrimeType, CrimeCategory,
         WARD, ArrestDate, ArrestCharge) %>%
  distinct()
# All pairs of people who appear in the same incident
edges <- df %>%
  select(INCI_ID, MasterID) %>%
  group_by(INCI_ID) %>%
  filter(n() > 1) %>%
  summarise(pairs = combn(MasterID, 2, simplify = FALSE)) %>%
  # summarise(pairs = ifelse(n()>1, combn(MasterID, 2, simplify = FALSE), as.list(paste(MasterID), "NA"))) %>%
  pull(pairs) %>%
  do.call(rbind, .) %>%
  as.data.frame(stringsAsFactors = FALSE)

colnames(edges) <- c("from", "to")

# Optional: remove duplicate or self edges
edges <- edges %>%
  filter(from != to) %>%
  distinct() %>% 
  right_join(data %>% select(MasterID), by=join_by(from == MasterID)) %>% 
  distinct() %>% 
  filter(!is.na(to))

g <- graph_from_data_frame(edges, directed = FALSE)

# Add attributes
V(g)$type <- ifelse(V(g)$name %in% df$MasterID[!is.na(df$ArrestID)], "Arrested", "NotArrested")

# Basic network stats
cat("Number of people:", vcount(g), "\n")
cat("Number of connections:", ecount(g), "\n")
cat("Average degree:", mean(degree(g)), "\n")
# Simple visualization
set.seed(123)
plot(
  g,
  vertex.size = log(degree(g) + 1) * 5,
  vertex.color = ifelse(V(g)$type == "Arrested", "red", "skyblue"),
  vertex.label = NA,
  main = "Gun Violence Involvement Network"
)

# save edges file
data  <- df %>% 
   left_join(edges, by=join_by(MasterID == from)) %>% 
  mutate(arrested = ifelse(is.na(ArrestID), 0, 1))

write.csv(edges, "gunViolenceEdges.csv")
write.csv(df, "gunViolenceCleanData.csv")
