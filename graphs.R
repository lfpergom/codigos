# this script generates the maps and graphs for the descriptive stats


install.packages("ggplot2")  # Install remotes package if not installed
library(ggplot2)

# Set working directory for saving output
setwd("D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/maps_graphs/graph")


# Load the dataset
load("D:/Consultoria/proyectos/NRC_research_analyst_consulyancy/data/Nigeria/data/nigeria_data.RData")

# Check what objects are available
ls()

# Print the structure of nigeria_data to confirm it contains age_gr, gender, and count
print(nigeria_data)
str(nigeria_data)  # Check the column names and types
head(nigeria_data)  # View first few rows

table(nigeria_data$gender)
table(nigeria_data$age_gr )

nigeria_data$gender <- as.factor(nigeria_data$gender)
nigeria_data$age_gr <- as.factor(nigeria_data$age_gr)


# age_gr X gender X LGA
ggplot(nigeria_data, aes(x = age_gr, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = 1), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = 0.9),  
            vjust = -0.5,  
            size = 3) +
  
  ggtitle("Age Group by Gender Across Locations") +
  xlab("Age Groups") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  facet_wrap(~ gender) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("age_gr_by_Gender_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# disp_year X gender X LGA
ggplot(nigeria_data, aes(x = year, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = 1), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = 0.9),  
            vjust = -0.2,  
            size = 3) +
  
  ggtitle("Displacement year by Gender Across Locations") +
  xlab("Year") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  facet_wrap(~ gender) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("disp_year_by_Gender_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# idp_hosting X gender X LGA
ggplot(nigeria_data, aes(x = idp_hosting, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = 1), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = 0.9),  
            vjust = -0.2,  
            size = 2) +
  
  ggtitle("IDP hosting by Gender Across Locations") +
  xlab("") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  facet_wrap(~ gender) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )


ggsave("num_of IDPhosted_by_Gender_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# schooling X gender X LGA


ggplot(nigeria_data, aes(x = schooling, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            size = 2) +
  
  ggtitle("Schooling (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  facet_wrap(~ gender) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("schooling_&_LGA.png", width = 10, height = 6, dpi = 300)

ggsave("schooling_by_Gender_&_LGA.png", width = 10, height = 6, dpi = 300)



################################################################################
# factor_to_move X gender X LGA
ggplot(nigeria_data, aes(x = factor_to_move, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = .9), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = .9),  
            vjust = .5, 
            hjust = -.3,
            size = 2.25) +
  
  ggtitle("Reasons to Leave Across Locations") +
  xlab("Reasons to leave") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 7, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 7, hjust = .9),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("reason_to_leave_&_LGA_1.png", width = 10, height = 6, dpi = 300)

################################################################################
# factor_to_move X gender X LGA
ggplot(nigeria_data, aes(x = factor_to_move, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = .9), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = .9),  
            vjust = .5, 
            hjust = -.3,
            size = 2.25) +
  
  ggtitle("Reasons to Leave Across Locations") +
  xlab("Reasons to leave") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 7, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 7, hjust = .9),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("reason_to_leave_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# ftm_howmany X gender X LGA
ggplot(nigeria_data, aes(x = ftm_howmany, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = 1), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = 0.9),  
            vjust = -0.2,  
            size = 3) +
  
  ggtitle("Number of Displacement by Gender Across Locations") +
  xlab("") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  facet_wrap(~ gender) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("num_of_disp_by_Gender_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# Housing
################################################################################
# housing_D1 X gender X LGA
ggplot(nigeria_data, aes(x = housing_D1, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = 1), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = 0.9),  
            vjust = -0.2, 
            hjust = .02,
            size = 3) +
  
  ggtitle("Pre-Displacement Housing Across Locations") +
  xlab("") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 7.5, hjust = 1),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("predisp_housing_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# housing_D3 X gender X LGA
ggplot(nigeria_data, aes(x = housing_D3, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = 1), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = 0.9),  
            vjust = -0.2, 
            hjust = .02,
            size = 2.5) +
  
  ggtitle("Post-Displacement Housing Across Locations") +
  xlab("") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 7.5, hjust = 1),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("postdisp_housing_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# housing_D5 X gender X LGA

nigeria_data$housing_D5

nigeria_data$housing_D5 <- gsub("Prefer not to answer  \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                "Prefer not to answer", 
                                nigeria_data$housing_D5)

nigeria_data$housing_D5


ggplot(nigeria_data, aes(x = housing_D5, fill = LGA_now)) +
  geom_bar(position = position_dodge(width = 1), color = "black") +
  
  # Add text labels above bars
  geom_text(aes(label = after_stat(count)), 
            stat = "count",  
            position = position_dodge(width = 0.9),  
            vjust = -0.2, 
            hjust = -.15,
            size = 3) +
  
  ggtitle("Housing satisfaction Compared to Before Displacement Across Locations") +
  xlab("") +
  ylab("") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  # Rotate X labels
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White figure background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Restore X and Y axes
  )
ggsave("housing_sat_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# Security
################################################################################
# security_E1 X gender X LGA

ggplot(nigeria_data, aes(x = security_E1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            size = 2) +
  
  ggtitle("Security Perception Compared to Before Displacement") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  facet_wrap(~ gender) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("security_percep_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# security_E2a_0 X gender X LGA

ggplot(nigeria_data, aes(x = security_E2a_0, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Exposure to Violence - IDPs") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("exp_violence_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# security_E15 X gender X LGA

ggplot(nigeria_data, aes(x = security_E15, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("IDP Arrival Effect on the Host Community - HOst") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_arrival_effect_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# security_E3_H X gender X LGA

ggplot(nigeria_data, aes(x = security_E3_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Relationship Between IDPs and the Host Community - Host") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_IDP_rel_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# security_E7_H X gender X LGA

ggplot(nigeria_data, aes(x = security_E7_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("IDPs and Host Community Tensions Resolution - Host") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_IDP_tension_sol_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# security_E16a_0 X gender X LGA

ggplot(nigeria_data, aes(x = security_E16a_0, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Exposure to Violence - Host Community") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("exp_violence_host_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# Health
################################################################################
# health_H1 X gender X LGA

ggplot(nigeria_data, aes(x = health_H1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Physical Health after Displacement (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_physical_healt_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# health_H222 X gender X LGA

ggplot(nigeria_data, aes(x = health_H222, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Food scarcity in the last 30 days due to lack of resources (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_food_scarcity_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# health_H224 X gender X LGA

ggplot(nigeria_data, aes(x = health_H224, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Going to sleep hungry in the last 30 days (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_going_to_sleep_hungry_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# health_H226 X gender X LGA

ggplot(nigeria_data, aes(x = health_H226, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Whole Day Without Eating in the last 30 days (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_whole_day_without_eating_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# health_H228 X gender X LGA

nigeria_data$health_H228

nigeria_data$health_H228 <- gsub("Prefer not to answer  \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                "Prefer not to answer", 
                                nigeria_data$health_H228)

nigeria_data$health_H228



ggplot(nigeria_data, aes(x = health_H228, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Assisstance Reception (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Assistance_reception_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# health_H3 X gender X LGA

nigeria_data$health_H3

nigeria_data$health_H3 <- gsub("\\{#progVars\\.inserts\\.INT_ST#}\\[INT: DO NOT READ OUT]\\{#progVars\\.inserts\\.INT_END#}", 
                               "Prefer not to answer", 
                               nigeria_data$health_H3)

nigeria_data$health_H3



ggplot(nigeria_data, aes(x = health_H3, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Health Access Compared to Before Displacement (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_health_access_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# health_H11_1 X gender X LGA

ggplot(nigeria_data, aes(x = health_H11_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Physical Health after Displacement (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_physical_healt_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# health_H222_H X gender X LGA

ggplot(nigeria_data, aes(x = health_H222_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Food scarcity in the last 30 days due to lack of resources (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_food_scarcity_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# health_H224_H X gender X LGA

ggplot(nigeria_data, aes(x = health_H224_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Going to sleep hungry in the last 30 days (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_going_to_sleep_hungry_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# health_H226_H X gender X LGA

ggplot(nigeria_data, aes(x = health_H226_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Whole Day Without Eating in the last 30 days (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_whole_day_without_eating_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# health_H228_H X gender X LGA

ggplot(nigeria_data, aes(x = health_H228_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Assisstance Reception (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Assistance_reception_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# health_H13 X gender X LGA

ggplot(nigeria_data, aes(x = health_H13, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Health Access Compared to Before Displacement (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_health_access_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# Livelihoods
################################################################################
# livelihhods_G01 X gender X LGA

ggplot(nigeria_data, aes(x = livelihhods_G01, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Income Earners Before Displacement (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Income_earners_beforedisp_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G01_bis X gender X LGA

ggplot(nigeria_data, aes(x = livelihhods_G01_bis, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Main Sources of Income before Displacement (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_sources_income_beforedisp_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G3_1 X gender X LGA


nigeria_data$livelihhods_G3_1

nigeria_data$livelihhods_G3_1 <- gsub("\\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                               "I do not work", 
                               nigeria_data$livelihhods_G3_1)
nigeria_data$livelihhods_G3_1

nigeria_data$livelihhods_G3_1

nigeria_data$livelihhods_G3_1 <- gsub("\\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                      "No other household members work", 
                                      nigeria_data$livelihhods_G3_1)
nigeria_data$livelihhods_G3_1


ggplot(nigeria_data, aes(x = livelihhods_G3_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Personal Income from work (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_personal_income_work_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G3_2 X gender X LGA


nigeria_data$livelihhods_G3_2

nigeria_data$livelihhods_G3_2 <- gsub("\\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                      "I do not work", 
                                      nigeria_data$livelihhods_G3_2)
nigeria_data$livelihhods_G3_2

nigeria_data$livelihhods_G3_2

nigeria_data$livelihhods_G3_2 <- gsub("\\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                      "No other household members work", 
                                      nigeria_data$livelihhods_G3_2)
nigeria_data$livelihhods_G3_2


ggplot(nigeria_data, aes(x = livelihhods_G3_2, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Household Income from work (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_HH_income_work_&_LGA.png", width = 10, height = 6, dpi = 300)



################################################################################
# livelihhods_G1 X gender X LGA



ggplot(nigeria_data, aes(x = livelihhods_G1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Income Earners After Displacement (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_HH_income_work_afterdisp_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# livelihhods_G1_bis X gender X LGA

ggplot(nigeria_data, aes(x = livelihhods_G1_bis, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Sources of Income After Displacement (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_HH_sources_income_afterdisp_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# livelihhods_G17 X gender X LGA


nigeria_data$livelihhods_G17

nigeria_data$livelihhods_G17 <- gsub("Prefer not to answer \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                      "Prefer not to answer", 
                                      nigeria_data$livelihhods_G17)
nigeria_data$livelihhods_G17


nigeria_data$livelihhods_G17

nigeria_data$livelihhods_G17 <- gsub("Prefer not to answer  Prefer not to answer", 
                                     "Prefer not to answer", 
                                     nigeria_data$livelihhods_G17)
nigeria_data$livelihhods_G17



  
  
ggplot(nigeria_data, aes(x = livelihhods_G17, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Financial Resources Sufficiency Before Displacement (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Financial_resources_beforedisp_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G18 X gender X LGA


nigeria_data$livelihhods_G18

nigeria_data$livelihhods_G18 <- gsub("Prefer not to answer  \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                     "Prefer not to answer", 
                                     nigeria_data$livelihhods_G18)
nigeria_data$livelihhods_G18



ggplot(nigeria_data, aes(x = livelihhods_G18, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Financial Resources Sufficiency Now (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Financial_resources_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G40_1 X gender X LGA


nigeria_data$livelihhods_G40_1

nigeria_data$livelihhods_G40_1 <- gsub("Prefer not to answer  \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                     "Prefer not to answer", 
                                     nigeria_data$livelihhods_G40_1)
nigeria_data$livelihhods_G40_1



ggplot(nigeria_data, aes(x = livelihhods_G40_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Afraid of Land Dispossession (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_land_dispossession_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_AG11 X gender X LGA


nigeria_data$livelihhods_G40_1

nigeria_data$livelihhods_G40_1 <- gsub("Prefer not to answer  \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                       "Prefer not to answer", 
                                       nigeria_data$livelihhods_G40_1)
nigeria_data$livelihhods_G40_1



ggplot(nigeria_data, aes(x = livelihhods_AG11, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Harvest Sufficient for HH Income/Food needs (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_harvest_sufficiency_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# livelihhods_AG16_1 X gender X LGA


nigeria_data$livelihhods_AG16_1

nigeria_data$livelihhods_AG16_1 <- gsub("Prefer not to answer  \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                       "Prefer not to answer", 
                                       nigeria_data$livelihhods_G40_1)
nigeria_data$livelihhods_AG16_1



ggplot(nigeria_data, aes(x = livelihhods_AG16_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Potential Income Generating Activities (IDPs)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_income_gen_act_&_LGA.png", width = 10, height = 6, dpi = 300)












################################################################################
# livelihhods_G19 X gender X LGA

ggplot(nigeria_data, aes(x = livelihhods_G19, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("HH Income Earners Before Displacement (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Income_earners_beforedisp_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G19_bis X gender X LGA

ggplot(nigeria_data, aes(x = livelihhods_G19_bis, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Main Sources of Income before Displacement (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_sources_income_beforedisp_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G25 X gender X LGA


ggplot(nigeria_data, aes(x = livelihhods_G25, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("HH Income Earners Now (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_HH_income_work_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)



################################################################################
# livelihhods_G33 X gender X LGA


nigeria_data$livelihhods_G33

nigeria_data$livelihhods_G33 <- gsub("Don't know \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                     "Don't know", 
                                     nigeria_data$livelihhods_G33)
nigeria_data$livelihhods_G33


ggplot(nigeria_data, aes(x = livelihhods_G33, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Financial Resources Sufficiency Before Displacement (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Financial_resources_beforedisp_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G34 X gender X LGA


ggplot(nigeria_data, aes(x = livelihhods_G34, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Financial Resources Sufficiency Now (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Financial_resources_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_G40_H X gender X LGA


nigeria_data$livelihhods_G40_H

nigeria_data$livelihhods_G40_H <- gsub("Prefer not to answer  \\{#progVars.inserts.INT_ST#\\}\\[INT: DO NOT READ OUT\\]\\{#progVars.inserts.INT_END#\\}", 
                                       "Prefer not to answer", 
                                       nigeria_data$livelihhods_G40_H)
nigeria_data$livelihhods_G40_H



ggplot(nigeria_data, aes(x = livelihhods_G40_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Afraid of Land Dispossession (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_land_dispossession_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# livelihhods_AG11_H X gender X LGA


ggplot(nigeria_data, aes(x = livelihhods_AG11_H, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Harvest Sufficient for HH Income/Food needs (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_harvest_sufficiency_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# livelihhods_AG16_H_1 X gender X LGA


ggplot(nigeria_data, aes(x = livelihhods_AG16_H_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Potential Income Generating Activities (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_income_gen_act_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# Education
################################################################################
# education_idp_F81_1

ggplot(nigeria_data, aes(x = education_idp_F81_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys School Disruption due to Displacement (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_boy_school_disruption_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_idp_F81_2

ggplot(nigeria_data, aes(x = education_idp_F81_2, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls School Disruption due to Displacement (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Girl_school_disruption_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_idp_F81a_1

ggplot(nigeria_data, aes(x = education_idp_F81a_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys Length of School Disuption (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_boy_length_disruption_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_idp_F8a1_2

ggplot(nigeria_data, aes(x = education_idp_F81a_2, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls Length of School Disuption (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Girl_length_disruption_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_idp_F9_1

ggplot(nigeria_data, aes(x = education_idp_F9_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys School Attendance Now (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_boy_school_attendance_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_idp_F9_2

ggplot(nigeria_data, aes(x = education_idp_F9_2, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls School Attendance Now (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Girl_school_attendance_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_idp_F13_1

ggplot(nigeria_data, aes(x = education_idp_F13_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys Education Satisfaction Compared to Before Displacement (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_boy_education_satisfaction_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_idp_F13_2

ggplot(nigeria_data, aes(x = education_idp_F13_2, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls Education Satisfaction Compared to Before Displacement (IDP)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("IDP_Girl_education_satisfaction_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# education_host_F15_1

ggplot(nigeria_data, aes(x = education_host_F15_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys School Attendance Before IDPs arrival (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_boy_education_attendance_beforeIDPs_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_host_F15_1

ggplot(nigeria_data, aes(x = education_host_F15_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls School Attendance Before IDPs arrival (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Girl_education_attendance_beforeIDPs_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_host_F181_1

ggplot(nigeria_data, aes(x = education_host_F181_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys School Disruption due to IDPs arrival (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_boy_education_attendance_beforeIDPs_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_host_F181_1

ggplot(nigeria_data, aes(x = education_host_F181_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls School Disruption due to IDPs arrival (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Girl_education_attendance_beforeIDPs_&_LGA.png", width = 10, height = 6, dpi = 300)

################################################################################
# education_host_F19_1

ggplot(nigeria_data, aes(x = education_host_F19_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys School Attendance Now (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_boy_school_attendance_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_host_F19_2

ggplot(nigeria_data, aes(x = education_host_F19_2, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls School Attendance Now (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Girl_school_Attendance_NOW_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_host_F22_1

ggplot(nigeria_data, aes(x = education_host_F22_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Boys Education Satisfaction  Now Vs before IDPs arrival (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_boy_educ_satisfaction_nowVSbeforeIDP_&_LGA.png", width = 10, height = 6, dpi = 300)


################################################################################
# education_host_F22_2

ggplot(nigeria_data, aes(x = education_host_F22_2, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Girls Education Satisfaction Now Vs before IDPs arrival (Host)") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("Host_Girl_educ_satisfaction_nosVSbeforeIDP_&_LGA.png", width = 10, height = 6, dpi = 300)






























 

################################################################################
# security_E2a_1 X gender X LGA

ggplot(nigeria_data, aes(x = security_E2a_1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Ability to produce or access food due to violence - IDPs") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("access_food_&_LGA.png", width = 10, height = 6, dpi = 300)



















































################################################################################
# security_E3 X gender X LGA

ggplot(nigeria_data, aes(x = security_E3, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", position = position_dodge(width = 1), color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), label = scales::percent(after_stat(prop), accuracy = 1), group = LGA_now), 
            stat = "count",  
            position = position_dodge(width = 1),  
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Relationship with Host Community") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  #facet_wrap(~ gender) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Rotate X labels
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("rel_host_&_LGA.png", width = 10, height = 6, dpi = 300)







ggplot(nigeria_data, aes(x = security_E1, fill = LGA_now)) +
  geom_bar(aes(y = after_stat(prop), group = LGA_now), 
           stat = "count", 
           position = position_dodge2(width = 1, preserve = "single"),  # Keeps bar width uniform
           color = "black") +
  
  # Add text labels showing percentages
  geom_text(aes(y = after_stat(prop), 
                label = scales::percent(after_stat(prop), accuracy = 1), 
                group = LGA_now), 
            stat = "count",  
            position = position_dodge2(width = 1, preserve = "single"),  # Keeps bar width uniform
            vjust = -0.5, 
            hjust = -.15,
            size = 2) +
  
  ggtitle("Security Perception Compared to Before Displacement") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  coord_flip() +
  #facet_wrap(~ gender) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format())  # Convert Y-axis to percentages

ggsave("security_percep_&_LGA.png", width = 10, height = 6, dpi = 300)







library(dplyr)
library(ggplot2)
library(scales)

# Compute proportions manually
nigeria_data_filtered <- nigeria_data %>%
  count(security_E1, LGA_now) %>%  # Get counts
  group_by(security_E1) %>%  
  mutate(prop = n / sum(n)) %>%  # Calculate proportions
  filter(prop > 0)  # Remove rows with 0% proportion


ggplot(nigeria_data_filtered, aes(x = security_E1, y = prop, fill = LGA_now)) +
  geom_col(position = position_dodge2(width = 1, preserve = "single"), color = "black") +
  
  # Add text labels showing percentages (only if > 0%)
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),  
            position = position_dodge2(width = 1, preserve = "single"),  
            vjust = -0.5, 
            hjust = -0.15,
            size = 2) +
  
  ggtitle("Security Perception Compared to Before Displacement") +
  xlab("") +
  ylab("Proportion") +
  labs(fill = "LGA") +
  
  # Define colors for each LGA
  scale_fill_manual(values = c("Tureta" = "darkred", "Tangaza" = "darkgreen")) +  
  
  # White background + minimal theme
  theme_minimal() +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 8, hjust = 1),  
    panel.background = element_rect(fill = "white", color = NA),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black")  
  ) +
  scale_y_continuous(labels = scales::percent_format()) 