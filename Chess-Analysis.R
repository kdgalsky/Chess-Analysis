###  Loading in necessary packages ###

suppressMessages(library(chessR)) 
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(caTools))
suppressMessages(library(caret))
suppressMessages(library(nnet))
suppressMessages(library(ggplot2))

# Scraping data from Chess.com using chessR package 

chessdotcom_game_data <- get_game_data(usernames = "kgalsky")
tail(chessdotcom_game_data)
ls(chessdotcom_game_data) 


### Data Preparation ###

# Select relevant columns

chessdotcom_game_data_clean <- chessdotcom_game_data %>%
  select(UserELO, OpponentELO, n_Moves, Opening, Termination, UserResult)

# Modifying variables

chessdotcom_game_data$UserResult <- factor(chessdotcom_game_data$UserResult, levels = c("Win", "Loss", "Draw"))
chessdotcom_game_data$Opening <- as.factor(chessdotcom_game_data$Opening)
chessdotcom_game_data$Termination <- as.factor(chessdotcom_game_data$Termination)

chessdotcom_game_data_clean$Opening_Group <- case_when(
  grepl("Knight", chessdotcom_game_data_clean$Opening) ~ "Knight Openings",
  grepl("Pawn", chessdotcom_game_data_clean$Opening) ~ "Pawn Openings",
  grepl("Defense", chessdotcom_game_data_clean$Opening) ~ "Defense Openings",
  grepl("Gambit", chessdotcom_game_data_clean$Opening) ~ "Gambits",
  grepl("Fianchetto", chessdotcom_game_data_clean$Opening) ~ "Fianchetto Openings",
  grepl("System", chessdotcom_game_data_clean$Opening) ~ "Systematic Openings",
  TRUE ~ "Other Opening Types"
)

chessdotcom_game_data_clean <- chessdotcom_game_data_clean %>%
  mutate(Termination_Group = case_when(
    grepl("checkmate", Termination, ignore.case = TRUE) ~ "Checkmate",
    grepl("time", Termination, ignore.case = TRUE) ~ "Time",
    grepl("resignation", Termination, ignore.case = TRUE) ~ "Resignation",
    grepl("abandoned", Termination, ignore.case = TRUE) ~ "Abandoned",
    grepl("stalemate", Termination, ignore.case = TRUE) ~ "Stalemate",
    grepl("insufficient material", Termination, ignore.case = TRUE) ~ "Insufficient Material",
    grepl("repetition", Termination, ignore.case = TRUE) ~ "Repetition",
    TRUE ~ "Other"
  ))

chessdotcom_game_data_clean <- chessdotcom_game_data_clean %>%
  select(-Opening)

chessdotcom_game_data_clean <- chessdotcom_game_data_clean %>%
  select(-Termination)

head(chessdotcom_game_data_clean)



### Multivariate Logistic Regression ###

set.seed(37)

split <- sample.split(chessdotcom_game_data_clean$UserResult, SplitRatio = 0.8)

train_data <- subset(chessdotcom_game_data_clean, split == TRUE)
test_data <- subset(chessdotcom_game_data_clean, split == FALSE)

table(train_data$UserResult)
table(test_data$UserResult)


logit_model_2 <- multinom(UserResult ~ UserELO + OpponentELO + n_Moves + 
                            Opening_Group,
                          data = train_data)

summary(logit_model_2)



predictions_2 <- predict(logit_model_2, test_data, type = "class")

table(Predicted = predictions_2, Actual = test_data$UserResult)


probabilities_2 <- predict(logit_model_2, test_data, type = "probs")

probabilities_percent_2 <- round(probabilities_2 * 100, 2)

probabilities_percent_df_2 <- as.data.frame(probabilities_percent_2)

probabilities_percent_df_2$Actual_Result <- test_data$UserResult

print(probabilities_percent_df_2)


predicted_class_indices_2 <- max.col(probabilities_percent_df_2[, 1:3], ties.method = "first")

predicted_classes_2 <- colnames(probabilities_percent_df_2)[predicted_class_indices_2]

probabilities_percent_df_2$Predicted_Class <- predicted_classes_2

accuracy <- mean(probabilities_percent_df_2$Predicted_Class == probabilities_percent_df_2$Actual_Result, na.rm = TRUE)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%"))



### Creating Visualizations used in Article ###

# Bar graph for opening and result

ggplot(train_data, aes(x = Opening_Group, fill = UserResult)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of User Result by Opening Group",
       x = "Opening Group", y = "Proportion") +
  scale_fill_manual(values = c("Win" = "green", "Loss" = "red", "Draw" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar graph for ending and result

ggplot(train_data, aes(x = Termination_Group, fill = UserResult)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of User Result by Termination (Ending) Group",
       x = "Termination Group", y = "Proportion") +
  scale_fill_manual(values = c("Win" = "green", "Loss" = "red", "Draw" = "blue")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box-and-whisker for number of moves and result

ggplot(train_data, aes(x = UserResult, y = n_Moves, fill = UserResult)) +
  geom_boxplot() +
  labs(title = "Distribution of Number of Moves by User Result",
       x = "User Result", y = "Number of Moves") +
  scale_fill_manual(values = c("Win" = "green", "Loss" = "red", "Draw" = "blue")) +
  theme_minimal()

