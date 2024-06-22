# Author: Hope Odubena
# Context: The English Premier League (EPL) is a distinguished soccer league in Great Britain, consisting of 20 teams.
# The season begins each year in August and concludes in May with each team playing each other team exactly twice (home and away).
# Each team plays 38 matches in a season while the total number of matches among all teams in a season is 380.
# A team receives 3 points for a win and if the game is tied, both teams receive 1 point; no points are awarded for a loss.
# At any point in the season, the team with the most points is the first-place team.
#
# Purpose: This script defines an R function called EPL_Standings that takes two parameters - date and season - 
# and returns the league standings for the specific date and season that a user specifies.
#
# User Input: EPL_Standings("04/25/2022", "2021/22") this function call shows the date & season inputs respectively and in the appropriate format.
#
# Output: 
# - record as wins-loses-ties (Record)
# - home record (HomeRec)
# - away record (AwayRec)
# - matches played (MatchesPlayed)
# - points (Points)
# - points per match (PPM)
# - point percentage = points / 3 * the number of games played, (PtPct)
# - goals scored (GS)
# - goals scored per match (GSM)
# - goals allowed (GA)
# - goals allowed per match (GAM)
# - record from last 10 games (Last10)
# - current match result streak (Streak)

# Load required libraries
library(readr)
library(tidyverse)
library(lubridate)
library(stringr)

# Validate date input
validate_date <- function(date) {
    if (!is.character(date) || is.na(mdy(date))) {
        stop("Invalid date format. Please provide a valid date in 'mm/dd/yyyy' format.")
    }
    return(mdy(date))
}

# Validate season input
validate_season <- function(season) {
    if (!is.character(season) || !grepl("^[0-9]{4}/[0-9]{2}$", season)) {
        stop("Invalid season format. Please provide a valid season in 'yyyy/yy' format.")
    }
    return(str_remove(str_sub(season, 3, 7), "\\D"))
}

# Load data
load_data <- function(season_string) {
    path <- paste0("https://www.football-data.co.uk/mmz4281/", season_string, "/E0.csv")
    tryCatch({
        df <- read_csv(url(path))
    }, error = function(e) {
        stop("Network error or invalid season. Please check the season and try again.")
    })
    return(df)
}

# Prepare data
prepare_data <- function(df, date) {
    df <- df %>% select(Date, HomeTeam:FTR)
    df$Date <- mdy(format.Date(dmy(df$Date), "%m/%d/%Y"))
    df <- filter(df, Date <= date)
    return(df)
}

# Convert match level data to team level data
convert_to_team_level <- function(df) {
    home_df <- df %>% select(TeamName = HomeTeam, everything())
    away_df <- df %>% select(TeamName = AwayTeam, everything())
    combined_df <- bind_rows(home_df, away_df)
    return(combined_df)
}

# Calculate overall records
calculate_overall_records <- function(combined_df) {
    combined_calc <- combined_df %>%
        group_by(TeamName) %>%
        summarise(
            HomeWins = sum(ifelse(is.na(HomeTeam) & FTR=="H", 1, 0)),
            HomeLosses = sum(ifelse(is.na(HomeTeam) & FTR=="A", 1, 0)),
            HomeTies = sum(ifelse(is.na(HomeTeam) & FTR=="D", 1, 0)),
            HGS = sum(ifelse(is.na(HomeTeam), FTHG, 0)),
            HGA = sum(ifelse(is.na(HomeTeam), FTAG, 0)),
            AwayWins = sum(ifelse(is.na(AwayTeam) & FTR=="A", 1, 0)),
            AwayLosses = sum(ifelse(is.na(AwayTeam) & FTR=="H", 1, 0)),
            AwayTies = sum(ifelse(is.na(AwayTeam) & FTR=="D", 1, 0)),
            AGS = sum(ifelse(is.na(AwayTeam), FTAG, 0)),
            AGA = sum(ifelse(is.na(AwayTeam), FTHG, 0))
        )
    return(combined_calc)
}

# Calculate last 10 records
calculate_last10_records <- function(combined_df) {
    last10_calc <- combined_df %>%
        arrange(desc(Date)) %>%
        group_by(TeamName) %>%
        mutate(Rank = row_number()) %>%
        filter(Rank <= 10) %>%
        summarise(
            Last10HomeWins = sum(ifelse(is.na(HomeTeam) & FTR=="H", 1, 0)),
            Last10HomeLosses = sum(ifelse(is.na(HomeTeam) & FTR=="A", 1, 0)),
            Last10HomeTies = sum(ifelse(is.na(HomeTeam) & FTR=="D", 1, 0)),
            Last10AwayWins = sum(ifelse(is.na(AwayTeam) & FTR=="A", 1, 0)),
            Last10AwayLosses = sum(ifelse(is.na(AwayTeam) & FTR=="H", 1, 0)),
            Last10AwayTies = sum(ifelse(is.na(AwayTeam) & FTR=="D", 1, 0))
        )
    return(last10_calc)
}

# Calculate additional columns
calculate_additional_columns <- function(combined_calc, last10_calc) {
    combined_calc <- full_join(combined_calc, last10_calc, by = "TeamName") %>%
        mutate(
            TotalWins = HomeWins + AwayWins,
            TotalLosses = HomeLosses + AwayLosses,
            TotalTies = HomeTies + AwayTies,
            MatchesPlayed = TotalWins + TotalLosses + TotalTies,
            Points = (TotalWins * 3) + TotalTies,
            PPM = Points / MatchesPlayed,
            PtPct = Points / (3 * MatchesPlayed),
            GS = HGS + AGS,
            GSM = GS / MatchesPlayed,
            GA = HGA + AGA,
            GAM = GA / MatchesPlayed,
            Last10Wins = Last10HomeWins + Last10AwayWins,
            Last10Losses = Last10HomeLosses + Last10AwayLosses,
            Last10Ties = Last10HomeTies + Last10AwayTies
        ) %>%
        mutate(
            Record = paste(TotalWins, TotalLosses, TotalTies, sep = "-"),
            HomeRec = paste(HomeWins, HomeLosses, HomeTies, sep = "-"),
            AwayRec = paste(AwayWins, AwayLosses, AwayTies, sep = "-"),
            Last10 = paste(Last10Wins, Last10Losses, Last10Ties, sep = "-")
        )
    return(combined_calc)
}

# Calculate streaks
calculate_streaks <- function(combined_df) {
    combined_df <- combined_df %>%
        mutate(FTR_standardized = case_when(
            is.na(HomeTeam) & FTR == 'H' ~ 'W',
            is.na(HomeTeam) & FTR == 'A' ~ 'L',
            is.na(HomeTeam) & FTR == 'D' ~ 'D',
            is.na(AwayTeam) & FTR == 'A' ~ 'W',
            is.na(AwayTeam) & FTR == 'H' ~ 'L',
            is.na(AwayTeam) & FTR == 'D' ~ 'D'
        )) %>%
        arrange(desc(Date))
    
    TeamName <- unique(combined_df$TeamName)
    streak_df <- data.frame(TeamName, Streak = rep(NA, length(TeamName)))
    
    for (team in TeamName) {
        filtered <- filter(combined_df, TeamName == team) %>% arrange(Date)
        streak_type <- 'W'
        streak_count <- 0
        
        for (result in filtered$FTR_standardized) {
            if (result == streak_type) {
                streak_count <- streak_count + 1
            } else {
                streak_type <- result
                streak_count <- 1
            }
        }
        streak_df$Streak[streak_df$TeamName == team] <- paste0(streak_type, streak_count)
    }
    return(streak_df)
}

# Generate final output
generate_output <- function(combined_calc, streak_df) {
    combined_calc <- full_join(combined_calc, streak_df, by = "TeamName")
    df_output <- combined_calc %>%
        arrange(desc(PPM), desc(TotalWins), desc(GSM), GAM) %>%
        select(
            TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM, Last10, Streak
        )
    return(df_output)
}

# Main function
EPL_Standings <- function(date, season) {
    date <- validate_date(date)
    season_string <- validate_season(season)
    df <- load_data(season_string)
    df <- prepare_data(df, date)
    combined_df <- convert_to_team_level(df)
    combined_calc <- calculate_overall_records(combined_df)
    last10_calc <- calculate_last10_records(combined_df)
    combined_calc <- calculate_additional_columns(combined_calc, last10_calc)
    streak_df <- calculate_streaks(combined_df)
    df_output <- generate_output(combined_calc, streak_df)
    return(df_output)
}

# Example usage
EPL_Standings("03/07/2022", "2021/22")
#EPL_Standings("04/23/2024", "2023/24")