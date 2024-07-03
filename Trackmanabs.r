  output$pitchingSummaryTable <- renderDT({
    pitching_summary <- filtered_data() %>%
      summarise(
        Pitcher = ifelse(input$pitcher == "All", "All", input$pitcher),
        P = n(),
        PA = sum(case_when(KorBB %in% c('Strikeout', 'Walk') | PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun', 'Out') | PitchCall == 'HitByPitch' ~ 1, TRUE ~ 0)),
        AB = sum(case_when(KorBB == 'Strikeout' | PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun', 'Out') ~ 1, TRUE ~ 0)),
        IP = round(sum(OutsOnPlay) / 3, 1),
        H = sum(case_when(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun') ~ 1, TRUE ~ 0)),
        AVG = round(H / sum(case_when(KorBB == 'Strikeout' | PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun', 'Out') ~ 1, TRUE ~ 0)), 3),
        BB = sum(case_when(KorBB == 'Walk' ~ 1, TRUE ~ 0)),
        HBP = sum(case_when(PitchCall == 'HitByPitch' ~ 1, TRUE ~ 0)),
        K = sum(case_when(KorBB == 'Strikeout' ~ 1, TRUE ~ 0)),
        `K/BB` = round(K / BB, 2),
        HR = sum(case_when(PlayResult == 'HomeRun' ~ 1, TRUE ~ 0)),
        FIP = round(((13 * HR) + (3 * (BB + HBP)) - (2 * K)) / IP + 3.5, 2),
        `1PS%` = round(sum(case_when(PitchCall %in% c('InPlay', 'FoulBallNotFieldable', 'FoulBallFieldable', 'StrikeCalled', 'StrikeSwinging') & Strikes == 0 & Balls == 0 ~ 1, TRUE ~ 0)) / sum(case_when(Strikes == 0 & Balls == 0 ~ 1, TRUE ~ 0)) * 100, 1),
        `1PZ%` = round(sum(case_when(Strikes == 0 & Balls == 0 & PlateLocHeight >= 1.7 & PlateLocHeight <= 3.4 & PlateLocSide >= -1.05 & PlateLocSide <= 1.05 ~ 1, TRUE ~ 0)) / sum(case_when(Strikes == 0 & Balls == 0 ~ 1, TRUE ~ 0)) * 100, 1),
        `1P-Swing%` = round(sum(case_when(PitchCall %in% c('InPlay', 'FoulBallNotFieldable', 'FoulBallFieldable', 'StrikeSwinging') & Strikes == 0 & Balls == 0 ~ 1, TRUE ~ 0)) / sum(case_when(Strikes == 0 & Balls == 0 ~ 1, TRUE ~ 0)) * 100, 1)
      )
