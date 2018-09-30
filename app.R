library(DT)
library(glue)
library(shiny)
library(shinyjs)
library(leaflet)
library(tidyverse)
library(geojsonio)
library(shinyalert)
library(shinymaterial)
library(collapsibleTree)

rm(list = ls())

ncsc_colors <- c('gold' = '#84754E', 
                 'gold2' = '#AD841F', 
                 'blue' = '#1B365D', 
                 'lightblue' = '#7FA9AE',
                 'red' = '#6F263D',
                 'brown' = '#5C462B',
                 'grey' = '#7E7F74',
                 'green' = '#67823A',
                 'teal' = '#115E67',
                 'orange' = '#B86125'
)

# Read in data ------------------------------------------------------------
files <- list.files('data', full.names = T)

map(files, function(file) {
    filename_without_extension <- str_remove_all(file, '\\.csv|data/')
    tmp <- read_csv(file)
    if ('DisplayOrder' %in% names(tmp)) {
        tmp <- arrange(tmp, DisplayOrder)
    }
    assign(filename_without_extension, tmp, .GlobalEnv)
})

PopulationCategory <- PopulationCategory_Fixed_Alex
Court <- Court_Fixed_Alex

# states <- geojson_read('http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json', what = 'sp')
# saveRDS(states, 'states_json.rds')
states <- readRDS('states_json.rds')

make_state_network <- function(df, state_name) {
    
    state_df <- df %>% 
        filter(USStateName == state_name) %>% 
        select(ParentCourtID, CourtNameName, ChildCourtID, CourtLevelID, CourtLevelRank) %>% 
        left_join(., ., by = c('ParentCourtID' = 'ChildCourtID')) %>% 
        select(ParentCourt = CourtNameName.y, CourtName = CourtNameName.x, CourtLevel = CourtLevelID.x, CourtLevelRank = CourtLevelRank.x) %>% 
        unique %>% 
        filter(!is.na(CourtName)) %>% 
        mutate(Color = factor(CourtLevel))
    
    levels(state_df$Color) <- ncsc_colors[c('red', 'green', 'teal', 'gold2')]
    
    colrs <- c(state_df$CourtName[state_df$CourtLevel == 'COLR'])
    
    if (length(colrs) > 1) {
        map(colrs, function(x) {
            state_df %>% 
                filter(!CourtName %in% colrs[colrs != x], !ParentCourt %in% colrs[colrs != x]) %>% 
                collapsibleTreeNetwork(attribute = 'CourtLevel',
                                       nodeSize = 'leafCount',
                                       fill = 'Color',
                                       collapsed = FALSE,
                                       zoomable = FALSE)
        })
    } else {
        list(
            state_df %>% 
                collapsibleTreeNetwork(attribute = 'CourtLevel',
                                       nodeSize = 'leafCount',
                                       fill = 'Color',
                                       collapsed = FALSE,
                                       zoomable = FALSE)
        )
    }

}

tots <- USState %>% 
    left_join(USStateCourt, by = 'USStateID') %>% 
    left_join(Court, by = 'CourtID') %>% 
    left_join(CourtCourtName, by = 'CourtID') %>% 
    left_join(CourtName, by = 'CourtNameID') %>% 
    left_join(PanelDecision, by = 'PanelDecisionID') %>% 
    left_join(CaseManagement, by = 'CaseManagementID') %>% 
    left_join(AppealProcess, by = c('CourtID' = 'ParentCourtID')) %>% 
    left_join(Funding, by = 'FundingID') %>% 
    rename(ParentCourtID = CourtID) %>% 
    left_join(ChildCourt, by = 'ChildCourtID') %>% 
    left_join(CourtLevel %>% mutate(CourtLevelRank = max(DisplayOrder) + 1 - DisplayOrder), by = 'CourtLevelID')

tots_cases <- tots %>% 
    select(USStateName, CourtID = ParentCourtID, CourtName = CourtNameName, CourtNotes = Notes) %>% 
    distinct %>% 
    left_join(CourtCaseType, by = 'CourtID') %>% 
    rename(CourtCaseNotes = Notes) %>% 
    left_join(CaseType, by = 'CaseTypeID')

css <- glue("
    .nav-wrapper {
        background-color: {{ncsc_colors['blue']}};
    }


    .shiny-material-tab-content, html {
        background-color: {{ncsc_colors['grey']}};
    }

    .tabs {
        background-color: #263f6a;
    }

    .dropdown-content li>a, .dropdown-content li>span {
        color: {{ncsc_colors['teal']}};
    }

    .row {
        margin: 0px;
    }
    
    .brand-logo {
        width: 100%;
    }
    
    h1 { font-family: TimesNewRoman, 'Times New Roman', Times, Baskerville, Georgia, serif; font-size: 24px; font-style: normal; font-variant: normal; font-weight: 700; line-height: 26.4px; } 
    h3 { font-family: TimesNewRoman, 'Times New Roman', Times, Baskerville, Georgia, serif; font-size: 14px; font-style: normal; font-variant: normal; font-weight: 700; line-height: 15.4px; } 
    p { font-family: TimesNewRoman, 'Times New Roman', Times, Baskerville, Georgia, serif; font-size: 14px; font-style: normal; font-variant: normal; font-weight: 400; line-height: 20px; } 
    blockquote { font-family: TimesNewRoman, 'Times New Roman', Times, Baskerville, Georgia, serif; font-size: 21px; font-style: normal; font-variant: normal; font-weight: 400; line-height: 30px; } 
    pre { font-family: TimesNewRoman, 'Times New Roman', Times, Baskerville, Georgia, serif; font-size: 13px; font-style: normal; font-variant: normal; font-weight: 400; line-height: 18.5714px; }
    html {
        font-family: TimesNewRoman, 'Times New Roman', Times, Baskerville, Georgia, serif;
    }

    #court_table {white-space: nowrap;  }

", .open = '{{', .close = '}}')

ui <- material_page(
    title = div(style = 'display:inline-block;width:99%;',
                a(href = 'http://www.courtstatistics.org/', target = '_blank', img(src="style/csp-logo.png", width = '10%')),
                a('US State Court Explorer', style = 'display:inline-block;width:80%;text-align:center;vertical-align:top;')
    ),
    inlineCSS(css),
    material_tabs(
        tabs = c(
            'State court overview' = 'overview',
            'Compare two states' = 'states'
        ),
        color = 'white'
    ),
    
    material_tab_content(
        tab_id = 'overview',
        material_row(
            material_column(width = 12,
                            material_card(title = 'Use the filters to narrow down on states',
                                          depth = 5,
                                          div(style = 'display:inline-block',
                                              div(style = 'display:inline-block', material_dropdown('trial_structure', 'Trial Structure', c('All', TrialStructure$TrialStructureDescription))),
                                              div(style = 'display:inline-block', material_dropdown('trial_criminal_processing', 'Trial Criminal Processing', c('All', TrialCriminalProcessing$TrialCriminalProcessingDescription))),
                                              div(style = 'display:inline-block', material_dropdown('population_density', 'Population Density', c('All', PopulationDensity$PopulationDensityDescription))),
                                              div(style = 'display:inline-block', material_dropdown('population_category', 'Population Category', c('All', PopulationCategory$PopulationCategoryDescription))),
                                              div(style = 'display:inline-block', material_dropdown('rural', 'Rural', c('All', Rural$RuralDescription))),
                                              div(style = 'display:inline-block', material_dropdown('death_penalty', 'Death Penalty', c('All', DeathPenalty$DeathPenaltyDescription))),
                                              div(style = 'display:inline-block', material_dropdown('caseload_size', 'Caseload Size', c('All', CaseloadSize$CaseloadSizeDescription))),
                                              div(style = 'display:inline-block', material_dropdown('appellate_criminal_structure', 'Appellate Criminal Structure', c('All', AppellateCriminalStructure$AppellateCriminalStructureDescription)))
                                          )
                            )
            )
        ),
        material_row(
            material_column(width = 6,
                            material_card(title = 'Click on a state to learn more',
                                          depth = 5,
                                          leafletOutput('map', height = 490)
                            )),
            material_column(width = 6,
                            material_row(uiOutput('state_drilldown')),
                            material_row(uiOutput('court_drilldown'))
            )
        )
    ),
    material_tab_content(
        tab_id = 'states',
        material_row(
            material_column(width = 6,
                            material_row(
                                material_card(depth = 5,
                                              material_dropdown('state1', 'State #1',
                                                                choices = unique(tots$USStateName), 
                                                                selected = unique(tots$USStateName)[1]),
                                              uiOutput('state1_network')
                                )),
                            material_row(
                                material_card(depth = 5,
                                              material_dropdown('state2', 'State #2', 
                                                                choices = unique(tots$USStateName), 
                                                                selected = unique(tots$USStateName)[2]),
                                              uiOutput('state2_network')
                                ))
            ),
            material_column(width = 6,
                            material_card(
                                title = '',
                                depth = 5,
                                dataTableOutput('state_comparison_table')
                            )
                            )
        )
        
    ),
    material_tab_content(
        tab_id = 'cases'
    )
)

server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            setView(-96, 37.8, 4) %>%
            addTiles()
    })
    
    filter_listener <- reactive({
        list(
            input$trial_structure,
            input$trial_criminal_processing,
            input$appellate_criminal_structure,
            input$rural,
            input$death_penalty,
            input$population_density,
            input$population_category,
            input$caseload_size
        )
    })
    
    filter_states <- eventReactive(filter_listener(), {
        
        states_filtered <- states
        
        if (input$trial_structure != 'All') {
            states_to_include <- TrialStructure %>% 
                filter(TrialStructureDescription == input$trial_structure) %>%
                left_join(USState, by = 'TrialStructureID') %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        if (input$appellate_criminal_structure != 'All') {
            states_to_include <- AppellateCriminalStructure %>% 
                filter(AppellateCriminalStructureDescription == input$appellate_criminal_structure) %>%
                left_join(USState, by = 'AppellateCriminalStructureID') %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        if (input$trial_criminal_processing != 'All') {
            states_to_include <- TrialCriminalProcessing %>% 
                filter(TrialCriminalProcessingDescription == input$trial_criminal_processing) %>%
                left_join(USState, by = 'TrialCriminalProcessingID') %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        if (input$rural != 'All') {
            states_to_include <- Rural %>% 
                filter(RuralDescription == input$rural) %>%
                left_join(USState, by = 'RuralID') %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        if (input$population_density != 'All') {
            states_to_include <- PopulationDensity %>% 
                filter(PopulationDensityDescription == input$population_density) %>%
                left_join(USState, by = 'PopulationDensityID') %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        if (input$death_penalty != 'All') {
            states_to_include <- DeathPenalty %>% 
                filter(DeathPenaltyDescription == input$death_penalty) %>%
                left_join(USState, by = 'DeathPenaltyID') %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        if (input$caseload_size != 'All') {
            states_to_include <- CaseloadSize %>% 
                filter(CaseloadSizeDescription == input$caseload_size) %>%
                left_join(USState, by = c('CaseloadSizeID' = 'TrialCaseloadSizeID')) %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        if (input$population_category != 'All') {
            states_to_include <- PopulationCategory %>% 
                filter(PopulationCategoryDescription == input$population_category) %>%
                left_join(USState, by = 'PopulationCategoryID') %>% 
                pull(USStateName)
            states_filtered <- states_filtered[states_filtered$NAME %in% states_to_include, ]
        }
        
        return(states_filtered)
    })
    
    observe({
        leafletProxy('map', session, data = filter_states()) %>% 
            clearShapes() %>% 
            addPolygons(
                weight = 2,
                opacity = 1,
                color = 'white',
                fillOpacity = 1,
                fillColor = ncsc_colors[['lightblue']],
                layerId = ~NAME,
                label = ~NAME,
                highlight = highlightOptions(
                    weight = 5,
                    color = ncsc_colors[['gold2']],
                    fillOpacity = 1,
                    bringToFront = TRUE
                ))
    })
    
    observeEvent(input$map_shape_click, {
        
        clicked_state <- input$map_shape_click$id
        
        output$network <- renderUI({
            
            state_networks <- make_state_network(tots, clicked_state)
            
            map(1:length(state_networks), function(x) {
                output[[str_glue('overview_network_{x}')]] <- renderCollapsibleTree({
                    state_networks[[x]]
                })
            })
            
            tagList(
                map(1:length(state_networks), function(x) {
                    collapsibleTreeOutput(str_glue('overview_network_{x}'), height = str_glue('{225/length(state_networks)}px'))
                })
            )
            
        })
        
        
        output$state_drilldown <- renderUI({
            material_column(width = 12,
                            material_card(title = str_glue('{input$map_shape_click$id} state court overview'),
                                          depth = 5,
                                          uiOutput('network'),
                                          dataTableOutput('court_table')
                            )
            )
        })
        
        output$court_table <- renderDataTable({
            
            appeal_by_right_cases <- tots_cases %>% 
                filter(USStateName == clicked_state, AppealByRight == 'True') %>% 
                group_by(Court = CourtName) %>% 
                summarise(AppealByRight = paste(CaseTypeDescription, collapse = ', '))
            
            appeal_by_permission_cases <- tots_cases %>% 
                filter(USStateName == clicked_state, AppealByPermission == 'True') %>% 
                group_by(Court = CourtName) %>% 
                summarise(AppealByPermission = paste(CaseTypeDescription, collapse = ', '))
            
            original_proceeding_cases <- tots_cases %>% 
                filter(USStateName == clicked_state, OriginalProceeding == 'True') %>% 
                group_by(Court = CourtName) %>% 
                summarise(OriginalProceeding = paste(CaseTypeDescription, collapse = ', '))
            
            interlocutory_appeal_cases <- tots_cases %>% 
                filter(USStateName == clicked_state, InterlocutoryAppeal == 'True') %>% 
                group_by(Court = CourtName) %>% 
                summarise(InterlocutoryAppeal = paste(CaseTypeDescription, collapse = ', '))
            
            exclusive_cases <- tots_cases %>% 
                filter(USStateName == clicked_state, Exclusive == 'True') %>% 
                group_by(Court = CourtName) %>% 
                summarise(Exclusive = paste(CaseTypeDescription, collapse = ', '))
            
            limited_cases <- tots_cases %>% 
                filter(USStateName == clicked_state, Limited == 'True') %>% 
                group_by(Court = CourtName) %>% 
                summarise(Limited = paste(CaseTypeDescription, collapse = ', '))
            
            tots %>% 
                filter(USStateName == clicked_state) %>% 
                arrange(-CourtLevelRank) %>% 
                group_by(Court = CourtNameName, Level = CourtLevelDescription, 
                         `Appeal From Admin Agency` = AppealFromAdminAgency, Funding = FundingDescription, 
                         `Panel Decision` = PanelDecisionDescription, CourtLevelRank,
                         `# Judges` = NumberJudges, Link, Notes) %>% 
                summarise(`# Divisions` = n_distinct(NumberDivisions),
                          `# Panels` = max(NumberPanels)) %>% 
                ungroup %>% 
                arrange(-CourtLevelRank) %>% 
                select(-CourtLevelRank) %>% 
                mutate(Notes = ifelse(Notes == '-99', '', Notes)) %>% 
                unique %>% 
                left_join(appeal_by_right_cases, by = 'Court') %>% 
                left_join(appeal_by_permission_cases, by = 'Court') %>% 
                left_join(interlocutory_appeal_cases, by = 'Court') %>% 
                left_join(exclusive_cases, by = 'Court') %>% 
                left_join(limited_cases, by = 'Court') %>% 
                mutate(Court = str_glue('<a href={Link} target=_blank><u>{Court}</u></a>')) %>% 
                select(-Link) %>% 
                rename(`Appeal By Right` = AppealByRight, 
                       `Appeal By Permission` = AppealByPermission, 
                       `Interlocutory Appeal` = InterlocutoryAppeal) %>% 
                select(everything(), Notes) %>% 
                datatable(extensions = c('Scroller', 'FixedColumns'),
                          rownames = FALSE,
                          options = list(scrollX = TRUE,
                                         scrollY = 222, 
                                         searching = FALSE,
                                         sort = FALSE,
                                         info = FALSE,
                                         fixedColumns = list(leftColumns = 1),
                                         scroller = TRUE,
                                         deferRender = TRUE),
                          escape = FALSE
                )
        })
        
    })
    
    # State comparison --------------------------------------------------------
    output$state1_network <- renderUI({
        
        state_networks <- make_state_network(tots, input$state1)
        
        map(1:length(state_networks), function(x) {
            output[[str_glue('state1_network_{x}')]] <- renderCollapsibleTree({
                state_networks[[x]]
            })
        })
        
        tagList(
            map(1:length(state_networks), function(x) {
                collapsibleTreeOutput(str_glue('state1_network_{x}'), height = str_glue('{250/length(state_networks)}px'))
            })
        )
        
    })
    
    output$state2_network <- renderUI({
        
        state_networks <- make_state_network(tots, input$state2)
        
        map(1:length(state_networks), function(x) {
            output[[str_glue('state2_network_{x}')]] <- renderCollapsibleTree({
                state_networks[[x]]
            })
        })
        
        tagList(
            map(1:length(state_networks), function(x) {
                collapsibleTreeOutput(str_glue('state2_network_{x}'), height = str_glue('{250/length(state_networks)}px'))
            })
        )
        
    })
    
    output$state_comparison_table <- renderDataTable({
        
        states_to_compare <- c('Washington', 'Oregon')
        states_to_compare <- c(input$state1, input$state2)
        
        state_comparision_table <- USState %>% 
            filter(USStateName %in% states_to_compare) %>% 
            left_join(DeathPenalty, by = 'DeathPenaltyID') %>% 
            left_join(PopulationCategory, by = 'PopulationCategoryID') %>% 
            left_join(PopulationDensity, by = 'PopulationDensityID') %>% 
            left_join(Rural, by = 'RuralID') %>%
            left_join(TrialStructure, by = 'TrialStructureID') %>%
            left_join(AppellateCriminalStructure, by = 'AppellateCriminalStructureID') %>%
            left_join(TrialCriminalProcessing, by = 'TrialCriminalProcessingID') %>%
            left_join(CaseloadSize, by = c('TrialCaseloadSizeID' = 'CaseloadSizeID')) %>% 
            select(USStateName, DeathPenaltyDescription, PopulationCategoryDescription,
                   PopulationDensityDescription, RuralDescription, TrialStructureDescription,
                   AppellateCriminalStructureDescription, TrialCriminalProcessingDescription,
                   CaseloadSizeDescription) %>% 
            gather(metric, value, -USStateName) %>% 
            spread(USStateName, value) %>% 
            mutate(metric = str_replace(metric, 'Description', ''),
                   metric = str_replace_all(metric, '([^_])([A-Z])', '\\1 \\2')) %>% 
            rename(Metric = metric)
        
        datatable(state_comparision_table,
                  extensions = c('Scroller'),
                  rownames = FALSE,
                  options = list(scrollX = FALSE,
                                 scrollY = 400, 
                                 sort = FALSE,
                                 searching = FALSE,
                                 info = FALSE,
                                 scroller = TRUE,
                                 deferRender = TRUE),
                  escape = FALSE
        )
    })
    
}

shinyApp(ui, server)