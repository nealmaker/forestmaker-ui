#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::navbarPage(
      # title = shiny::tags$div(
      #   class = "d-flex w-100 align-items-center",  # Makes title & icon align horizontally
      #   shiny::tags$span("ForestMaker", class = "me-auto"),  # Title stays left-aligned
      #
      #   shiny::tags$a(
      #     href = "#",  # Placeholder for now
      #     id = "command-line-btn",
      #     class = "nav-link",
      #     shiny::icon("terminal"),
      #     shiny::tags$span(class = "sr-only", "Open R Console")  # Accessibility
      #   )
      # ),
      title = "ForestMaker",

      theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),

      # Dashboard Overview
      shiny::tabPanel("Dashboard",
                      shiny::navlistPanel(
                        id = "dashboard_navlist",
                        "Data Exploration",
                        shiny::tabPanel(
                          "Forest Conditions",
                          shiny::h2("Forest Conditions"),
                          shiny::p("Interactive visualizations of operational unit and stand level stocking and species composition go here. A 'load data' button is diplayed instead if there's no data loaded."),
                          shiny::p(shiny::actionButton("load_data", "Load Data", class = "btn-lg btn-warning"),
                                   class = "d-flex justify-content-center")
                        ),
                        shiny::tabPanel(
                          "Timber Volumes & Values",
                          shiny::h2("Standing Timber Volumes & Values"),
                          shiny::p("Interactive visualizations of standing timber volumes and values go here.")
                        ),
                        shiny::tabPanel(
                          "Carbon Stocking",
                          shiny::h2("Carbon Stocking"),
                          shiny::p("Interactive visualizations of operational unit and stand level carbon stocking go here.")
                        ),
                        shiny::tabPanel(
                          "Maple Production",
                          shiny::h2("Maple Production"),
                          shiny::p("Interactive visualizations of maple production potential go here.")
                        ),
                        shiny::tabPanel(
                          "Conservation & Biodiversity",
                          shiny::h2("Conservation & Biodiversity"),
                          shiny::p("Interactive visualizations of landscape-, property-, and stand-scale habitat and conservation metrics go here.")
                        ),
                        shiny::tabPanel(
                          "Logging Removals",
                          shiny::h2("Logging Removals"),
                          shiny::p("Interactive visualizations of removed timber volumes and values go here. Same screen as in 'Data Management -> Logging Removals'")
                        ),
                        shiny::tabPanel(
                          "Timber Prices",
                          shiny::h2("Timber Prices"),
                          shiny::p("Interactive visualizations of historic and current timber prices go here. Same screen as in 'Data Management -> Timber Prices'")
                        ),
                        shiny::tabPanel(
                          "Growth Simulations",
                          shiny::h2("Growth Simulations"),
                          shiny::p("Interactive visualizations of growth simulations go here, including visualizations of optimized management regimes.")
                        ),
                      )
      ),

      # Data Management
      shiny::navbarMenu("Data Management",
                        shiny::tabPanel("Manage Data Files",
                                        shiny::sidebarLayout(
                                          shiny::sidebarPanel(
                                            shiny::fileInput("file_upload", "Upload Data File"),
                                            shiny::downloadButton("download_data", "Save Data"),
                                            shiny::downloadButton("download_xlsx_data", "Download Data as .xlsx")
                                          ),
                                          shiny::mainPanel(
                                            shiny::h2("Manage Data Files"),
                                            shiny::p("Upload, view, and export data files related to your projects. A list of loaded files shows up here, which the user can choose from for export.")
                                          )
                                        )
                        ),
                        shiny::tabPanel("Landowner Info",
                                        shiny::navlistPanel(
                                          id = "ownerifo_navlist",
                                          "Landowner Data Management",
                                          shiny::tabPanel(
                                            "New Owner Info",
                                            shiny::h2("New Owner Info"),
                                            shiny::p("Form for entering info.")
                                          ),
                                          shiny::tabPanel(
                                            "Edit Owner Info",
                                            shiny::h2("Edit Owner Info"),
                                            shiny::p(shiny::actionButton("load_owner", "Choose an Owner", class = "btn-primary")),
                                            shiny::p("Pre-filled, edited form")
                                          ),
                                          shiny::tabPanel(
                                            "Link Owners to Properties",
                                            shiny::h2("Link Owners to Properties"),
                                            shiny::p("Form that links existing owners to property data (maybe this is part of the owner info form instead?)")
                                          ),
                                          shiny::tabPanel(
                                            "Owner Objectives",
                                            shiny::h2("Owner Objectives"),
                                            shiny::p("Form to choose landowner's objectives and edit objectives text.")
                                          )
                                        )
                        ),
                        shiny::tabPanel("Inventory Data",
                                        shiny::navlistPanel(
                                          id = "invdata_navlist",
                                          "Inventory Data Management",
                                          shiny::tabPanel(
                                            "Compilation",
                                            shiny::h2("Inventory Compilation"),
                                            shiny::p("Compilation from gpkg or xlsx cruise data file, with option to automatically add remote data.")
                                          ),
                                          shiny::tabPanel(
                                            "Remote Data",
                                            shiny::h2("Remote Inventory Data"),
                                            shiny::p("Add remote data to an already-compiled inventory dataset."),
                                            shiny::p(shiny::actionButton("load_cruise", "Choose a Compiled Inventory", class = "btn-primary")),
                                            shiny::p("Form to add selected remote data to an already-compiled inventory.")
                                          ),
                                          shiny::tabPanel(
                                            "Inventory Text",
                                            shiny::h2("Inventory Text Editing"),
                                            shiny::p("Edit the text in an already-compiled inventory dataset."),
                                            shiny::p(shiny::actionButton("load_cruise", "Choose a Compiled Inventory", class = "btn-primary")),
                                            shiny::p("Displays inventory text (stand history narrative, etc.) and allows for editing.")
                                          ),
                                          shiny::tabPanel(
                                            "Forest Condition Estimates",
                                            shiny::h2("Forest Condition Estimates"),
                                            shiny::p("Combines exisiting data from one or more inventories and logging removals to create a 'forest condition estimate', which can be used in simulations and management plans.")
                                          ),
                                          shiny::tabPanel(
                                            "Simulate Growth",
                                            shiny::h2("Grow Inventory Data or Forest Condition Estimates"),
                                            shiny::p("Simulate future conditions, update old data to estimate current conditions, or grow estimates back in time to estimate previous conditions."),
                                            shiny::p(shiny::actionButton("load_cruise_fce", "Choose a Compiled Inventory or Forest Condition Estimate", class = "btn-primary")),
                                            shiny::p("Generates a new inventory or forest condition estimate file.")
                                          )
                                        )
                        ),
                        shiny::tabPanel("Logging Removals",
                                        shiny::h2("Logging Removals"),
                                        shiny::p("Enter information about timber volumes removed and corresponding trucking, mill and stumpage prices."),
                                        shiny::p(shiny::actionButton("new_timber_removals", "Enter New Removals Data", class = "btn-primary"), shiny::actionButton("edit_timber_removals", "Edit Existing Removals Data", class = "btn-primary")),
                                        shiny::p("Option to generate removals reports on submission. Interactive logging removal summary data panel goes here.")
                        ),
                        shiny::tabPanel("Timber Prices",
                                        shiny::h2("Timber Prices"),
                                        shiny::p(shiny::actionButton("load_cruise", "Add New Data From a Mill Price Sheet", class = "btn-primary"),
                                                 shiny::downloadButton("download_xlsx_data", "Export Historic Price Data as .xlsx")),
                                        shiny::p("Interactive visualizations of historic and current timber prices go here.")
                        )
      ),

      # Cruise Planning
      shiny::navbarMenu("Inventory Planning",
                        shiny::tabPanel("Inventory Prioritization",
                                        shiny::h2("Inventory Prioritization"),
                                        shiny::p("Tool goes here to help prioritize future inventories based on the existing data and shortcomings.")
                        ),
                        shiny::tabPanel("Inventory Design",
                                        shiny::h2("Inventory Design"),
                                        shiny::p("Tool goes here to plan inventory type and plot layout based on stand map and user input, and to generate a gpkg file that can be uploaded to QField for data collection.")
                        ),
                        shiny::tabPanel("Supporting Materials",
                                        shiny::navlistPanel(
                                          id = "cruisesupportingmaterials_navlist",
                                          "Supporting Materials for Inventory",
                                          shiny::tabPanel(
                                            "Stand Report",
                                            shiny::h2("Pre-cruise Stand Report"),
                                            shiny::p("Reports on stand-level information that can be gleaned from remote and existing data.")
                                          ),
                                          shiny::tabPanel(
                                            "Plot Map",
                                            shiny::h2("Plot Map"),
                                            shiny::p("Map of stands and inventory plots.")
                                          ),
                                          shiny::tabPanel(
                                            "Soils Map",
                                            shiny::h2("Soils Map"),
                                            shiny::p("Stand soils map with terrain.")
                                          )
                                        )
                        )
      ),

      # Management Planning
      shiny::navbarMenu("Management Planning",
                        shiny::tabPanel("Management Optimization",
                                        shiny::h2("Management Strategy Optimization"),
                                        shiny::p("Estimate optimal management strategies for a variety of outcomes, using ForestMaker's heuristic optimization techniques and neighborhood-scale forest growth simulator."),
                                        shiny::p("If no simulation parameter files are loaded, create a new simulation parameter file using the 'Set Simulation Parameters' button below (if none are loaded, optimizer button is disabled and set params button is highlighted.)"),
                                        shiny::p(shiny::actionButton("set_sim_params", "Set Simulation Parameters"),
                                                 shiny::actionButton("optimize", "Optimize Management", class = "btn-lg btn-success")),
                                        shiny::p("Report loads here, along with links to science behind optimizer."),
                                        shiny::p(shiny::actionButton("go_dashboard_sims", "Go to Dashboard View", class = "btn-primary"))
                        ),
                        shiny::tabPanel("Management Strategy Simulations",
                                        shiny::h2("Management Strategy Simulations & Comparison"),
                                        shiny::p("Compare expected long-term outcomes from different silvicultural systems and optimal management regimes."),
                                        shiny::p("If no simulation parameter files are loaded, create a new simulation parameter file using the 'Set Simulation Parameters' button below (if none are loaded, simulation buttons are disabled and set params button is highlighted.)"),
                                        shiny::h3("Run Simulations"),
                                        shiny::p(shiny::actionButton("set_sim_params", "Set Simulation Parameters"),
                                                 shiny::actionButton("silv_sim", "Simulate Management Under a Predefined Silvicultural System", class = "btn-primary"),
                                                 shiny::actionButton("optimize", "Simulate Optimal Management", class = "btn-lg btn-success")),
                                        shiny::h3("Compare Simulations"),
                                        shiny::p("Selector to choose from loaded simulations, Comparison loads here after selection is made."),
                                        shiny::p(shiny::actionButton("go_dashboard_sims", "Go to Dashboard View", class = "btn-primary"))
                        ),
                        shiny::tabPanel("Inventory Analyses",
                                        shiny::navlistPanel(
                                          id = "invanalysis_navlist",
                                          "Preconfigured Inventory Analyses",
                                          shiny::tabPanel(
                                            "Based on Tree Data",
                                            shiny::h2("Expected Treatments"),
                                            shiny::p("based on trees marked as 'cut' in inventory.", style = "font-size: 20px;"),
                                            shiny::p(shiny::actionButton("generate_tree_treatments", "Configure Report", class = "btn-primary")),
                                            shiny::p("Report loads here, showing expected removals and expected residual stand characteristics.")
                                          ),
                                          shiny::tabPanel(
                                            "Based on Plot Data",
                                            shiny::h2("Expected Treatments"),
                                            shiny::p("based on plot-level 'treatment' calls in inventory.", style = "font-size: 20px;"),
                                            shiny::p(shiny::actionButton("generate_neighborhood_treatments", "Configure Report", class = "btn-primary")),
                                            shiny::p("Report loads here, showing expected removals and expected residual stand characteristics.")
                                          ),
                                          shiny::tabPanel(
                                            "Based on Cutting Rules",
                                            shiny::h2("Expected Treatments"),
                                            shiny::p("based on cutting rules.", style = "font-size: 20px;"),
                                            shiny::p(shiny::actionButton("generate_cutting_rule_treatments", "Configure Report", class = "btn-primary")),
                                            shiny::p("Report loads here, showing expected removals and expected residual stand characteristics.")
                                          ),
                                          "Custom Analysis",
                                          shiny::tabPanel(
                                            "Command-line Interface",
                                            shiny::p("R editor goes here. (Can we load RStudio panels within app?)")
                                          )
                                        )
                        ),
                        shiny::tabPanel("Plan Writing",
                                        shiny::navlistPanel(
                                          id = "planwriting_navlist",
                                          "Plan Writing",
                                          shiny::tabPanel(
                                            "Owner Objectives",
                                            shiny::h2("Owner Objectives"),
                                            shiny::p("Form to choose landowner's objectives and edit objectives text.")
                                          ),
                                          shiny::tabPanel(
                                            "Near-Term Options",
                                            shiny::h2("Share Near-Term Management Options"),
                                            shiny::p("Form goes here to pick management options and generate a nice-looking email to share them with owner, to solicit feedback.")
                                          ),
                                          shiny::tabPanel(
                                            "Management Plan Text",
                                            shiny::h2("Generate & Edit Plan Text"),
                                            shiny::p("Recommended long-term management strategy and prescription text is displayed by stand, and user can edit it.")
                                          ),
                                          shiny::tabPanel(
                                            "Generate Management Plan",
                                            shiny::h2("Generate a Management Plan"),
                                            shiny::p("Generate a management plan from existing inventory data, owner data, owner objectives, management plan text, and report order form. If you do not have an existing order form, generate a new one with the 'New Order Form' button."),
                                            shiny::p(shiny::actionButton("order_form", "New Order Form"), shiny::actionButton("write_plan", "Generate Management Plan", class = "btn-primary")),
                                            shiny::p("Plan loads here.")
                                          )
                                        )
                        ),
                        shiny::tabPanel("Harvest Scheduling",
                                        shiny::h1("Harvest Scheduling"),
                                        shiny::p("Prioritize logging operations across operational units based on financial optimization.")
                        )
      ),

      # Reporting
      shiny::tabPanel("Reporting",
                      shiny::navlistPanel(
                        id = "reporting_navlist",
                        shiny::tabPanel(
                          "Report Order Forms",
                          shiny::h2("Report Order Forms"),
                          shiny::p("Order forms can be used to customize the information and formatting used in reports. They are espercially important for management plans and cruise reports."),
                          shiny::p(shiny::actionButton("order_form", "New Order Form", class = "btn-primary"))
                        ),
                        "Supporting Materials for Inventory",
                        shiny::tabPanel(
                          "Pre-inventory Stand Report",
                          shiny::h2("Pre-cruise Stand Report"),
                          shiny::p("Reports on stand-level information that can be gleaned from remote and existing data.")
                        ),
                        shiny::tabPanel(
                          "Plot Map",
                          shiny::h2("Plot Map"),
                          shiny::p("Map of stands and inventory plots.")
                        ),
                        shiny::tabPanel(
                          "Soils Map",
                          shiny::h2("Soils Map"),
                          shiny::p("Stand soils map with terrain.")
                        ),
                        "Current Conditions",
                        shiny::tabPanel(
                          "Inventory Summary",
                          shiny::h2("Inventory Summary Report"),
                          shiny::p("Property and stand level summary information from an inventory. Use a report order form to customize."),
                          shiny::p(shiny::actionButton("generate_cruise_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Standing Timber",
                          shiny::h2("Standing Timber Report"),
                          shiny::p("Stand and stock tables."),
                          shiny::p(shiny::actionButton("generate_timber_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Maple Sugar Potential",
                          shiny::h2("Maple Sugar Potential Report"),
                          shiny::p("Estimated maple tap counts and sugar production."),
                          shiny::p(shiny::actionButton("generate_maple_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Carbon Stocking",
                          shiny::h2("Current Carbon Stocking Report"),
                          shiny::p("Estimated carbon stocking at property and stand scales."),
                          shiny::p(shiny::actionButton("generate_carbon_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Conservation & Biodiversity",
                          shiny::h2("Conservation & Biodiversity Report"),
                          shiny::p("Landscape-, property-, and stand-scale habitats and habitat elements."),
                          shiny::p(shiny::actionButton("generate_biodiversity_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        "Forest Growth Simulations",
                        shiny::tabPanel(
                          "Growth Simulation",
                          shiny::h2("Forest Growth Simulation Report"),
                          shiny::p("Estimated forest development and removals from an existing simulation."),
                          shiny::p(shiny::actionButton("generate_simulation_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Optimal Management",
                          shiny::h2("Optimal Management Regime Report"),
                          shiny::p("Analysis of strategies employed for optimal management, along with simulation report showing forest development and removals under optimal management."),
                          shiny::p(shiny::actionButton("generate_optimal_mngmt_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        "Management Planning",
                        shiny::tabPanel(
                          "Near-term Management Options",
                          shiny::h2("Near-term Management Options to Share With Landowner"),
                          shiny::p("Generate a nice-looking email to share with owner, to solicit feedback."),
                          shiny::p("Form shows up here.")
                        ),
                        shiny::tabPanel(
                          "Management Plan",
                          shiny::h2("Forest Management Plan"),
                          shiny::p("Use a report order form to customize plan and ensure it conforms to applicable standards."),
                          shiny::p(shiny::actionButton("write_plan", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Harvest Scheduling",
                          shiny::h2("Optimal Harvest Scheduling Report"),
                          shiny::p("Prioritization of logging operations to maximize long-term value."),
                          shiny::p(shiny::actionButton("harvest_scheduling", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        "Logging Operations",
                        shiny::tabPanel(
                          "Logging Removals",
                          shiny::h2("Logging Removals Report"),
                          shiny::p("Timber volumes removed over a single payment period, and associated trucking, mill, and stumpage prices."),
                          shiny::p(shiny::actionButton("generate_logging_removal_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Logging Operation Summary",
                          shiny::h2("Logging Operation Summary Report"),
                          shiny::p("Harvested volumes, trucking prices, mill prices, and stumpage payments for a logging operation."),
                          shiny::p(shiny::actionButton("generate_logging_operation_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        ),
                        shiny::tabPanel(
                          "Stumpage Report",
                          shiny::h2("Stumpage Report"),
                          shiny::p("Stumpage rates paid across logging operations for a set period of time."),
                          shiny::p(shiny::actionButton("generate_stumpage_report", "Configure Report", class = "btn-primary")),
                          shiny::p("Report loads here.")
                        )
                      )

      ),
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "forester"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
