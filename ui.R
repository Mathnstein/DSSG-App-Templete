library(shinydashboard)
library(leaflet)
library(shinythemes)

# This forces the attributes to work
convertMenuItem <- function(mi,tabName) {
	mi$children[[1]]$attribs['data-toggle']="tab"
	mi$children[[1]]$attribs['data-value'] = tabName
	mi
}

# A dashboard header
header <- dashboardHeader(
	title = "DSSG 2018 analysis over Surrey")
sidebar <- dashboardSidebar(sidebarMenu(id = "tabs",
############################################### Tab 1 side bar ############################################
	convertMenuItem(menuItem("EDI Dashboard", tabName = "edi", icon = NULL,
					 selectizeInput('select_edi_neighborhood', label = NULL, choices = NULL,
																					 options = list(create = TRUE, maxItems = 1,
																					 							 placeholder = 'Select a neighborhood')
					 ),
					radioButtons("radio_edi", label = h3("Choose the EDI Wave"),
						 choices = list("Wave 2: 2004-2007", "Wave 3: 2007-2009",
						 							 "Wave 4: 2009-2011", "Wave 5: 2011-2013", "Wave 6: 2013-2016"),
						 selected = "Wave 6: 2013-2016"),
					selectInput("select_edi_subscale", label = h3("Choose the subscale"),
						c("Count", "Valid Count", "Physical", "Social", "Emotional", "Language", "Communication",
							"One or More", "One or More (w/o Communication)"),
						selected = "Count")
	),'dashboard'),
############################################### Tab 2 side bar ############################################
	convertMenuItem(menuItem("Cluster Dashboard", tabName = "cluster", icon = NULL,
					radioButtons("radio_cluster", label = h3("Choose the EDI Wave"),
						choices = list("Wave 2: 2004-2007", "Wave 3: 2007-2009",
													 "Wave 4: 2009-2011", "Wave 5: 2011-2013", "Wave 6: 2013-2016",
													 	"All Waves"),
						selected = "Wave 6: 2013-2016"),
					radioButtons("radio_cluster_method", label = h3("Choose the clustering method"),
											 choices = list("tSNE", "UMAP"),
											 selected = "tSNE"),
						checkboxGroupInput(inputId = "check_census", label = h3("Census Groups:"),
							choices = list("Geography" = "ge",
														 "Ethnic Origins" = "eo",
														 "Language and Immigration" = "li",
														 "Income" = "in",
													 	 "Cost of Living" = "cl",
														 "Employment" = "em",
													 	 "Occupation" = "oc",
													 	 "Population" = "po"),
							selected = NULL),
						selectizeInput('select_census', label = NULL, choices = NULL,
													 options = list(create = TRUE, maxItems = 10,
													 placeholder = 'Select Census Variables')
													 ),
						radioButtons("radio_anova", label = h3("Anova input"),
												 choices = list("Alpha = .01",
												 							 "Alpha = .05",
												 							 "Alpha = .10"), selected = "Alpha = .05")
													 ),'cluster')
)
# End sidepanel
)

body <- dashboardBody(tabItems(
############################################### Tab 1 body ############################################
	tabItem(tabName = "edi",
					fluidRow(column(width = 7,
				 box(title = "Neighborhood Map", status = "primary", solidHeader = TRUE,
				 		width = NULL, height = 600,
				 		leafletOutput("SHPplot",height = 540)
				 )
				),
				column(width = 5,
				 box(title = "Over all EDI", status = "warning", solidHeader = TRUE,
				 		width = NULL,
				 		plotOutput("edi_overall", height = 300)
				 		),
				 box(title = "Subscale", status = "warning", solidHeader = TRUE,
				 		collapsible = TRUE, width = NULL,
				 		plotOutput("edi_subscale", height = 300)
				 		)
				)
					)
	),
############################################### Tab 2 body ############################################
	tabItem(tabName = "cluster",
					fluidRow(column(width = 7,
													box(title = "Neighborhood Map", status = "primary", solidHeader = TRUE,
															width = NULL, height = 600,
															leafletOutput("SHPcluster",height = 540)
															),
													box(title = "Anova Test", status = "warning", solidHeader = TRUE,
															width = NULL, height = 400,
															htmlOutput("anovaresult")
															)
																		),
					column(width = 5,
								 box(title = "Bar Graph of Clusters", status = "warning", solidHeader = TRUE,
								 		width = NULL,
								 		plotOutput("clusterbar")
								 		),
								 box(title = "Census Variables", status = "warning", solidHeader = TRUE,
								 		collapsible = TRUE, width = NULL,
								 		plotOutput("cluster_census")
								 		)
					)
					)
	)
)
# End body
)
			
ui <- dashboardPage(
	header,
	sidebar,
	body
)