library(shinydashboard)
library(leaflet)
library(shinythemes)

# This forces the attributes to work
convertMenuItem <- function(mi,tabName) {
	mi$children[[1]]$attribs['data-toggle']="tab"
	mi$children[[1]]$attribs['data-value'] = tabName
	mi
}

# A dashboard header with 3 dropdown menus
header <- dashboardHeader(
	title = "EDI in Surrey",
	
	# Dropdown menu for messages
	dropdownMenu(type = "messages", badgeStatus = "success",
							 messageItem("Support Team",
							 						"This is the content of a message.",
							 						time = "5 mins"
							 ),
							 messageItem("Support Team",
							 						"This is the content of another message.",
							 						time = "2 hours"
							 ),
							 messageItem("New User",
							 						"Can I get some help?",
							 						time = "Today"
							 )
	),
							 
							 # Dropdown menu for notifications
							 dropdownMenu(type = "notifications", badgeStatus = "warning",
							 						 notificationItem(icon = icon("users"), status = "info",
							 						 								 "5 new members joined today"
							 						 ),
							 						 notificationItem(icon = icon("warning"), status = "danger",
							 						 								 "Resource usage near limit."
							 						 ),
							 						 notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
							 						 								 status = "success", "25 sales made"
							 						 ),
							 						 notificationItem(icon = icon("user", lib = "glyphicon"),
							 						 								 status = "danger", "You changed your username"
							 						 )
							 ),
							 
							 # Dropdown menu for tasks, with progress bar
							 dropdownMenu(type = "tasks", badgeStatus = "danger",
							 						 taskItem(value = 20, color = "aqua",
							 						 				 "Refactor code"
							 						 ),
							 						 taskItem(value = 40, color = "green",
							 						 				 "Design new layout"
							 						 ),
							 						 taskItem(value = 60, color = "yellow",
							 						 				 "Another task"
							 						 ),
							 						 taskItem(value = 80, color = "red",
							 						 				 "Write documentation"
							 						 )
							 )
	)
sidebar <- dashboardSidebar(sidebarMenu(id = "tabs",
############################################### Tab 1 side bar ############################################
	convertMenuItem(menuItem("EDI Dashboard", tabName = "edi", icon = icon("dashboard"),
					 selectizeInput('select_edi_neighborhood', label = NULL, choices = NULL,
																					 options = list(create = TRUE, maxOptions = 5, maxItems = 1,
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
													 						 selected = "Wave 6: 2013-2016")
													 ),'cluster')
)
)

body <- dashboardBody(tabItems(
############################################### Tab 1 body ############################################
	tabItem(tabName = "edi",
					fluidRow(column(width = 7,
				 box(title = "Neighborhood Map", status = "primary", solidHeader = TRUE,
				 		width = NULL, height = 700,
				 		leafletOutput("SHPplot",height = 640)
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
															width = NULL, height = 700,
															leafletOutput("SHPcluster",height = 640)
													)
																		),
					column(width = 5,
								 box(title = "Bar Graph of Clusters", status = "warning", solidHeader = TRUE,
								 		width = NULL,
								 		plotOutput("clusterbar")
								 		),
								 box(title = "Here we should put census data", status = "warning", solidHeader = TRUE,
								 		collapsible = TRUE, width = NULL
								 		)
					)
					)
	)
)
)
			
ui <- dashboardPage(
	header,
	sidebar,
	body
)


