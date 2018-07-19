library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readxl)
library(rgdal)
library(data.table)
library(tmap)
library(tmaptools)
library(stringr)

server <- function(input, output, session) {
	############################################# Data Import ########################################
	
	# Import shape file from Andy
	shape <- readOGR(dsn="./data/Surrey_neighborhood_with_whiterock/surrey_neighborhoods2.shp",
									 layer="surrey_neighborhoods2", GDAL1_integer64_policy='TRUE')
	
	# Skip the weird notes at the top of excel file, skip = 6
	edi <- read_excel("./data/edi_wave2-6.xlsx",
											sheet = 2, col_names = TRUE, skip=6)
	
	
	# Take kevin's tSNE labelled data, drop the rownumber column
	labels.surrey <- read.csv("./data/sur_cluster_labels.csv")
	labels.surrey <- labels.surrey[ , colnames(labels.surrey) %in% c("N_CODE","label_2",
																																	 "label_3","label_4",
																																	 "label_5","label_6",
																																	 "label_aw")]
	
	########################################### Global data manipulation ###################################
	
	# Create temporary data table to work on only Surrey, then map coordinates back to Lat Long
	shape.surrey <- shape[shape$SD_NAME == "Surrey",]
	shape.surrey <- spTransform(shape.surrey, CRS("+init=epsg:4326"))
	surrey.data <- data.frame(shape.surrey@data)
	surrey.data$N_CODE <- sapply(surrey.data$N_CODE,as.character)
	
	edi.surrey <- filter(edi,N_CODE %in% surrey.data$N_CODE) 
	edi.surrey <- subset(edi.surrey, select = -c(n_code_name))
	
	# Coalesce data into the shape file
	shape.surrey <- append_data(shp = shape.surrey, data = edi.surrey, 
															key.shp = "N_CODE", key.data = "N_CODE")
	shape.surrey <- append_data(shp = shape.surrey, data = labels.surrey, 
															key.shp = "N_CODE", key.data = "N_CODE")
	
	####################################### Reactive environments ############################################
	
	## EDI Tab
	# Here we use the radio buttons to select all data for a specific wave, rename all variables
	 	edi_wave_number <-reactive({
										switch(input$radio_edi,
												"Wave 2: 2004-2007" = "_2",
												"Wave 3: 2007-2009" = "_3",
												"Wave 4: 2009-2011" = "_4",
												"Wave 5: 2011-2013" = "_5",
												"Wave 6: 2013-2016" = "_6")
	 	})
	# Create the full wave data frame, contains all subscales for a particular wave number
		edi_wave <-reactive({
			wave_number <- edi_wave_number()
			tempwave <- shape.surrey@data[,grep(wave_number, names(shape.surrey@data))]
			names(tempwave)[names(tempwave) == paste0("editot",wave_number)] <- 'count'
			names(tempwave)[names(tempwave) == paste0("edival",wave_number)] <- 'valid_count'
			names(tempwave)[names(tempwave) == paste0("PCTPHYRI",wave_number)] <- 'physical'
			names(tempwave)[names(tempwave) == paste0("PCTSOCRI",wave_number)] <- 'social'
			names(tempwave)[names(tempwave) == paste0("PCTEMORI",wave_number)] <- 'emotional'
			names(tempwave)[names(tempwave) == paste0("PCTLANRI",wave_number)] <- 'language'
			names(tempwave)[names(tempwave) == paste0("PCTCOMRI",wave_number)] <- 'communication'
			names(tempwave)[names(tempwave) == paste0("PCTEVERR",wave_number)] <- 'one_or_more'
			names(tempwave)[names(tempwave) == paste0("PCTEVER4",wave_number)] <- 'one_or_more_wo_com'
			tempwave
		})
	 	
	# Pull from the overall wave data frame the specific subscale data
		edi_subscaleData <- reactive({
			wave <- edi_wave()
			switch(input$select_edi_subscale,
					 	"Count" = wave$count,
						"Valid Count" = wave$valid_count,
						"Physical" = wave$physical,
						"Social" = wave$social,
						"Emotional" = wave$emotional,
						"Language" = wave$language,
						"Communication" = wave$communication,
						"One or More" = wave$one_or_more,
						"One or More (w/o Communication)" = wave$one_or_more_wo_com)
	 })
		
	updateSelectizeInput(session, 'select_edi_neighborhood', choices = surrey.data$N_NAME, server = TRUE)
		
	
	## Cluster tab
	cluster_wave_number <-reactive({
		switch(input$radio_cluster,
					 "Wave 2: 2004-2007" = "_2",
					 "Wave 3: 2007-2009" = "_3",
					 "Wave 4: 2009-2011" = "_4",
					 "Wave 5: 2011-2013" = "_5",
					 "Wave 6: 2013-2016" = "_6",
					 "All Waves" = "_aw")
	})
	# Create the cluster data frame
		cluster_wave <- reactive({
			wave_number <- cluster_wave_number()
			shape.surrey@data[,grepl(paste0('label',wave_number),
																			names(shape.surrey@data))]
		})
		
	########################################### Begin Plots #################################################
		
	# Leaflet plots
	output$SHPcluster <- renderLeaflet({
		clusterwave <- cluster_wave()
		# Color based on chosen data
		if (cluster_wave_number() != "_aw"){
		qpal <- colorFactor(rainbow(3), clusterwave)	
		pops <- paste0("<strong>Neighborhood: </strong>",shape.surrey$N_NAME,"<br>",
									 "<strong>Cluster: </strong>", clusterwave, "<br>")
		# Plot shapes
		shape.Plot <- leaflet(data = shape.surrey) %>% 
		            addTiles() %>% 
		            addPolygons(data = shape.surrey, stroke = TRUE, opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
								  color = "black", fillColor = ~qpal(clusterwave), weight = 1,popup = pops,
								  highlight = highlightOptions(weight = 5, color = "#666",fillOpacity = 0.7)) %>%
		            addLegend(values = ~clusterwave, pal = qpal, title = "Cluster Label",
		            					labels = c(0,1,2))
		} else {
			qpal <- colorFactor(rainbow(6), clusterwave)	
			pops <- paste0("<strong>Neighborhood: </strong>",shape.surrey$N_NAME,"<br>",
										 "<strong>Cluster: </strong>", clusterwave, "<br>")
			# Plot shapes
			shape.Plot <- leaflet(data = shape.surrey) %>% 
				addTiles() %>% 
				addPolygons(data = shape.surrey, stroke = TRUE, opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
										color = "black", fillColor = ~qpal(clusterwave), weight = 1,popup = pops,
										highlight = highlightOptions(weight = 5, color = "#666",fillOpacity = 0.7)) %>%
				addLegend(values = ~clusterwave, pal = qpal, title = "Cluster Label",
									labels = c(0,1,2,3,4,5))
		}
			
		})
	
	output$SHPplot <- renderLeaflet({
		subscale <- edi_subscaleData()
		
		# Color based on chosen data
		qpal <- colorBin(rev(heat.colors(5)), subscale, bins=5)	
		pops <- paste0("<strong>Neighborhood: </strong>",shape.surrey$N_NAME,"<br>",
									 "<strong>Vulnerable percent: </strong>", subscale, "<br>")
		# Plot shapes
		shape.Plot <- leaflet(data = shape.surrey) %>% 
			addTiles() %>% 
			addPolygons(data = shape.surrey, stroke = TRUE, opacity = 1, fillOpacity = 0.5, smoothFactor = 0.5,
									color = "black", fillColor = ~qpal(subscale), weight = 1,popup = pops,
									highlight = highlightOptions(weight = 5, color = "#666",fillOpacity = 0.7)) %>%
			addLegend(values = ~subscale, pal = qpal, title = "Vulnerable Percent")
	})
	
	
	# GGPlots
	output$edi_overall <- renderPlot({
		if(input$select_edi_neighborhood == ""){
			dat <- shape.surrey@data[,grep("editot", names(shape.surrey@data))]
			dat <- round(colMeans(dat))
			waves <- 2:6
			df <- data.frame(wave = waves, edi_scores = dat)
			ggplot(data=df, aes(x=wave, y=edi_scores)) +
				geom_line()+
				geom_point()+
				xlab("Wave")+
				ylab("Number of children vulnerable")+
				ggtitle("EDI vulnerability for each wave for all neighborhoods")
		} else if (!is.null(input$select_edi_neighborhood)){
			dat <- filter(shape.surrey@data, N_NAME == input$select_edi_neighborhood)
			dat <- dat[,grep("editot", names(dat))]
			dat <- colMeans(dat)
			waves <- 2:6
			df <- data.frame(wave = waves, edi_scores = dat)
			title <- paste0('EDI vulnerability for each wave in ',input$select_edi_neighborhood)
			ggplot(data=df, aes(x=wave, y=edi_scores)) +
				geom_line()+
				geom_point()+
				xlab("Wave")+
				ylab("Number of children vulnerable")+
				ggtitle(title)
		}
	})
	
	
	output$edi_subscale <- renderPlot({
		if(input$select_edi_neighborhood == ""){
			subscale <- switch(input$select_edi_subscale,
												 "Count" = "editot",
												 "Valid Count" = "edival",
												 "Physical" = "PCTPHYRI",
												 "Social" = "PCTSOCRI",
												 "Emotional" = "PCTEMORI",
												 "Language" = "PCTLANRI",
												 "Communication" = "PCTCOMRI",
												 "One or More" = "PCTEVERR",
												 "One or More (w/o Communication)" = "PCTEVER4")
			dat <- shape.surrey@data[,grep(subscale, names(shape.surrey@data))]
			dat <- round(colMeans(dat))
			waves <- 2:6
			df <- data.frame(wave = waves, edi_scores = dat)
			if (input$select_edi_subscale %in% c("Count", "Valid Count")) {
				title <- paste0(input$select_edi_subscale,' for all neighborhoods')
				ytitle <- "Count"
			} else {
				title <- paste0(input$select_edi_subscale,' vulnerability for all neighborhoods')
				ytitle <- "Percent vulnerable (%)"
			}
			ggplot(data=df, aes(x=wave, y=edi_scores)) +
				geom_line()+
				geom_point()+
				xlab("Wave")+
				ylab(ytitle)+
				ggtitle(title)+
				theme_classic()
		} else if (!is.null(input$select_edi_neighborhood)){
			subscale <- switch(input$select_edi_subscale,
												 "Count" = "editot",
												 "Valid Count" = "edival",
												 "Physical" = "PCTPHYRI",
												 "Social" = "PCTSOCRI",
												 "Emotional" = "PCTEMORI",
												 "Language" = "PCTLANRI",
												 "Communication" = "PCTCOMRI",
												 "One or More" = "PCTEVERR",
												 "One or More (w/o Communication)" = "PCTEVER4")
			dat <- filter(shape.surrey@data, N_NAME == input$select_edi_neighborhood)
			dat <- dat[,grep(subscale, names(dat))]
			dat <- colMeans(dat)
			waves <- 2:6
			df <- data.frame(wave = waves, edi_scores = dat)
			if (input$select_edi_subscale %in% c("Count", "Valid Count")) {
				title <- paste0(input$select_edi_subscale,' for ',input$select_edi_neighborhood)
				ytitle <- "Count"
			} else {
				title <- paste0(input$select_edi_subscale,' vulnerability for ',input$select_edi_neighborhood)
				ytitle <- "Percent vulnerable (%)"
			}
			ggplot(data=df, aes(x=wave, y=edi_scores)) +
				geom_line()+
				geom_point()+
				xlab("Wave")+
				ylab(ytitle)+
				ggtitle(title)+
				theme_classic()
		}
	})
	
	output$clusterbar <- renderPlot({
		clusterwave <- cluster_wave()
		clusterwave <- data.frame('label' = clusterwave)
		if (cluster_wave_number() != "_aw"){
		clusterwave %>% 
			ggplot(aes(label)) +
			geom_bar(fill = rainbow(3))
		} else {
			clusterwave %>% 
				ggplot(aes(label)) +
				geom_bar(fill = rainbow(6))
		}
	})
	
}
