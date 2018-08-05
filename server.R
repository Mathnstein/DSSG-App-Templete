############################################# Packages ###########################################
library(leaflet)
library(ggplot2)
library(dplyr)
library(readxl)
library(rgdal)
library(reticulate)
library(data.table)
library(tmap)
library(tmaptools)
library(stringr)
library(colormap)

#devtools::install_github("mattflor/chorddiag")
library(chorddiag)

library(dplyr)
library(ggpubr)
library(car)
############################################# Data Import ########################################

source("data_manipulation.R")

############################################# Server ########################################

server <- function(input, output, session) {
	


	
	####################################### Reactive environments ############################################
	
	####################################### EDI Tab ######################################################
	
	# Input button information
	
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
		
		# Create the options for search bar
		
		updateSelectizeInput(session, 'select_edi_neighborhood', choices = shape.surrey@data$N_NAME, server = TRUE)
		
	
	###################################### Cluster Tab #####################################################
	
	# Input button information
	
	cluster_wave_number <- reactive({switch(input$radio_cluster,
					 "Wave 2: 2004-2007" = "_2",
					 "Wave 3: 2007-2009" = "_3",
					 "Wave 4: 2009-2011" = "_4",
					 "Wave 5: 2011-2013" = "_5",
					 "Wave 6: 2013-2016" = "_6",
					 "All Waves" = "_aw")
	})
	
	method <- reactive({
		switch(input$radio_cluster_method,
					 "tSNE" = "t",
					 "UMAP" = "u")
	})
	
	alpha <- reactive({switch(input$radio_anova,
														"Alpha = .01" = .01,
														"Alpha = .05" = .05,
														"Alpha = .10" = .10)
	})
	
	# Create the cluster data frame
	
		cluster_wave <- reactive({
			wave_number <- cluster_wave_number()
			method <- method()
			shape.surrey@data[,grepl(paste0('label',wave_number,method),
																			names(shape.surrey@data))]
		})
		
		observe({
			if (!is.null(input$check_census)) {
				vars <- filter(census_typed, type %in% input$check_census)
				vars <- vars$variable
				updateSelectizeInput(session, 'select_census', choices = vars, server = TRUE)
			} else{
				vars <- names(census)
				vars <- vars[-1]
			}
			updateSelectizeInput(session, 'select_census', choices = vars, server = TRUE)
		})
		
		# Create Census data frame
		
		census_data <-reactive({
			if(!is.null(input$select_census)) {
				census_scaled <- census %>% select(N_CODE, input$select_census)
				census_scaled[,c(2:ncol(census_scaled))] <- scale(census_scaled[,c(2:ncol(census_scaled))])
				if (length(input$select_census) == 1){
					names(census_scaled)[2] <- 'variable'
					census_scaled <- data.frame('N_CODE' = census_scaled$N_CODE, 'variable' = rep(input$select_census,24),
																		'value' = census_scaled$variable)
				} else {
					census_scaled <- melt(census_scaled, id.vars = "N_CODE")
				}
				wave_number <- cluster_wave_number()
				method <- method()
				cluster_labels <- labels.surrey[,c('N_CODE',paste0('label',wave_number,method))]
				colnames(cluster_labels)[colnames(cluster_labels)==paste0('label',wave_number,method)] <- "label"
				census_scaled <- left_join(census_scaled, cluster_labels)
			}
		})
		
	# Anova test for all selected census vars
	
	anova_results <- reactive({
		if(!is.null(input$select_census)) {
			alpha = alpha()
			data <- census_data()
			vars <- input$select_census
			census_anova <- vars
			vars <- paste("<b>",vars,"</b>")
			for(i in input$select_census){
				temp <- filter(data, variable == i)
				census_var <- temp[,3]
				census_label <- temp[,ncol(temp)]
				source('Anova_cluster.r', local = TRUE)
				census_anova[census_anova == i] <- result
			}
			c(rbind(vars,census_anova))
		} else {
			'Choose census variables to compare.'
		}
	})
	
		
	########################################### Begin Plots #################################################
		
	############################### Tab 1 #####################################	
		
	# Leaflet plot
		output$SHPplot <- renderLeaflet({
			subscale <- edi_subscaleData()
			if (input$select_edi_subscale %in% c("Count", "Valid Count")) {
				lab_title <- 'Total vulnerable'
				} else {
				lab_title <- 'Vunerable Percent'
			}
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
				addLegend(values = ~subscale, pal = qpal, title = lab_title)
		})
		
		# Top right box
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
		
		# Bottom right box
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
		
		
		############################### Tab 2 #####################################	
		
		# Leaflet plot		
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
		
		# Top right box	
		output$clusterbar <- renderPlot({
			clusterwave <- cluster_wave()
			clusterwave <- data.frame('label' = clusterwave)
			if (cluster_wave_number() != "_aw"){
				clusterwave %>% 
					ggplot(aes(label)) +
					geom_bar(fill = rainbow(3))
			} else if(method() == 't') {
				clusterwave %>% 
					ggplot(aes(label)) +
					geom_bar(fill = rainbow(6))
			} else {
				clusterwave %>% 
					ggplot(aes(label)) +
					geom_bar(fill = rainbow(4))
			}
		})
		
		# Bottom right box
		output$cluster_census <- renderPlot({
			census_data <- census_data()
			if (!is.null(input$select_census)){
				ggplot(census_data) +
					geom_boxplot(aes(x=factor(label), y=value, fill = variable), 
											 position = position_dodge(0.8)) +
					labs(x='EDI Clusters', y='Scaled Value')+
					scale_fill_discrete(name="Variables",
															label = paste0("Variable", as.character(1:length(input$select_census))))+
					theme_light()+
					theme(legend.title=element_text(size=25), 
								legend.text=element_text(size=20),
								axis.text.x = element_text(size = 17),
								axis.text.y = element_text(size = 17),
								axis.title.x = element_text(size = 20),
								axis.title.y = element_text(size = 20)) }
		})
		
		# Bottom left box
		output$anovaresult <- renderUI({
			string <- anova_results()
			HTML(paste(string, collapse = '<br/>'))
		})
		
# END APP
}
