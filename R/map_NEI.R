
# get geographic coordinates from sf object function
# source: https://github.com/r-spatial/sf/issues/231
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


# Geom Text Outline function
# Source: https://github.com/yjunechoe/penngradlings/blob/master/R/ggplot2-annotations.R
geom_text_outline <- function(..., geom = "geom_text", inner_params = list(), outer_params = list(), use_outer = FALSE) {
  .id <- stringr::str_flatten(sample(letters, 24, replace = TRUE))
  geom <- rlang::ensym(geom)
  .inner_params <- list(
    x = eval(rlang::call2(geom, ...)),
    colour = "#FFFFFF",
    sigma = 0.01,
    expand = 4,
    id = .id
  )
  .outer_params <- list(
    x = .id,
    colour = "#EBEBEB",
    sigma = 0.01,
    expand = 8
  )
  if (use_outer) {
    list(
      do.call(ggfx::with_outer_glow, utils::modifyList(.inner_params, inner_params)),
      do.call(ggfx::with_outer_glow, utils::modifyList(.outer_params, outer_params))
    )
  } else {
    .inner_params <- c(.inner_params, include = TRUE)
    do.call(ggfx::with_outer_glow, utils::modifyList(.inner_params, inner_params))
  }
}




#' Map NEI data
#'
#' @param data .csv file of compiled NEI data.
#' @param sector string input of the pollutant sector of interest.
#'
#' @return A ggplot object of spatially modeled Virginia pollutant sources.
#' @export
#'
map_NEI <- function(data, sector){
  # GET NEI DATA + MAP GEOMETRY --
  emiss_data <- read.csv(data)

  # load USAboundaries county geographical information
  us_counties.in <- us_counties()
  va_counties <- us_counties.in[us_counties.in$state_abbr == 'VA',] # subset to VA

  # get geometric coordinates from sf object
  usab <- sf::st_centroid(va_counties)
  usab <- sfc_as_cols(usab)  # change geometry format from lists of lat/long to x and y

  # alter centroids
  #Wise county (conflicted with city of Norton)
  usab[63,14] <- 37
  #Norton City (covers itself with text, makes color harder to identify)
  #Original longitude = 36.93270
  usab[20,14] <- 36.91
  #Martinsville city (conflicted with Henry county)
  #Original longitude = 36.68212
  usab[59,14] <- 36.72
  #Henry County (conflicted with Martinsville city)
  #Original longitude = 36.68228
  usab[120,14] <- 36.59
  #Prince George county (not to be confused with Prince William county)
  #Conflicted with Petersburg, original longitude = 37.18476
  usab[81,14] <- 37.14
  #Hopewell (conflicted with Colonial Heights City)
  #Original longitude = 37.29119
  usab[121,14] <- 37.31
  #Henrico county (conflicted with Richmond)
  #Original longitude = 37.53576, original latitude = -77.39660
  usab[67,14] <- 37.62
  usab[67,13] <- -77.5
  #Albermarle county (Conflicted with Charlottesville)
  #Original longitude = 38.02056
  usab[117,14] <- 37.96
  #Augusta county (conflicted with Staunton)
  #Original longitude = 38.16215
  usab[109,14] <- 38.25
  #Rockbridge county (conflicted with Lexington city)
  #Original longitude = 37.81339
  #Original latitude = -79.44715
  usab[32,14] <- 37.9
  usab[32,13] <- -79.40
  #Frederick county (conflicted with Winchester)
  #Original longitude = 39.19933
  #Original latitude = -78.26258
  usab[56,14] <- 39.25
  usab[56,13] <- -78.21
  #Allegheny county (conflicted with Covington city)
  #Original longitude = 37.78621
  usab[126,14] <- 37.85
  #Covington City (covers itself with text)
  #Original longitude = 37.77890
  usab[57,14] <- 37.75
  #Lexington City (covers itself with text)
  #Original longitude = 37.78289
  usab[37,14] <- 37.82
  #Rockingham county (conflicts with Harrisonburg)
  #Original longitude = 38.50800
  usab[112,14] <- 38.59
  #King George county (conflicted with Fredericksburg)
  #Original longitude = 38.26816
  usab[60,14] <- 38.24
  #Essex county (conflicted with richmond COUNTY, not city)
  #Original longitude = 37.93388
  #Original latitude = -76.94210
  usab[129,14] <- 37.88
  usab[129,13] <- -76.9
  #Roanoke City (Conflicted with Roanoke County and Salem city)
  #From USAB both city and county are listed only as Roanoke,
  #but we can look at geoid/county FIPS (city is 51770, county is 51161) to distinguish
  #Change the name to 'Roanoke City' to make it clearer, since we DO have emissions data
  #for the city, county, and also Salem city.
  usab[24,6] <- 'Roanoke City'
  #Roanoke County (Conflicted with Roanoke City and Salem city)
  #Original longitude = 37.26929
  usab[87,14] <- 37.2
  #Salem City (Conflicted with Roanoke City/county)
  #Original longitude = 37.28329
  #Original latitude = -80.05863
  usab[74,14] <- 37.32
  usab[74,13] <- -80.12
  #Roanoke City (conflicted with Salem City *after previous geometry changes*)
  #Original longitude = 37.27816
  usab[24,14] <- 37.25
  #York county (conflicted with Williamsburg)
  #Original longitude = 37.24071
  #Original latitude = -76.54238
  usab[98,14] <- 37.21
  usab[98,13] <- -76.47
  #Williamsburg city (covers its own city geometry, so you can't see the color scheme)
  #Original longitude = 37.27974
  usab[2,14] <- 37.26
  #Greensville county (conflicts with Emporia city)
  #Original longitude = 36.67877
  usab[100,14] <- 36.62
  #Emporia city (covers itself with label)
  #Original longitude = 36.69712
  usab[30,14] <- 36.72
  #King and Queen county (conflicts with King William county)
  #Original longitude = 37.71052
  usab[71,14] <- 37.78
  #Alexandria (conflicts with basically all of Northern Virginia)
  #Original longitude = 38.81394
  #Original latitude = -77.08185
  usab[80,14] <- 38.79
  usab[80,13] <- -76.99
  #Arlington (conflicts with basically all of Northern Virginia)
  #Original latitude = -77.10139
  usab[78,13] <- -77
  #Falls Church (conflicts with adjacent Arlington and is much smaller)
  #Original longitude = 38.88484
  usab[34,14] <- 38.92
  #Fairfax city and County are not distinguished, so we must fix:
  usab[95,6] <- 'Fairfax City'
  #Fairfax city (covers itself with text)
  #Original longitude = 38.85180
  usab[95,14] <- 38.83
  #Fairfax county (conflicts with basically all of Northern Virginia)
  #Original longitude = 38.83671
  #Original latitude = -77.27637
  usab[132,14] <- 38.7
  usab[132,13] <- -77.1
  #Prince William county (conflicts with Manassas)
  #Original longitude = 38.70862
  usab[114,14] <- 38.65
  #Manassas Park (conflicts with Manassas)
  #Original longitude = 38.77044
  usab[108,14] <- 38.79
  #Manassas city (covers itself with text)
  #Original longitude = 38.74657
  usab[107,14] <- 38.72
  #Charlottesville (covers itself with text)
  #Original longitude = 38.03715
  usab[96,14] <- 38.07
  #Waynesboro city (covers itself with text)
  #Original longitude = 38.06392
  usab[14,14] <- 38.03
  #Staunton city (covers itself with text)
  #Original longitude = 38.16203
  usab[118,14] <- 38.13


  # GET EMISSIONS CATEGORY (SECTOR) --
  # filter our data to select emissions in desired category
  if (sector == "Waste Disposal"){
    emiss_data_sect <- emiss_data[emiss_data$SECTOR == "Waste Disposal",]
  } else if(sector == "Fuel Comb"|
            sector == "Mobile"|
            sector == "Industrial Processes"|
            sector == "Fires"){
    emiss_data_sect<- emiss_data %>% filter(str_detect(SECTOR, sector))
  } else if(sector == "Solvents & Misc Non-Industrial NEC"){
    emiss_data_solv <- emiss_data %>% filter(str_detect(SECTOR, "Solvent"))
    emiss_data_misc <- emiss_data %>% filter(str_detect(SECTOR, "Miscellaneous Non-Industrial NEC"))
    emiss_data_sect <- rbind(emiss_data_solv,emiss_data_misc)
  }else if(sector == "Total Emissions"){
    emiss_data_sect <- emiss_data
  }


  # merge emissions data with county geographical data by geoid
  emiss_data_sect$geoid <- paste0( emiss_data_sect$STATE_FIPS,
                                   formatC( emiss_data_sect$COUNTY_FIPS, width = 3, flag = '0' ))
  emiss_data_sect.sf <- merge( va_counties, emiss_data_sect,
                               by = 'geoid')


  # MAP NEI --
  # group and summarize emissions by county (otherwise variable color fill has problems)
  sum_emiss <- emiss_data_sect.sf %>%
    group_by(geoid) %>%
    summarise(total_emissions = sum(EMISSIONS))

  emiss_plot <- ggplot(sum_emiss, aes( fill = total_emissions)) +
    # use geometry from our .sf objects and set county border thickness
    geom_sf(size=0.1)+
    # plot county labels using USAB centroids
    stat_midpoint(aes(label = name,x = x, y = y, group = geoid),
                  geom = "text",size=1,data=usab,color='white',inherit.aes=FALSE)+
    # legend title
    labs(fill='Emissions (Tons)')+
    # key height/width
    theme(legend.key.height = unit(0.5, 'cm'),
          legend.key.width = unit(0.2, 'cm'),
          # changing legend position
          legend.position = c(0.2, 0.7),
          # remove legend background (so it doesn't overlap with Virginia)
          legend.background = element_rect(fill='transparent'),
          # center plot title
          plot.title = element_text(hjust = 0.5,size=10),
          legend.text = element_text(size=7),
          # size legend title
          legend.title = element_text(size=7),
          # remove axis titles, grid, etc.
          axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank())


  if (sector == "Waste Disposal"){
    emiss_plot <- emiss_plot +
      ggtitle('Waste Disposal')+
      scale_fill_gradient(low = "tan4", high = "yellow")

  }else if(sector == "Fuel Comb"){
    emiss_plot <- emiss_plot +
      ggtitle('Fuel Combustion (all types)')+
      scale_fill_gradient(low = "cornsilk", high = "red")

  }else if(sector == "Mobile"){
    emiss_plot <- emiss_plot +
      ggtitle('Mobile Sources (all types)')+
      scale_fill_gradient(low = "black", high = "red")

  }else if(sector == "Industrial Processes"){
    emiss_plot <- emiss_plot +
      ggtitle('Industrial Processes (all types)')+
      scale_fill_gradient(low = "blue", high = "gold")

  }else if(sector == "Fires"){
    emiss_plot <- emiss_plot +
      ggtitle('Fires (All Types)')+
      scale_fill_gradient(low = "darkgreen", high = "yellow")


  }else if(sector == "Solvents & Misc Non-Industrial NEC"){
    emiss_plot <- emiss_plot +
      ggtitle('Other Emissions (Solvents, Misc Non-Industrial NEC)')+
      scale_fill_gradient(low = "black", high = "orange")

  }else if(sector == "Total Emissions"){
    emiss_plot <- emiss_plot +
      ggtitle('Total 2011 County SO2 Emissions')+
      scale_fill_gradient(low = "black", high = "red")
  }


  return(emiss_plot)}
