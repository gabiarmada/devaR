#' Process NEI data
#'
#' @param df .csv file containing EPA NEI Original data.
#' @return A dataframe of cleaned NEI data
#' @export
#'
process_NEI <- function(df){
  # read NEI data
  NEI_data <- read.csv(df)
  NEI_data <- filter(NEI_data, UNIT.OF.MEASURE != 'LB') # filter out pollutants measured in LBs


  clean_data <- NEI_data %>% group_by(COUNTY) # group by county

  clean_data <- clean_data[c(4,6,1,8)] # select columns of interest: COUNTY, POLLUTANT, SECTOR, and EMISSIONS

  colnames(clean_data)[colnames(clean_data) == "EMISSIONS"] <- "EMISSIONS (TONS)" # rename EMISSIONS columns to include units
  colnames(clean_data)[colnames(clean_data) == "TIER"] <- "SECTOR" # rename TIER column to SECTOR

  clean_data['YEAR'] <- str_sub(df,-8, -5) # add a year identifier

  return(clean_data)
}


#' Compile NEI data
#'
#' @param data_folder  A local folder containing .csv files of EPA NEI Original data to process + compile.
#' @return A dataframe of compiled NEI data.
#' @export
#'
compile_NEI <- function(data_folder){
  NEI_compiled <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(NEI_compiled) <- c("COUNTY", "POLLUTANT", "SECTOR", "EMISSIONS (TONS)", "YEAR")

  # get file paths of all EPA NEI Original Data files
  file_paths <- fs::dir_ls(here(data_folder))

  # iterate file paths through process_NEI()
  for (i in seq_along(file_paths)){
    output <- process_NEI(file_paths[i])
    NEI_compiled <- rbind(NEI_compiled, output)
    print(i)
    rm(output)
  }
  return(NEI_compiled)
}
