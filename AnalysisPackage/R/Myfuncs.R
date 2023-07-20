#' Clean the input data
#'
#' This function removes missing values and outliers from the input data.
#'
#' @param data A data frame.
#' @param na_threshold The threshold for missing values (default is 0.5).
#' @param outlier_threshold The threshold for outliers (default is 3).
#' @return A cleaned data frame.
#'
#' @examples
#' # Load the package and sample data
#' library(my_analysis_package)
#' data <- iris
#' # Clean the data with default thresholds
#' cleaned_data <- clean_data(data)
#' # Clean the data with custom thresholds
#' cleaned_data <- clean_data(data, na_threshold = 0.3, outlier_threshold = 2)
#'
#' @export
clean_data <- function(data, na_threshold = 0.5, outlier_threshold = 3) {
  # Remove rows with too many missing values
  num_missing <- colSums(is.na(data))
  data <- data[,which(num_missing <= na_threshold * nrow(data)) ]

  # Handle outliers
  outlier_columns <- sapply(data, is.numeric)
  for (col in names(data[outlier_columns])) {
    col_mean <- mean(data[[col]], na.rm = TRUE)
    col_sd <- sd(data[[col]], na.rm = TRUE)
    data[[col]] <- ifelse(
      abs((data[[col]] - col_mean) / col_sd) > outlier_threshold,
      NA,
      data[[col]]
    )
  }

  # Remove rows with outliers
  data <- data[complete.cases(data), ]
  return(data)
}



#' Create plots to visualize the data
#'
#' This function creates various types of plots to visualize the data.
#'
#' @param data A data frame.
#' @param plot_type The type of plot to create ("scatter" or "histogram").
#' @return A plot object.
#'
#' @examples
#' # Load the package and sample data
#' library(my_analysis_package)
#' data <- iris
#' # Create a scatter plot
#' scatter_plot <- plot_data(data, "scatter")
#' print(scatter_plot)
#' # Create a histogram
#' histogram_plot <- plot_data(data, "histogram")
#' print(histogram_plot)
#'
#' @export
plot_data <- function(data, plot_type) {
  if (plot_type == "scatter") {
    # Create a scatter plot
    if (sum(sapply(data, is.numeric)) < 2) {
      stop("Scatter plot requires at least two numeric variables in the data.")
    }
    plot_object <- ggplot(data, aes(x = data[[1]], y = data[[2]])) +
      geom_point()
  } else if (plot_type == "histogram") {
    # Create a histogram
    if (sum(sapply(data, is.numeric)) == 0) {
      stop("Histogram plot requires at least one numeric variable in the data.")
    }
    plot_object <- ggplot(data, aes(x = data[[1]])) +
      geom_histogram()
  } else {
    stop("Invalid plot_type. Supported plot types are 'scatter' and 'histogram'.")
  }

  return(plot_object)
}


#' Calculate summary statistics for numeric columns in the data frame
#'
#' This function calculates summary statistics for numeric columns in the data frame.
#'
#' @param data A data frame.
#' @return A data frame containing summary statistics for numeric columns.
#'
#' @examples
#' # Load the package and sample data
#' library(my_analysis_package)
#' data <- iris
#' summary_statistics <- summary_stats(data)
#' print(summary_statistics)
#'
#' @export
summary_stats <- function(data) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }

  # Select numeric columns
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols]

  # Calculate summary statistics
  summary_stats_df <- data.frame(
    Variable = colnames(numeric_data),
    Mean = apply(numeric_data, 2, mean, na.rm = TRUE),
    Median = apply(numeric_data, 2, median, na.rm = TRUE),
    SD = apply(numeric_data, 2, sd, na.rm = TRUE),
    Min = apply(numeric_data, 2, min, na.rm = TRUE),
    Max = apply(numeric_data, 2, max, na.rm = TRUE),
    N = apply(!is.na(numeric_data), 2, sum)
  )

  return(summary_stats_df)
}



