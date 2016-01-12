#' Multivatiate Z test
#' 
#' Calculates the differences between multivariate distributions with a KS test
#' of Sammon distances. N.B Experimental, not tested.
#' 
#' @param df_a Numerical matrix or dataframe
#' @param df_b Numerical matrix or dataframe
#' @param plot if TRUE will plot distributions
#' 
#' @return D score and p value, if plot is TRUE, then will plot results
#'
#' @import dplyr
#' @importFrom MASS sammon
#'
#' @export
#' 
#' @examples
#' a <- rnorm(100, 101, 0.5)
#' a2 <- rnorm(100, 5, 2)
#' a3 <- rnorm(100, 5, 0.5)
#' a4 <- rnorm(100, 0.1, 0.1)
#' a5 <- rnorm(100, 5, 0.1)
#' a6 <- rnorm(100, 100, 0.1)
#' 
#' sample_df_a <- data.frame(a,
#'                    a2,
#'                    a3,
#'                    a4,
#'                    a5,
#'                    a6)
#' 
#' b <- rnorm(100, 5, 0.5)
#' b2 <- rnorm(100, 5, 2)
#' b3 <- rnorm(100, 5, 0.5)
#' b4 <- rnorm(100, 5, 0.1)
#' b5 <- rnorm(100, 5, 0.1)
#' b6 <- rnorm(100, 5, 0.1)
#' 
#' sample_df_b <- data.frame(b,
#'                    b2,
#'                    b3,
#'                    b4,
#'                    b5,
#'                    b6)
#'                    
#' z_test(sample_df_a, sample_df_b)
#' z_test(sample_df_a, sample_df_b, plot = TRUE)

z_test <- function(df_a, df_b,
                   plot = FALSE){
    
    # scale data to 1D with Sammon's non-linear mapping
    # canberra distance metric used
    cmds_a <- df_a %>% 
        as.matrix() %>% 
        dist(method = "canberra") %>% 
        sammon(k = 1,
               trace = FALSE)
    
    cmds_b <- df_b %>%
        as.matrix() %>% 
        dist(method = "canberra") %>% 
        sammon(k = 1,
               trace = FALSE)
    
    # KS test on univariate data produced by non-linear MD scaling
    ks_out <- ks.test(cmds_a$points[, 1],
                      cmds_b$points[, 1])
    
    if (plot == FALSE){return(ks_out)}
    
    if (plot == TRUE){
        ecdf_a <- ecdf(cmds_a$points[,1])
        ecdf_b <- ecdf(cmds_b$points[,1])
        
        plt <- plot(ecdf_a, verticals = TRUE,
                    do.points = FALSE,
                    main = "ECDF",
                    sub = paste("D =", ks_out[[1]], "\np =", signif(ks_out[[2]], 4)),
                    xlab = "",
                    xlim = c(
                        min(c(cmds_a$points, cmds_b$points)),
                        max(c(cmds_a$points, cmds_b$points))
                        ))
        plt_lines <- lines(ecdf_b, verticals = TRUE,
              do.points = FALSE,
              col = "red")
    print(ks_out, c(plt, digits = 4))
    }
}

