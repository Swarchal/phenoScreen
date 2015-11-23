z_test <- function(df_a, df_b,
                   plot = FALSE){
    
    require(magrittr)
    require(MASS)
    
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

