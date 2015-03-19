# removes columns in a dataframe that consist entirely of NAs
#
# 'df' argument is for a dataframe
#
# can be used with multiple dataframes(dfs) if dfs are placed in a list and
# rm_col_na is used within lapply
#
# e.g:
# data_list <- list(df1, df2, df3)
# lapply(data_list, rm_col_na)


rm_col_na <- function(df){
    df[,colSums(is.na(df)) != nrow(df)]
}
