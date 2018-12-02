library(Rtsne)

set.seed(42)
df_D3 %>%
        mutate(soil_type=as.numeric(soil_type),
               wilderness_area=as.numeric(wilderness_area)) %>%
        distinct() %>%
        group_by(cover_type) %>%
        sample_n(400) %>%
        ungroup() -> df_tsne

df_tsne %>%
        dplyr::select(-cover_type, -vdth_lt_350, -vdth_btw_350_500, -vdth_gt_500) %>%
        as.matrix() -> tsne_mat

tsne_fit <- Rtsne(X = tsne_mat,
                  dims = 2,
                  verbose = T,
                  pca_center = T,
                  pca_scale = T,
                  max_iter = 1500,
                  perplexity = 200) #Fitting performed in 17976.12 seconds.

result <- tsne_fit$Y %>%
        as_tibble() %>%
        bind_cols(df_tsne)
result
result %>%
        ggplot(aes(V1,V2))+
        geom_point(aes(color=cover_type),alpha=0.4)
result %>%
        ggplot(aes(V1,V2))+
        geom_point(aes(color=wilderness_area),alpha=0.4)
result %>%
        ggplot(aes(V1,V2))+
        geom_point(aes(color=soil_type),alpha=0.4)
result %>%
        ggplot(aes(V1,V2))+
        geom_point(aes(color=log(soil_type*wilderness_area)),alpha=0.4)
result %>%
        ggplot(aes(V1,V2))+
        geom_point(aes(color=soil_type), alpha=0.4)+
        facet_wrap(~as.factor(wilderness_area),nrow = 3)
result %>%
        ggplot(aes(V1,V2))+
        geom_point(aes(color=cover_type))+
        facet_wrap(~as.factor(wilderness_area),nrow = 3)
