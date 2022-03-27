#reticulate::use_python("C:\\Users\\weldy\\anaconda3", required = TRUE)
reticulate::use_condaenv("tf", required = TRUE)
# reticulate::use_condaenv("torch")
# setwd("C:")
# reticulate::use_condaenv("C:\\Users\\weldy\\anaconda3\\envs\\tf", required = TRUE)
# reticulate::py_config()
#setwd("P:/PROJECTS/Aardvark_LG")
# autoencoder in keras
#head(df, n=10)
suppressPackageStartupMessages(library(keras))

z_for_pca <- scale(df_clustered_mode[, 7:ncol(df_clustered_mode)])
z_for_pca <- for_pca_mode
z_for_pca_kruger <- scale(for_pca_kruger)
summary(z_for_pca)
head(z_for_pca)

#z_for_pca[1,1] <- NA
# set training data

x_train <- as.matrix(z_for_pca[1:nrow(z_for_pca), 1:ncol(z_for_pca)])
x_train <- as.matrix(df_clustered_mode[, 7:ncol(df_clustered_mode)])
x_train <- as.matrix(z_for_pca)
#x_train <- as.matrix(z_for_pca_kruger)

# set model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 6, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 3, activation = "tanh", name = "bottleneck") %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))


model <- keras_model_sequential()
model %>%
  layer_dense(units = 10, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = 3, activation = "tanh", name = "bottleneck") %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))

model <- keras_model_sequential()
model %>%
  layer_dense(units = 16, activation = "tanh", input_shape = ncol(x_train)) %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = 3, activation = "tanh", name = "bottleneck") %>%
  layer_dense(units = 6, activation = "tanh") %>%
  layer_dense(units = 10, activation = "tanh") %>%
  layer_dense(units = 16, activation = "tanh") %>%
  layer_dense(units = ncol(x_train))
# view model layers
summary(model)

# compile model
model %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam"
)

# evaluate the performance of the model
# fit model
model %>% fit(
  x = x_train, 
  y = x_train, 
  epochs = 2000,
  callbacks = list(
    #callback_reduce_lr_on_plateau(monitor = "loss", factor = 0.1),
    callback_early_stopping(monitor = "loss",
                            min_delta = 0,
                            patience = 5,
                            mode = c("min"))),
  verbose = 1
)

# evaluate the performance of the model
mse.ae2 <- evaluate(model, x_train, x_train)
mse.ae2

# extract the bottleneck layer
intermediate_layer_model <- keras_model(inputs = model$input, outputs = get_layer(model, "bottleneck")$output)
intermediate_output <- predict(intermediate_layer_model, x_train)

library(plotly)
aedf3 <- data.frame(node1 = intermediate_output[,1], node2 = intermediate_output[,2], node3 = intermediate_output[,3])
(ae_plotly <- plot_ly(aedf3, x = ~node1, y = ~node2, z = ~node3, 
                     #color = ~df$Population,
                     color = ~as.factor(df$CombinedPop),
                     #color = ~df$Region[which(df$Region!="NA")],
                     colors = "Set3") %>% add_markers()
)
htmlwidgets::saveWidget(as_widget(ae_plotly ), "./figures/index.html")
