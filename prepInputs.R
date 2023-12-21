prepInputs <- function(rawSheet = rawSheet, adjm = adjm, 
                       totServ = totServ, baseServ = baseServ, 
                       foodLoss = foodLoss, baseFL = baseFL)
{
  #------
  # library(igraph)
  # numComp = 13
  # adjm <- matrix(c(rep(0,numComp*6),
  #                  0,0.46,0.165,0.094,0.04,0.015,rep(0,7), # Broth
  #                  rep(0,6),0.56,rep(0,6),  # Stock
  #                  rep(0,6),0.214,rep(0,6), # Food Loss
  #                  rep(0,8),0.165,rep(0,4), # bones in Food Loss
  #                  rep(0,8),0.044,rep(0,4), # vegs in Food Loss
  #                  rep(0,8),0.005,rep(0,4), # meat in Food Loss
  #                  0.15,0.19,rep(0,5),0.56,rep(0,5) # Final Product
  #                  ), 
  #                ncol=numComp)
  # 
  # g_adjm <- graph_from_adjacency_matrix(adjm, weighted=FALSE)
  # 
  # names_adjm <- list("noodle","water","bones","vegs","meat","condi",
  #                    "Broth","Stock","Food Loss",
  #                    "FL.bones","FL.vegs","FL.meat","Final Product")
  # g_recipe <- set.vertex.attribute(g_adjm, "label", value = names_adjm)
  # 
  # # example 9/16/2022
  # foodloss_date1 <- c(rep(9.5,13))
  # (adjm/0.214)%*%foodloss_date1
  # t(adjm/0.214)%*%foodloss_date1
  # 
  # totServ_date1 <- c(rep(47.7,13))
  # (adjm/0.9)%*%totServ_date1
  # t(adjm/0.9)%*%totServ_date1
  #
    # 
    # 
    # plot(g_recipe)
  
  # adjm <- matrix(c(0,0,0,-0.15,
  #                   -0.46,0,0,-0.19,
  #                   -0.165,0,0,0,
  #                   -0.094,0,0,0,
  #                   -0.04,0,0,0,
  #                   -0.015,0,0,0,
  #                   0.56,-0.56,0,0,
  #                   0,0.56,0,0,
  #                   0,0,0.214,0,
  #                   0,0,-0.165,0,
  #                   0,0,-0.044,0,
  #                   0,0,-0.005,0,
  #                   0,0,0,0.9),
  #                 ncol=numComp)
  #-----------
  # numcoln = ncol(adjm)
  
  numrows = nrow(adjm)

  totServ.reped.transposed <- t(matrix(rep(totServ,numrows),ncol = numrows))
  inputs.from.totServ <- t(adjm/baseServ)%*%totServ.reped.transposed
  
  fl.reped.transposed <- t(matrix(rep(foodLoss,numrows),ncol = numrows))
  inputs.from.foodLoss <- t(adjm/baseFL)%*%fl.reped.transposed
  
  diff.Broth <- t(inputs.from.foodLoss[8,]) - t(inputs.from.totServ[8,])
  diff.prods <- (adjm[4,13]/adjm[2,8])*diff.Broth
  
  return(data.frame(date = rawSheet[,1],
                    totServ_input  = t(inputs.from.totServ),
                    foodLoss_input = t(inputs.from.foodLoss),
                    Broth_diff     = t(diff.Broth),
                    Final_Prod_diff= t(diff.prods)))
}
