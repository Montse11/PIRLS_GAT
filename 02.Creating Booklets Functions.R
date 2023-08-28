## 2.1. Harder passages
booklet_creation_Hard_func = function(cc, ii){
  H = passages_cond[[cc]][1]
  
  # FOR MORE DIFFICULT BOOKLETS
  #select only high and moderate difficult passages
  PIRLS16_H = PIRLS16[which(PIRLS16$Passagediff_adaptive == "H" |
                              PIRLS16$Passagediff_adaptive == "M" ),]
  
  #Reasign values to the passages to use lsasim 
  PIRLS16_H$PassageID_rec = recode(PIRLS16_H$PassageID, `1`=1,`2`=2,`3` =3,`4`=4,
                                   `7`=5,`8`=6,`9`=7,`10`=8) # Replace these so the following matrices make sense
  
  #New items for harder passages
  set.seed(47405 * ii)
  new_items_H = data.frame(Item = paste0("NH", H, 1:(6*12)),
                           Passage_Name = c(rep("New Passage Inf D3",12),
                                            rep("New Passage Lit D3",12),
                                            rep("New Passage Inf D4",12),
                                            rep("New Passage Lit D4",12),
                                            rep("New Passage Inf D5",12),
                                            rep("New Passage Lit D5",12)),
                           Passage_Sequence = c(paste0("NInfD3_", 1:(12)), paste0("NLitD3_", 1:(12)),
                                                paste0("NInfD4_", 1:(12)), paste0("NLitD4_", 1:(12)),
                                                paste0("NInfD5_", 1:(12)), paste0("NLitD5_", 1:(12))),
                           Purposes_for_Reading = c(rep("Acquire and Use Information", 12), 
                                                    rep("Literary Experience", 12),
                                                    rep("Acquire and Use Information", 12), 
                                                    rep("Literary Experience", 12),
                                                    rep("Acquire and Use Information", 12), 
                                                    rep("Literary Experience", 12)),
                           Slope = runif(72, min = .4, max = 2.28),
                           Location = runif(72, min = .64, max = 2.15),
                           ItemID_og = paste0("i.", 181:(181+71)),
                           PassageID_rec = c(rep(9, 12), rep(10, 12),
                                             rep(11, 12), rep(12, 12),
                                             rep(13, 12), rep(14, 12))
                           )
  #have names matching for merging
  names(new_items_H) <- names(PIRLS16_H)[c(1:3,9,16,18,32,34)]
  #merge
  PIRLS16_H = bind_rows(PIRLS16_H, new_items_H)
  
  # Create these clusters based on the appropriate condition
  if(H == 0){
    CB_cluster_matrix_H <- data.frame(matrix(  c(
      1,7,1, # 1,9,1,
      2,2,6, # 2,2,8,
      3,3,5, # 3,3,7,
      4,7,2, # 4,9,2,
      5,1,5, # 5,1,7,
      6,4,6, # 6,4,8,
      7,8,1, # 7,10,1,
      8,5,2, # 8,7,2,
      9,3,6  # 9,3,8
    ), ncol=3, byrow = T))
    
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_H) <- c("Book", "B1", "B2")
    #create book design 
    book_design_H = data.frame(matrix(c(
      1,0,0,0,0,0,1,0,
      0,1,0,0,0,1,0,0,
      0,0,1,0,1,0,0,0,
      0,1,0,0,0,0,1,0,
      1,0,0,0,1,0,0,0,
      0,0,0,1,0,1,0,0,
      1,0,0,0,0,0,0,1,
      0,1,0,0,1,0,0,0,
      0,0,1,0,0,1,0,0),
      ncol=8, byrow = T))
    # Subset passages
    PIRLS16_H = PIRLS16_H %>% filter(PassageID_rec %in% c(1:8))
    
    #PA for passage  
  }else if(H == 2){
    CB_cluster_matrix_H <- data.frame(matrix(  c(
      1,7,1, 
      2,2,6, 
      3,3,9, # 3,3,5,
      4,7,10, # 4,7,2,
      5,1,5, 
      6,4,6, 
      7,8,1, 
      8,5,2, 
      9,3,6,  
      10,9,10 #new booklet
    ), ncol=3, byrow = T))
    
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_H) <- c("Book", "B1", "B2")
    #create book design 
    book_design_H = data.frame(matrix(c(
      1,0,0,0,0,0,1,0,0,0,
      0,1,0,0,0,1,0,0,0,0,
      0,0,1,0,0,0,0,0,1,0,
      0,0,0,0,0,0,1,0,0,1,
      1,0,0,0,1,0,0,0,0,0,
      0,0,0,1,0,1,0,0,0,0,
      1,0,0,0,0,0,0,1,0,0,
      0,1,0,0,1,0,0,0,0,0,
      0,0,1,0,0,1,0,0,0,0,
      0,0,0,0,0,0,0,0,1,1),
      ncol=10, byrow = T))
    # Subset passages
    PIRLS16_H = PIRLS16_H %>% filter(PassageID_rec %in% c(1:10))
  }else if(H == 4){
    CB_cluster_matrix_H <- data.frame(matrix(  c(
      1,7,12, ## 1,7,1, 
      2,2,6, 
      3,3,9, # 3,3,5,
      4,7,10, # 4,7,2,
      5,1,5, 
      6,4,11, ## 6,4,6, 
      7,8,1, 
      8,5,2, 
      9,3,6,  
      10,9,10, #new booklet
      11,11,12 ##new booklet
    ), ncol=3, byrow = T))
    
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_H) <- c("Book", "B1", "B2")
    #create book design 
    book_design_H = data.frame(matrix(c(
      0,0,0,0,0,0,1,0,0,0,0,1,
      0,1,0,0,0,1,0,0,0,0,0,0,
      0,0,1,0,0,0,0,0,1,0,0,0,
      0,0,0,0,0,0,1,0,0,1,0,0,
      1,0,0,0,1,0,0,0,0,0,0,0,
      0,0,0,1,0,0,0,0,0,0,1,0,
      1,0,0,0,0,0,0,1,0,0,0,0,
      0,1,0,0,1,0,0,0,0,0,0,0,
      0,0,1,0,0,1,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,1,1,0,0,
      0,0,0,0,0,0,0,0,0,0,1,1),
      ncol=12, byrow = T))
    # Subset passages
    PIRLS16_H = PIRLS16_H %>% filter(PassageID_rec %in% c(1:12))
  }else if(H == 6){
    CB_cluster_matrix_H <- data.frame(matrix(  c(
      1,7,12, ## 1,7,1, 
      2,2,6, 
      3,3,9, # 3,3,5,
      4,7,10, # 4,7,2,
      5,1,5, 
      6,4,11, ## 6,4,6, 
      7,8,1, 
      8,5,14, ### 8,5,2, 
      9,3,6,  
      10,9,10, #new booklet
      11,11,12, ##new booklet
      12,13,2, ###new booklet
      13,13,14 ###new booklet
    ), ncol=3, byrow = T))
    
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_H) <- c("Book", "B1", "B2")
    #create book design 
    book_design_H = data.frame(matrix(c(
      0,0,0,0,0,0,1,0,0,0,0,1,0,0,
      0,1,0,0,0,1,0,0,0,0,0,0,0,0,
      0,0,1,0,0,0,0,0,1,0,0,0,0,0,
      0,0,0,0,0,0,1,0,0,1,0,0,0,0,
      1,0,0,0,1,0,0,0,0,0,0,0,0,0,
      0,0,0,1,0,0,0,0,0,0,1,0,0,0,
      1,0,0,0,0,0,0,1,0,0,0,0,0,0,
      0,0,0,0,1,0,0,0,0,0,0,0,0,1,
      0,0,1,0,0,1,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,1,1,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,1,1,0,0,
      0,1,0,0,0,0,0,0,0,0,0,0,1,0,
      0,0,0,0,0,0,0,0,0,0,0,0,1,1),
      ncol=14, byrow = T))
    # Subset passages
    PIRLS16_H = PIRLS16_H %>% filter(PassageID_rec %in% c(1:14))
  }
  ## Extract CBA item-cluster-matrix (dummy coding)
  ## the items of only this group of passages
  PIRLS16_H$ItemID_gat = 1:nrow(PIRLS16_H)
  
  PIRLS16_H_items = PIRLS16_H[,c("Item", "ItemID_og", "ItemID_mod", "ItemID_gat",
                                 "PassageID_rec","passage_difficulty", "Cycle",
                                 "Slope (aj)", "Location (bj)"#,  "Guessing (cj)", 
                                 # "Step 1 (dj1)", "Step 2 (dj2)", "Step 3 (dj3)"
  )]
  
  
  PIRLS_item_cluster_matrix_H  = dummy_cols(PIRLS16_H_items, select_columns = "PassageID_rec")
  
  names(PIRLS_item_cluster_matrix_H) <- c("Item", "ItemID_og", "ItemID_mod","ItemID_gat",
                                          "PassageID","passage_difficulty", "Cycle",
                                          "Slope", "Location",  #"Guessing", 
                                          # "Step 1", "Step 2", "Step 3",
                                          paste0("PA",1:ncol(book_design_H))) 
  
  # For Harder Booklets
  ## Define item pool 
  #PIRLS16_H$ItemID = 1:nrow(PIRLS16_H)
  item_pool_H = PIRLS16_H[,c("ItemID_gat", "Slope (aj)", "Location (bj)")]
  
  row.names(item_pool_H) <- 1:nrow(item_pool_H)
  names(item_pool_H) <- c("item", "a","b")
  item_pool_H[is.na(item_pool_H)] = 0 # replacing all NAs to zero
  item_pool_H = data.frame(item_pool_H)
  
  
  ##Define item-cluster matrix
  item_block_H <- PIRLS_item_cluster_matrix_H[,paste0("PA", c(1:ncol(book_design_H)))]
  row.names(item_block_H) <- 1:nrow(item_block_H)
  colnames(item_block_H) <- paste0("PA", 1:ncol(book_design_H))
  
  ##Assign items to blocks/passages
  block_H <- lsasim::block_design(item_parameters = item_pool_H,
                                  item_block_matrix = item_block_H)
  
  ##Assign blocks to booklets
  book_H <- lsasim::booklet_design(item_block_assignment = block_H$block_assignment,
                                   book_design = book_design_H )
  
  #all the objects I want the function to return
  mylist = list(book_H = book_H, 
                item_pool_H = item_pool_H,
                PIRLS16_H = PIRLS16_H,
                PIRLS_item_cluster_matrix_H = PIRLS_item_cluster_matrix_H,
                CB_cluster_matrix_H = CB_cluster_matrix_H)
  return(mylist)  
  
}

## 2.2. Easier passages
booklet_creation_Easier_func = function(cc, ii){
  
  L = passages_cond[[cc]][2]
  
  # FOR MORE DIFFICULT BOOKLETS
  #select only high and moderate difficult passages
  PIRLS16_L = PIRLS16[which(PIRLS16$Passagediff_adaptive == "L" |
                              PIRLS16$Passagediff_adaptive == "M" ),]
  
  #Reasign values to the passages to use lsasim 
  PIRLS16_L$PassageID_rec = recode(PIRLS16_L$PassageID, `3`=1,`4`=2,`9`=3,`10`=4,
                                   `5`=5,`6`=6,`11`=7,`12`=8) # Replace these so the following matrices make sense
  
  #New items for harder passages
  set.seed(47405 * ii)
  new_items_L = data.frame(Item = paste0("NL", L, 1:(6*12)),
                           Passage_Name = c(rep("New Passage Inf E4",12),
                                            rep("New Passage Lit E4",12),
                                            rep("New Passage Inf E5",12),
                                            rep("New Passage Lit E5",12),
                                            rep("New Passage Inf E6",12),
                                            rep("New Passage Lit E6",12)),
                           Passage_Sequence = c(paste0("NInfE4_", 1:(12)), paste0("NLitE4_", 1:(12)),
                                                paste0("NInfE5_", 1:(12)), paste0("NLitE5_", 1:(12)),
                                                paste0("NInfE6_", 1:(12)), paste0("NLitE6_", 1:(12))),
                           Purposes_for_Reading = c(rep("Acquire and Use Information", 12), 
                                                    rep("Literary Experience", 12),
                                                    rep("Acquire and Use Information", 12), 
                                                    rep("Literary Experience", 12),
                                                    rep("Acquire and Use Information", 12), 
                                                    rep("Literary Experience", 12)),
                           Slope = runif(72, min = .4, max = 2.28),
                           Location = runif(72, min = -3.89, max = -.61),
                           ItemID_og = paste0("i.", 253:(253+71)),
                           PassageID_rec = c(rep(9, 12), rep(10, 12),
                                             rep(11, 12), rep(12, 12),
                                             rep(13, 12), rep(14, 12))
  )
  #have names matching for merging
  names(new_items_L) <- names(PIRLS16_L)[c(1:3,9,16,18,32,34)]
  #merge
  PIRLS16_L = bind_rows(PIRLS16_L, new_items_L)
  
  # Create these clusters based on the appropriate condition
  if(L == 0){
    CB_cluster_matrix_L <-data.frame(matrix(  c(
      1,5,3, # 10,5,9,
      2,7,1, # 11,11,3,
      3,8,6, # 12,12,6,
      4,5,4, # 13,5,10,
      5,8,2, # 14,12,4,
      6,6,7, # 15,6,11,
      7,6,4, # 16,6,10,
      8,7,2, # 17,11,4,
      9,5,8  # 18,5,12
    ), ncol=3, byrow = T))
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_L) <- c("Book", "B1", "B2")
    #create book design 
    book_design_L = data.frame(matrix(c(
      0,0,1,0,1,0,0,0,
      1,0,0,0,0,0,1,0,
      0,0,0,0,0,1,0,1,
      0,0,0,1,1,0,0,0,
      0,1,0,0,0,0,0,1,
      0,0,0,0,0,1,1,0,
      0,0,0,1,0,1,0,0,
      0,1,0,0,0,0,1,0,
      0,0,0,0,1,0,0,1),
      ncol=8, byrow = T))
    # Subset passages
    PIRLS16_L = PIRLS16_L %>% filter(PassageID_rec %in% c(1:8))
    
    #PA for passage  
  }else if(L == 2){
    CB_cluster_matrix_L <-data.frame(matrix(  c(
      1,5,3, 
      2,7,1, 
      3,8,6,
      4,10,4, #4,5,4, 
      5,9,2,  #5,8,2, 
      6,6,7, 
      7,6,4, 
      8,7,2, 
      9,5,8,  
      10,10,9 #New booklet
    ), ncol=3, byrow = T))
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_L) <- c("Book", "B1", "B2")
    #create book design 
    book_design_L = data.frame(matrix(c(
      0,0,1,0,1,0,0,0,0,0,
      1,0,0,0,0,0,1,0,0,0,
      0,0,0,0,0,1,0,1,0,0,
      0,0,0,1,0,0,0,0,0,1,
      0,1,0,0,0,0,0,0,1,0,
      0,0,0,0,0,1,1,0,0,0,
      0,0,0,1,0,1,0,0,0,0,
      0,1,0,0,0,0,1,0,0,0,
      0,0,0,0,1,0,0,1,0,0,
      0,0,0,0,0,0,0,0,1,1),
      ncol=10, byrow = T))
    # Subset passages
    PIRLS16_L = PIRLS16_L %>% filter(PassageID_rec %in% c(1:10))
  }else if(L == 4){
    CB_cluster_matrix_L <-data.frame(matrix(  c(
      1,5,3, 
      2,11,1, ##2,7,1, 
      3,8,6,
      4,10,4, #4,5,4, 
      5,9,2,  #5,8,2, 
      6,6,7, 
      7,12,4,  ##7,6,4, 
      8,7,2, 
      9,5,8,  
      10,10,9,  #New booklet
      11,12,11 ##New booklet
    ), ncol=3, byrow = T))
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_L) <- c("Book", "B1", "B2")
    #create book design 
    book_design_L = data.frame(matrix(c(
      0,0,1,0,1,0,0,0,0,0,0,0,
      1,0,0,0,0,0,0,0,0,0,1,0,
      0,0,0,0,0,1,0,1,0,0,0,0,
      0,0,0,1,0,0,0,0,0,1,0,0,
      0,1,0,0,0,0,0,0,1,0,0,0,
      0,0,0,0,0,1,1,0,0,0,0,0,
      0,0,0,1,0,0,0,0,0,0,0,1,
      0,1,0,0,0,0,1,0,0,0,0,0,
      0,0,0,0,1,0,0,1,0,0,0,0,
      0,0,0,0,0,0,0,0,1,1,0,0,
      0,0,0,0,0,0,0,0,0,0,1,1),
      ncol=12, byrow = T))
    # Subset passages
    PIRLS16_L = PIRLS16_L %>% filter(PassageID_rec %in% c(1:12))
  }else if(L == 6){
    CB_cluster_matrix_L <-data.frame(matrix(  c(
      1,5,3, 
      2,11,1, ##2,7,1, 
      3,8,14, ###3,8,6,
      4,10,4, #4,5,4, 
      5,9,2,  #5,8,2, 
      6,6,7, 
      7,12,4,  ##7,6,4, 
      8,7,2, 
      9,5,8,  
      10,10,9,  #New booklet
      11,12,11, ##New booklet
      12,14,13,  ###New booklet
      13,6,13    ###New booklet
    ), ncol=3, byrow = T))
    
    #name cluster matrix  
    colnames(CB_cluster_matrix_L) <- c("Book", "B1", "B2")
    #create book design 
    book_design_L = data.frame(matrix(c(
      0,0,1,0,1,0,0,0,0,0,0,0,0,0,
      1,0,0,0,0,0,0,0,0,0,1,0,0,0,
      0,0,0,0,0,0,0,1,0,0,0,0,0,1,
      0,0,0,1,0,0,0,0,0,1,0,0,0,0,
      0,1,0,0,0,0,0,0,1,0,0,0,0,0,
      0,0,0,0,0,1,1,0,0,0,0,0,0,0,
      0,0,0,1,0,0,0,0,0,0,0,1,0,0,
      0,1,0,0,0,0,1,0,0,0,0,0,0,0,
      0,0,0,0,1,0,0,1,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,1,1,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,1,1,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,1,1,
      0,0,0,0,0,1,0,0,0,0,0,0,1,0),
      ncol=14, byrow = T))
    # Subset passages
    PIRLS16_L = PIRLS16_L %>% filter(PassageID_rec %in% c(1:14))
  }
  ## Extract CBA item-cluster-matrix (dummy coding)
  ## the items of only this group of passages
  PIRLS16_L$ItemID_gat = 1:nrow(PIRLS16_L)
  
  PIRLS16_L_items = PIRLS16_L[,c("Item", "ItemID_og", "ItemID_mod", "ItemID_gat",
                                 "PassageID_rec","passage_difficulty", "Cycle",
                                 "Slope (aj)", "Location (bj)"#,  "Guessing (cj)", 
                                 # "Step 1 (dj1)", "Step 2 (dj2)", "Step 3 (dj3)"
  )]
  
  
  PIRLS_item_cluster_matrix_L  = dummy_cols(PIRLS16_L_items, select_columns = "PassageID_rec")
  
  names(PIRLS_item_cluster_matrix_L) <- c("Item", "ItemID_og", "ItemID_mod","ItemID_gat",
                                          "PassageID","passage_difficulty", "Cycle",
                                          "Slope", "Location",  #"Guessing", 
                                          # "Step 1", "Step 2", "Step 3",
                                          paste0("PA",1:ncol(book_design_L))) 
  
  # For Harder Booklets
  ## Define item pool 
  #PIRLS16_L$ItemID = 1:nrow(PIRLS16_L)
  item_pool_L = PIRLS16_L[,c("ItemID_gat", "Slope (aj)", "Location (bj)")]
  
  row.names(item_pool_L) <- 1:nrow(item_pool_L)
  names(item_pool_L) <- c("item", "a","b")
  item_pool_L[is.na(item_pool_L)] = 0 # replacing all NAs to zero
  item_pool_L = data.frame(item_pool_L)
  
  
  ##Define item-cluster matrix
  item_block_L <- PIRLS_item_cluster_matrix_L[,paste0("PA", c(1:ncol(book_design_L)))]
  row.names(item_block_L) <- 1:nrow(item_block_L)
  colnames(item_block_L) <- paste0("PA", 1:ncol(book_design_L))
  
  ##Assign items to blocks/passages
  block_L <- lsasim::block_design(item_parameters = item_pool_L,
                                  item_block_matrix = item_block_L)
  
  ##Assign blocks to booklets
  book_L <- lsasim::booklet_design(item_block_assignment = block_L$block_assignment,
                                   book_design = book_design_L )
  
  #all the objects I want the function to return
  mylist = list(book_L = book_L,
                item_pool_L = item_pool_L,
                PIRLS16_L = PIRLS16_L,
                PIRLS_item_cluster_matrix_L = PIRLS_item_cluster_matrix_L,
                CB_cluster_matrix_L = CB_cluster_matrix_L)
  return(mylist)  
}