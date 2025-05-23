#BASE LAYER SMOOTH TRANSITIONS
    geom_tile(aes_string(x="long", y="lat", fill=var.name, key = "id"
                  ), showSelected="time2", color = NA, alpha = 1,
              data=climate)+ 
    #SELECTION LAYER     
    # geom_tile(aes_string(x="long", y="lat", key = "id"
    #               ),clickSelects = "id", color = "black", fill=NA, alpha = 0.4, showselected="time2",
    #           data=climate)+
    