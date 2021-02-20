


## Extract values from simulation, 


apt11 <- purrr::map_dbl(Int1, ~max(.x[,"I"])) %>% summary()


apt12 <- purrr::map_dbl(Int1, ~which.max(.x[,"I"])) %>% summary()


apt13 <- purrr::map_dbl(Int1, ~max(.x[,"E"])) %>% summary()


apt14 <- purrr::map_dbl(Int1, ~which.max(.x[,"E"])) %>% summary()


apt15 <- purrr::map_dbl(Int1, ~max(.x[,"H"])) %>% summary()


apt16 <- purrr::map_dbl(Int1, ~which.max(.x[,"H"])) %>% summary()


apt17 <- purrr::map_dbl(Int1, ~max(.x[,"U"])) %>% summary()


apt18 <- purrr::map_dbl(Int1, ~which.max(.x[,"U"])) %>% summary()


apt19 <- purrr::map_dbl(Int1, ~.x[366,"D"]) %>% summary()



## INTERVENTION 2 OUTPUT

apt21 <- purrr::map_dbl(Int2, ~max(.x[,"I"])) %>% summary()


apt22 <- purrr::map_dbl(Int2, ~which.max(.x[,"I"])) %>% summary()


apt23 <- purrr::map_dbl(Int2, ~max(.x[,"E"])) %>% summary()


apt24 <- purrr::map_dbl(Int2, ~which.max(.x[,"E"])) %>% summary()


apt25 <- purrr::map_dbl(Int2, ~max(.x[,"H"])) %>% summary()


apt26 <- purrr::map_dbl(Int2, ~which.max(.x[,"H"])) %>% summary()


apt27 <- purrr::map_dbl(Int2, ~max(.x[,"U"])) %>% summary()


apt28 <- purrr::map_dbl(Int2, ~which.max(.x[,"U"])) %>% summary()


apt29 <- purrr::map_dbl(Int2, ~.x[366,"D"]) %>% summary()



##############################################################
## INTERVENTION 3 OUTPUT
##############################################################

apt31 <- purrr::map_dbl(Int3, ~max(.x[,"I"])) %>% summary()


apt32 <- purrr::map_dbl(Int3, ~which.max(.x[,"I"])) %>% summary()


apt33 <- purrr::map_dbl(Int3, ~max(.x[,"E"])) %>% summary()


apt34 <- purrr::map_dbl(Int3, ~which.max(.x[,"E"])) %>% summary()


apt35 <- purrr::map_dbl(Int3, ~max(.x[,"H"])) %>% summary()


apt36 <- purrr::map_dbl(Int3, ~which.max(.x[,"H"])) %>% summary()


apt37 <- purrr::map_dbl(Int3, ~max(.x[,"U"])) %>% summary()


apt38 <- purrr::map_dbl(Int3, ~which.max(.x[,"U"])) %>% summary()


apt39 <- purrr::map_dbl(Int3, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 4 OUTPUT
##############################################################

apt41 <- purrr::map_dbl(Int4, ~max(.x[,"I"])) %>% summary()


apt42 <- purrr::map_dbl(Int4, ~which.max(.x[,"I"])) %>% summary()


apt43 <- purrr::map_dbl(Int4, ~max(.x[,"E"])) %>% summary()


apt44 <- purrr::map_dbl(Int4, ~which.max(.x[,"E"])) %>% summary()


apt45 <- purrr::map_dbl(Int4, ~max(.x[,"H"])) %>% summary()


apt46 <- purrr::map_dbl(Int4, ~which.max(.x[,"H"])) %>% summary()


apt47 <- purrr::map_dbl(Int4, ~max(.x[,"U"])) %>% summary()


apt48 <- purrr::map_dbl(Int4, ~which.max(.x[,"U"])) %>% summary()


apt49 <- purrr::map_dbl(Int4, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 5 OUTPUT
##############################################################

apt51 <- purrr::map_dbl(Int5, ~max(.x[,"I"])) %>% summary()


apt52 <- purrr::map_dbl(Int5, ~which.max(.x[,"I"])) %>% summary()


apt53 <- purrr::map_dbl(Int5, ~max(.x[,"E"])) %>% summary()


apt54 <- purrr::map_dbl(Int5, ~which.max(.x[,"E"])) %>% summary()


apt55 <- purrr::map_dbl(Int5, ~max(.x[,"H"])) %>% summary()


apt56 <- purrr::map_dbl(Int5, ~which.max(.x[,"H"])) %>% summary()


apt57 <- purrr::map_dbl(Int5, ~max(.x[,"U"])) %>% summary()


apt58 <- purrr::map_dbl(Int5, ~which.max(.x[,"U"])) %>% summary()


apt59 <- purrr::map_dbl(Int5, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 6 OUTPUT
##############################################################

apt61 <- purrr::map_dbl(Int6, ~max(.x[,"I"])) %>% summary()


apt62 <- purrr::map_dbl(Int6, ~which.max(.x[,"I"])) %>% summary()


apt63 <- purrr::map_dbl(Int6, ~max(.x[,"E"])) %>% summary()


apt64 <- purrr::map_dbl(Int6, ~which.max(.x[,"E"])) %>% summary()


apt65 <- purrr::map_dbl(Int6, ~max(.x[,"H"])) %>% summary()


apt66 <- purrr::map_dbl(Int6, ~which.max(.x[,"H"])) %>% summary()


apt67 <- purrr::map_dbl(Int6, ~max(.x[,"U"])) %>% summary()


apt68 <- purrr::map_dbl(Int6, ~which.max(.x[,"U"])) %>% summary()


apt69 <- purrr::map_dbl(Int6, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 7 OUTPUT
##############################################################

apt71 <- purrr::map_dbl(Int7, ~max(.x[,"I"])) %>% summary()


apt72 <- purrr::map_dbl(Int7, ~which.max(.x[,"I"])) %>% summary()


apt73 <- purrr::map_dbl(Int7, ~max(.x[,"E"])) %>% summary()


apt74 <- purrr::map_dbl(Int7, ~which.max(.x[,"E"])) %>% summary()


apt75 <- purrr::map_dbl(Int7, ~max(.x[,"H"])) %>% summary()


apt76 <- purrr::map_dbl(Int7, ~which.max(.x[,"H"])) %>% summary()


apt77 <- purrr::map_dbl(Int7, ~max(.x[,"U"])) %>% summary()


apt78 <- purrr::map_dbl(Int7, ~which.max(.x[,"U"])) %>% summary()


apt79 <- purrr::map_dbl(Int7, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 8 OUTPUT
##############################################################

apt81 <- purrr::map_dbl(Int8, ~max(.x[,"I"])) %>% summary()


apt82 <- purrr::map_dbl(Int8, ~which.max(.x[,"I"])) %>% summary()


apt83 <- purrr::map_dbl(Int8, ~max(.x[,"E"])) %>% summary()


apt84 <- purrr::map_dbl(Int8, ~which.max(.x[,"E"])) %>% summary()


apt85 <- purrr::map_dbl(Int8, ~max(.x[,"H"])) %>% summary()


apt86 <- purrr::map_dbl(Int8, ~which.max(.x[,"H"])) %>% summary()


apt87 <- purrr::map_dbl(Int8, ~max(.x[,"U"])) %>% summary()


apt88 <- purrr::map_dbl(Int8, ~which.max(.x[,"U"])) %>% summary()


apt89 <- purrr::map_dbl(Int8, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 9 OUTPUT
##############################################################

apt91 <- purrr::map_dbl(Int9, ~max(.x[,"I"])) %>% summary()


apt92 <- purrr::map_dbl(Int9, ~which.max(.x[,"I"])) %>% summary()


apt93 <- purrr::map_dbl(Int9, ~max(.x[,"E"])) %>% summary()


apt94 <- purrr::map_dbl(Int9, ~which.max(.x[,"E"])) %>% summary()


apt95 <- purrr::map_dbl(Int9, ~max(.x[,"H"])) %>% summary()


apt96 <- purrr::map_dbl(Int9, ~which.max(.x[,"H"])) %>% summary()


apt97 <- purrr::map_dbl(Int9, ~max(.x[,"U"])) %>% summary()


apt98 <- purrr::map_dbl(Int9, ~which.max(.x[,"U"])) %>% summary()


apt99 <- purrr::map_dbl(Int9, ~.x[366,"D"]) %>% summary()



##############################################################
## INTERVENTION 10 OUTPUT
##############################################################

aptA1 <- purrr::map_dbl(Int10, ~max(.x[,"I"])) %>% summary()


aptA2 <- purrr::map_dbl(Int10, ~which.max(.x[,"I"])) %>% summary()


aptA3 <- purrr::map_dbl(Int10, ~max(.x[,"E"])) %>% summary()


aptA4 <- purrr::map_dbl(Int10, ~which.max(.x[,"E"])) %>% summary()


aptA5 <- purrr::map_dbl(Int10, ~max(.x[,"H"])) %>% summary()


aptA6 <- purrr::map_dbl(Int10, ~which.max(.x[,"H"])) %>% summary()


aptA7 <- purrr::map_dbl(Int10, ~max(.x[,"U"])) %>% summary()


aptA8 <- purrr::map_dbl(Int10, ~which.max(.x[,"U"])) %>% summary()


aptA9 <- purrr::map_dbl(Int10, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 11 OUTPUT
##############################################################

aptB1 <- purrr::map_dbl(Int11, ~max(.x[,"I"])) %>% summary()


aptB2 <- purrr::map_dbl(Int11, ~which.max(.x[,"I"])) %>% summary()


aptB3 <- purrr::map_dbl(Int11, ~max(.x[,"E"])) %>% summary()


aptB4 <- purrr::map_dbl(Int11, ~which.max(.x[,"E"])) %>% summary()


aptB5 <- purrr::map_dbl(Int11, ~max(.x[,"H"])) %>% summary()


aptB6 <- purrr::map_dbl(Int11, ~which.max(.x[,"H"])) %>% summary()


aptB7 <- purrr::map_dbl(Int11, ~max(.x[,"U"])) %>% summary()


aptB8 <- purrr::map_dbl(Int11, ~which.max(.x[,"U"])) %>% summary()


aptB9 <- purrr::map_dbl(Int11, ~.x[366,"D"]) %>% summary()

##############################################################
## INTERVENTION 12 OUTPUT
##############################################################

aptC1 <- purrr::map_dbl(Int12, ~max(.x[,"I"])) %>% summary()


aptC2 <- purrr::map_dbl(Int12, ~which.max(.x[,"I"])) %>% summary()


aptC3 <- purrr::map_dbl(Int12, ~max(.x[,"E"])) %>% summary()


aptC4 <- purrr::map_dbl(Int12, ~which.max(.x[,"E"])) %>% summary()


aptC5 <- purrr::map_dbl(Int12, ~max(.x[,"H"])) %>% summary()


aptC6 <- purrr::map_dbl(Int12, ~which.max(.x[,"H"])) %>% summary()


aptC7 <- purrr::map_dbl(Int12, ~max(.x[,"U"])) %>% summary()


aptC8 <- purrr::map_dbl(Int12, ~which.max(.x[,"U"])) %>% summary()


aptC9 <- purrr::map_dbl(Int12, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 13 OUTPUT
##############################################################

aptD1 <- purrr::map_dbl(Int13, ~max(.x[,"I"])) %>% summary()


aptD2 <- purrr::map_dbl(Int13, ~which.max(.x[,"I"])) %>% summary()


aptD3 <- purrr::map_dbl(Int13, ~max(.x[,"E"])) %>% summary()


aptD4 <- purrr::map_dbl(Int13, ~which.max(.x[,"E"])) %>% summary()


aptD5 <- purrr::map_dbl(Int13, ~max(.x[,"H"])) %>% summary()


aptD6 <- purrr::map_dbl(Int13, ~which.max(.x[,"H"])) %>% summary()


aptD7 <- purrr::map_dbl(Int13, ~max(.x[,"U"])) %>% summary()


aptD8 <- purrr::map_dbl(Int13, ~which.max(.x[,"U"])) %>% summary()


aptD9 <- purrr::map_dbl(Int13, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 14 OUTPUT
##############################################################

aptE1 <- purrr::map_dbl(Int14, ~max(.x[,"I"])) %>% summary()


aptE2 <- purrr::map_dbl(Int14, ~which.max(.x[,"I"])) %>% summary()


aptE3 <- purrr::map_dbl(Int14, ~max(.x[,"E"])) %>% summary()


aptE4 <- purrr::map_dbl(Int14, ~which.max(.x[,"E"])) %>% summary()


aptE5 <- purrr::map_dbl(Int14, ~max(.x[,"H"])) %>% summary()


aptE6 <- purrr::map_dbl(Int14, ~which.max(.x[,"H"])) %>% summary()


aptE7 <- purrr::map_dbl(Int14, ~max(.x[,"U"])) %>% summary()


aptE8 <- purrr::map_dbl(Int14, ~which.max(.x[,"U"])) %>% summary()


aptE9 <- purrr::map_dbl(Int14, ~.x[366,"D"]) %>% summary()


##############################################################
## INTERVENTION 15 OUTPUT
##############################################################

aptF1 <- purrr::map_dbl(Int15, ~max(.x[,"I"])) %>% summary()


aptF2 <- purrr::map_dbl(Int15, ~which.max(.x[,"I"])) %>% summary()


aptF3 <- purrr::map_dbl(Int15, ~max(.x[,"E"])) %>% summary()


aptF4 <- purrr::map_dbl(Int15, ~which.max(.x[,"E"])) %>% summary()


aptF5 <- purrr::map_dbl(Int15, ~max(.x[,"H"])) %>% summary()


aptF6 <- purrr::map_dbl(Int15, ~which.max(.x[,"H"])) %>% summary()


aptF7 <- purrr::map_dbl(Int15, ~max(.x[,"U"])) %>% summary()


aptF8 <- purrr::map_dbl(Int15, ~which.max(.x[,"U"])) %>% summary()


aptF9 <- purrr::map_dbl(Int15, ~.x[366,"D"]) %>% summary()


## create a matrix lists  


apt <- rbind(apt11, apt12, apt13, apt14, apt15, apt16, apt17, apt18, apt19,
             apt21, apt22, apt23, apt24, apt25, apt26, apt27, apt28, apt29,
             apt31, apt32, apt33, apt34, apt35, apt36, apt37, apt38, apt39,
             apt41, apt42, apt43, apt44, apt45, apt46, apt47, apt48, apt49,
             apt51, apt52, apt53, apt54, apt55, apt56, apt57, apt58, apt59,
             apt61, apt62, apt63, apt64, apt65, apt66, apt67, apt68, apt69,
             apt71, apt72, apt73, apt74, apt75, apt76, apt77, apt78, apt79,
             apt81, apt82, apt83, apt84, apt85, apt86, apt87, apt88, apt89,
             apt91, apt92, apt93, apt94, apt95, apt96, apt97, apt98, apt99,
             aptA1, aptA2, aptA3, aptA4, aptA5, aptA6, aptA7, aptA8, aptA9,
             aptB1, aptB2, aptB3, aptB4, aptB5, aptB6, aptB7, aptB8, aptB9,
             aptC1, aptC2, aptC3, aptC4, aptC5, aptC6, aptC7, aptC8, aptC9,
             aptD1, aptD2, aptD3, aptD4, aptD5, aptD6, aptD7, aptD8, aptD9,
             aptE1, aptE2, aptE3, aptE4, aptE5, aptE6, aptE7, aptE8, aptE9,
             aptF1, aptF2, aptF3, aptF4, aptF5, aptF6, aptF7, aptF8, aptF9 ) 
