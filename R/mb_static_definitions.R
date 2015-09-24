valid_currencies   <- c("EUR","USD","GBP","CAD","IDR","CNY","JPY","AUD","HKD")
valid_market_states<- c("suspended","open")
valid_odds_types   <- c("US","DECIMAL","%","HK","MALAY","INDO","FRAC")
valid_languages    <- c("en","de","es","fr","hi","id","ja","ru","th","zh","zh_HANS","zh_HANT")

mb_odds_ladder <- c(
  seq(100,199,by=1),
  seq(200,298,by=2),
  seq(300,395,by=5),
  seq(400,650,by=10),
  seq(660,980,by=20),
  seq(1000,1950,by=50),
  seq(2000,2900,by=100),
  seq(3000,3800,by=200),
  seq(4000,6500,by=500),
  seq(7000,9000,by=1000),
  seq(10000,20000,by=5000),225, 275, 750, 850, 950, 30000, 50000,99999)
