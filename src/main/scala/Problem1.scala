object Problem1:
  @main def runProblem1() =
    getBestGroupPrices(Data.rates, Data.cabinPrices).foreach(println)

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

  case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
  )

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = 
    val groupPrices = for
      rate  <- rates
      price <- prices
      if rate.rateCode == price.rateCode
    yield BestGroupPrice(
      cabinCode = price.cabinCode, 
      rateCode  = price.rateCode, 
      price     = price.price, 
      rateGroup = rate.rateGroup
    )

    groupPrices
      .groupBy(gp => (gp.cabinCode, gp.rateGroup))
      .values
      .toList
      .map(_.minBy(_.price))
      .sortBy(_.price)
