class Problem2Test extends munit.FunSuite:
  import Problem2.*

  test("allCombinablePromotions"):
    val result = allCombinablePromotions(Data.promotions)
    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )
    assertEquals(result, expected)

  test("combinablePromotions 1"):
    val result = combinablePromotions("P1", Data.promotions)
    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
    )
    assertEquals(result, expected)

  test("combinablePromotions 2"):
    val result = combinablePromotions("P3", Data.promotions)
    val expected = Seq(
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )
    assertEquals(result, expected)
