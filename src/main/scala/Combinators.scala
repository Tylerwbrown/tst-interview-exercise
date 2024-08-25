extension [A](xss: Seq[Seq[A]]) 
  def distinctByPeerSuperset: Seq[Seq[A]] =
    xss.filterNot: xs =>
      xss.filter(ys => xs.forall(ys.contains)).length > 1
