package object FuncionesRecursivas {

  def maxLin(l: List[Int]): Int = {

    def max(x: Int, y: Int): Int = if (x > y) x else y

    if (l.tail.isEmpty) l.head else max(l.head, maxLin(l.tail))

  // lineal recursiva
  }

  def maxIt(l: List[Int]): Int = {

    def max(x: Int, y: Int): Int = if (x > y) x else y

    def aux(mayor:Int,l: List[Int]):Int = {if (l.isEmpty) mayor else aux(max(mayor,l.head),l.tail) }

    aux(l.head, l.tail)
  }


}