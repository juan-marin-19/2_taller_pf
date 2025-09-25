package object Comparador {
    type AlgoritmoOrd[T] = List[T] => (List[T], Int)
    type Comparador[T] = (T, T) => Boolean

    def insert[T](e: T, l: List[T], comp: Comparador[T]): (List[T], Int) = {
        if (l.isEmpty) {
            (e :: List.empty[T], 0) // Caso base: lista vacía, insertar e al inicio, 0 comparaciones.
        } else if (comp(e, l.head)) {
            (e :: l, 1) // e debe ir antes que head (comparación exitosa), insertar aquí, contar 1 comparación.
        } else {
            val (sortedTail, count) = insert(e, l.tail, comp) // Recursión: no insertar aquí, avanzar a la cola.
            (l.head :: sortedTail, count + 1) // Reconstruir lista con head al frente, sumar 1 comparación fallida.
        }
    }


    def insertionSort[T](comp: Comparador[T]): AlgoritmoOrd[T] = {
        def sort(l: List[T]): (List[T], Int) = {
            if (l.isEmpty) {
                (List.empty[T], 0) // Caso base: lista vacía, 0 comparaciones.
            } else {
                val (sortedTail, countTail) = sort(l.tail) // Recursión: ordenar la cola.
                val (inserted, countInsert) = insert(l.head, sortedTail, comp) // Insertar cabeza en la cola ordenada.
                (inserted, countTail + countInsert) // Sumar conteos totales.
            }
        }

        sort // Devolver la función sort como resultado.
    }


    def menoresQue_noMenoresQue[T](l: List[T], v: T, comp: Comparador[T]): (List[T], List[T], Int) = {
        if (l.isEmpty) {
            (Nil, Nil, 0)
        } else {
          val head = l.head
          val tail = l.tail
          val (menoresQue, noMenoresQue, comps) = menoresQue_noMenoresQue(tail, v, comp)

          if (comp(head, v)) {
            (head :: menoresQue, noMenoresQue, comps + 1)
          } else {
            (menoresQue, head :: noMenoresQue, comps + 1)
          }

        }
    }


    def quickSort[T](comp: Comparador[T]): AlgoritmoOrd[T] = {
        def QuickSortAlt(l: List[T]): (List[T], Int) = {
            if (l.isEmpty || l.tail.isEmpty) (l, 0)
            else {
                val pivote = l.head
                val tail = l.tail

                val (menoresQue, noMenoresQue, compsPartes) =
                    menoresQue_noMenoresQue(tail, pivote, comp)
                val (menoresOrdenados, compMenores) = QuickSortAlt(menoresQue)
                val (noMenoresOrdenados, compNoMenores) = QuickSortAlt(noMenoresQue)


                (menoresOrdenados ++ List(pivote) ++ noMenoresOrdenados, compsPartes + compMenores + compNoMenores)
            }
        }

        QuickSortAlt

    }


    def comparar[T](a1: AlgoritmoOrd[T], a2:AlgoritmoOrd[T], l:List[T]): (Int, Int) = {
      val (l1, c1) = a1(l)
      val (l2,c2) = a2(l)
      if (l1==l2) (c1,c2) else (-1,-1)
    }




}