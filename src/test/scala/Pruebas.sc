import Comparador . _
import scala.util.Random

val random = new Random ( )
def listaAlAzar(long:Int): List[Int] = {
  val v= Vector.fill( long){
    random.nextInt(long*2)+1
  }
  v.toList
}

def menorQue ( a: Int, b: Int) : Boolean = a < b
def mayorQue ( a: Int, b: Int) : Boolean = a > b

val l5 =listaAlAzar(5)
val l10 = listaAlAzar(10)
val l20 = listaAlAzar(20)
val l50 = listaAlAzar(50)


//InsertionSort Asc
val iSortAsc = insertionSort[Int](menorQue)

iSortAsc(List(4, 5, 6, 1, 2, 3, 2))
iSortAsc(l5)
iSortAsc(l10)
iSortAsc(l20)
iSortAsc(l50)


//Insertion sort Des
val iSortDes = insertionSort[Int](mayorQue)

iSortDes(List(4, 5, 6, 1, 2, 3, 2))
iSortDes(l5)
iSortDes(l10)
iSortDes(l20)
iSortDes(l50)


//QuickSort Des
val qSortDes = quickSort[Int](mayorQue)

qSortDes(List(4,5,6,1,2,3))
qSortDes(l5)
qSortDes(l10)
qSortDes(l20)
qSortDes(l50)


//QuickSort Asc
val qSortAsc = quickSort[Int](menorQue)

qSortAsc(List(4,5,6,1,2,3))
qSortAsc(l5)
qSortAsc(l10)
qSortAsc(l20)
qSortAsc(l50)


//Comparar
comparar(iSortAsc , qSortAsc, l5)
comparar(iSortAsc , qSortAsc, l10)
comparar(iSortAsc , qSortAsc, l20)
comparar(iSortAsc , qSortAsc, l50)








