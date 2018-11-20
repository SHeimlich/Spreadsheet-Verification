
// AUTOMATICALLY GENERATED by sbt-rats - EDIT AT YOUR OWN RISK

package sbtrats

trait Action[T] {
    def run (arg : T) : T
}

object SList {
    type L[T] = scala.collection.immutable.List[T]
    def empty[T] : L[T] = Nil
    def create[T] (hd : T) : L[T] = hd :: Nil
    def create[T] (hd : T, nxt : T) : L[T] = hd :: nxt :: Nil
    def create[T] (hd : T, tl : L[T]) : L[T] = hd :: tl
    def reverse[T] (l : L[T]) : L[T] = l.reverse
}

object SVector {
    type L[T] = scala.collection.immutable.Vector[T]
    def empty[T] : L[T] = Vector ()
    def create[T] (hd : T) : L[T] = hd +: Vector ()
    def create[T] (hd : T, nxt : T) : L[T] = hd +: nxt +: Vector ()
    def create[T] (hd : T, tl : L[T]) : L[T] = hd +: tl
    def reverse[T] (l : L[T]) : L[T] = l.reverse
}

object ParserSupport {

    import scala.language.higherKinds

    def apply[T] (actions : Seq[Action[T]], seed : T) : T =
        actions.foldLeft (seed) {
            case (result, action) =>
                action.run (result)
        }

}

