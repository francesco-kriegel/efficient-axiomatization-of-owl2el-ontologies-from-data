package de.tu_dresden.inf.lat
package axiomatization


object NestedParallelComputations {

  val FORK_JOIN_POOL = java.util.concurrent.ForkJoinPool.commonPool()

  extension[T, CC[_], C](iterable: scala.collection.Iterable[T] with scala.collection.IterableOps[T, CC, C]) {

    /**
     * Apply f to each element in parallel for its side effects.
     * Note: [U] parameter needed to help scalac's type inference.
     */
    inline def foreachPar[U](f: T => U): Unit = {
      val tasks =
        iterable.map(t =>
          new java.util.concurrent.RecursiveAction() {
            override protected def compute(): Unit = {
              f(t)
            }
          }
        )
      tasks.foreach(FORK_JOIN_POOL.execute)
      tasks.foreach(_.quietlyJoin())
    }

//    private def par[U](f: T => U) : T => U = {
//      val tasks = scala.collection.mutable.HashMap[T, java.util.concurrent.RecursiveTask[U]]()
//      iterable.foreach(t =>
//        tasks(t) = () => {
//          f(t)
//        }
//      )
//      tasks.values.foreach(forkJoinPool.execute)
//      t => tasks(t).join()
//    }

    /** Builds a new collection by applying a function to all elements of this collection.
     *
     * @param f the function to apply to each element.
     * @tparam U the element type of the returned collection.
     * @return a new collection resulting from applying the given function
     *         `f` to each element of this collection and collecting the results.
     */
    inline def mapPar[U](f: T => U): CC[U] = {
      val tasks = scala.collection.mutable.HashMap[T, java.util.concurrent.RecursiveTask[U]]()
      iterable.foreach(t =>
        tasks(t) = () => { f(t) }
      )
      tasks.values.foreach(FORK_JOIN_POOL.execute)
      iterable.map(t => tasks(t).join())
//      iterable.map(par(f))
    }

    /** Builds a new collection by applying a function to all elements of this collection
     * and using the elements of the resulting collections.
     *
     *  @param f      the function to apply to each element.
     *  @tparam U     the element type of the returned collection.
     *  @return       a new collection resulting from applying the given collection-valued function
     *                `f` to each element of this collection and concatenating the results.
     */
    inline def flatMapPar[U](f: T => IterableOnce[U]): CC[U] = {
      val tasks = scala.collection.mutable.HashMap[T, java.util.concurrent.RecursiveTask[IterableOnce[U]]]()
      iterable.foreach(t =>
        tasks(t) = () => { f(t) }
      )
      tasks.values.foreach(FORK_JOIN_POOL.execute)
      iterable.flatMap(t => tasks(t).join())
//      iterable.flatMap(par(f))
    }

//    /** Applies a side-effecting function to each element in this collection.
//     * Strict collections will apply `f` to their elements immediately, while lazy collections
//     * like Views and LazyLists will only apply `f` on each element if and when that element
//     * is evaluated, and each time that element is evaluated.
//     *
//     * @param f a function to apply to each element in this collection
//     * @tparam U the return type of f
//     * @return The same logical collection as this
//     */
//    inline def tapEachPar[U](f: T => U): C = {
//      iterable.tapEach(par(f))
//    }

  }

}
