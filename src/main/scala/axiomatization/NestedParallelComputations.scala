package de.tu_dresden.inf.lat
package axiomatization


object NestedParallelComputations {

  extension[T, CC[_], C](iterable: scala.collection.Iterable[T] with scala.collection.IterableOps[T, CC, C]) {

    /**
     * Apply f to each element in parallel for its side effects.
     * Note: [U] parameter needed to help scalac's type inference.
     */
    inline def foreachPar[U](f: T => U): Unit = {
      val forkJoinTasks = iterable.map { t => ({ () => f(t) }: java.util.concurrent.RecursiveAction).fork() }
      forkJoinTasks.foreach { _.join() }
    }

    private def par[U](f: T => U): T => U = {
      val forkJoinTasks = scala.collection.mutable.HashMap[T, java.util.concurrent.ForkJoinTask[U]]()
      iterable.foreach { t => forkJoinTasks(t) = ({ () => f(t) }: java.util.concurrent.RecursiveTask[U]).fork() }
      forkJoinTasks(_).join()
    }

    /** Builds a new collection by applying a function to all elements of this collection.
     *
     * @param f the function to apply to each element.
     * @tparam U the element type of the returned collection.
     * @return a new collection resulting from applying the given function
     *         `f` to each element of this collection and collecting the results.
     */
    inline def mapPar[U](f: T => U): CC[U] = {
      iterable.map(par(f))
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
      iterable.flatMap(par(f))
    }

  }

}
