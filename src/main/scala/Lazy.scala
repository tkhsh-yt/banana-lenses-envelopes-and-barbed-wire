package object Lazy {

  type Thunk[A] = () => A

  def thunk[A](a: => A): () => A =
    () => a

  def eval[A](a: () => A): A =
    a()
}
