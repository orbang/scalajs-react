package japgolly.scalajs.react.ref3

import japgolly.scalajs.react.{raw => Raw, Ref => _, _}
import japgolly.scalajs.react.internal.{identityFn, jsNullToOption}
import scala.scalajs.js.|

// TODO No root Ref[?] type?

object Ref {

  trait Get[A] { self =>
    val get: CallbackOption[A]

    def map[B](f: A => B): Get[B]

    def widen[B >: A]: Get[B]

    final def foreach(f: A => Unit): Callback =
      foreachCB(a => Callback(f(a)))

    final def foreachCB(f: A => Callback): Callback =
      get.flatMapCB(f).toCallback

    /** Get the reference immediately.
      *
      * ONLY USE THIS IN UNIT TESTS. DO NOT USE THIS IN PRODUCTION CODE.
      *
      * Unsafe for two reasons:
      *
      * 1. It reads an underlying variable. (impurity)
      * 2. It throws an exception when the ref is empty (partiality)
      */
    final def unsafeGet(): A =
      get.asCallback.runNow().getOrElse(sys error "Reference is empty")
  }

  trait Set[A] {
    val set: CallbackKleisli[Option[A], Unit]

    final lazy val rawSetFn: Raw.React.RefFn[A] =
      set.contramap[A | Null](jsNullToOption).toJsFn

    def contramap[B](f: B => A): Set[B]

    def narrow[B <: A]: Set[B]
  }

  trait Handle[A] {
    val raw: Raw.React.RefHandle[A]
  }

  trait Fn[I, O] extends Set[I] with Get[O] {
    override def contramap[X](f: X => I): Fn[X, O]
    override def narrow[X <: I]: Fn[X, O]
    override def map[X](f: O => X): Fn[I, X]
    override def widen[X >: O]: Fn[I, X]
  }

  final class Full[I, A, O](val raw: Raw.React.RefHandle[A], l: I => A, r: A => O) extends Handle[A] with Fn[I, O] {

    def root: Simple[A] =
      fromJs(raw)

    override val set: CallbackKleisli[Option[I], Unit] =
      CallbackKleisli((oi: Option[I]) => Callback(raw.current = oi match {
        case Some(i) => l(i)
        case None    => null
      }))

    override val get: CallbackOption[O] =
      CallbackOption(CallbackTo(jsNullToOption(raw.current).map(r)))

    override def contramap[X](f: X => I): Full[X, A, O] =
      new Full(raw, l compose f, r)

    override def narrow[X <: I]: Full[X, A, O] =
      new Full(raw, l, r)

    override def map[X](f: O => X): Full[I, A, X] =
      new Full(raw, l, f compose r)

    override def widen[X >: O]: Full[I, A, X] =
      new Full(raw, l, r)
  }

  type Simple[A] = Full[A, A, A]

  def fromJs[A](raw: Raw.React.RefHandle[A]): Simple[A] =
    new Full(raw, identityFn, identityFn)

  def apply[A]: Simple[A] =
    fromJs(Raw.React.createRef[A]())

  // TODO profunctor instances in scalaz/cats modules
}