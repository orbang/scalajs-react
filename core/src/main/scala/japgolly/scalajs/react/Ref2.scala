package japgolly.scalajs.react.ref2

import japgolly.scalajs.react.{raw => Raw, Ref => _, _}
import japgolly.scalajs.react.internal.jsNullToOption
import scala.scalajs.js
import scala.scalajs.js.|
/*
trait Ref[A] { self =>

  val get: CallbackOption[A]

  def map[B](f: A => B): Ref[B] =
    new Ref[B] {
      override val get = self.get.map(f)
    }

  def widen[B >: A]: Ref[B] =
    map[B](o => o)

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

object Ref {

  trait SetFn[I] {
    val set: CallbackKleisli[Option[I], Unit]

    final lazy val rawSetFn: Raw.React.RefFn[I] =
      set.contramap[I | Null](jsNullToOption).toJsFn

    def contramap[A](f: A => I): Set[A]

    def narrow[A <: I]: Set[A] =
      contramap[A](a => a)
  }

  trait Fn[I, O] extends Set[I] with Ref[O] { self =>

    override def contramap[A](f: A => I): Fn[A, O] =
      new Fn[A, O] {
        override val get = self.get
        override val set = self.set.contramap[Option[A]](_ map f)
      }

    override def narrow[A <: I]: Fn[A, O] =
      contramap[A](a => a)

    override def map[A](f: O => A): Fn[I, A] =
      new Fn[I, A] {
        override val set = self.set
        override val get = self.get.map(f)
      }
  }

  def apply[A]: Fn[A, A] =
    new Fn[A, A] {
      private[this] var ref = Option.empty[A]
      override val get = CallbackOption(CallbackTo(ref))
      override val set = CallbackKleisli((r: Option[A]) => Callback {ref = r})
    }

  // ===================================================================================================================

  trait RawHandle[A] {
    val raw: Raw.React.RefHandle[A]
  }

  def fromJs[A](raw: Raw.React.RefHandle[A]) =
    Handle(raw)

  // TODO Not sure about this...
  def create[A] = fromJs(Raw.React.createRef[A]())

  // TODO Create via React.createRef()
  // TODO Rename to Js or JsHandle or something?
  final class Handle[A] private[Ref] (val raw: Raw.React.RefHandle[A]) extends RawHandle[A] with Ref[A] {
    override val get = CallbackOption(CallbackTo(jsNullToOption(raw.current)))
    override def map[B](f: A => B): Handle.Mapped[A, B] = new Handle.Mapped(this, f)
    override def widen[B >: A]: Handle.Mapped[A, B] = map[B](a => a)
  }

  object Handle {

    final class Mapped[A, B](val root: Handle[A], f: A => B) extends RawHandle[A] with Ref[B] {
      override val raw = root.raw
      override val get = root.get.map(f)
      override def map[C](g: B => C): Handle.Mapped[A, C] = new Handle.Mapped(root, g compose f)
      override def widen[C >: B]: Handle.Mapped[A, C] = map[C](b => b)
    }

  }

}
*/