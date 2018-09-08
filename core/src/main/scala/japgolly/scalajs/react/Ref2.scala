package japgolly.scalajs.react.ref

import japgolly.scalajs.react.{Ref => _, raw => Raw, _}
import japgolly.scalajs.react.internal.jsNullToOption
import scala.scalajs.js.|

trait Ref[O] {self =>

  val get: CallbackOption[O]

  def map[A](f: O => A): Ref[A] =
    new Ref[A] {
      override val get = self.get.map(f)
    }

  def widen[A >: O]: Ref[A] =
    map[A](o => o)

  final def foreach(f: O => Unit): Callback =
    foreachCB(a => Callback(f(a)))

  final def foreachCB(f: O => Callback): Callback =
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
  final def unsafeGet(): O =
    get.asCallback.runNow().getOrElse(sys error "Reference is empty")
}

object Ref {

  // TODO Rename
  trait Input[I] {
    val set: CallbackKleisli[Option[I], Unit]

    final lazy val rawSetFn: Raw.React.RefFn[I] =
      set.contramap[I | Null](jsNullToOption).toJsFn

    def contramap[A](f: A => I): Input[A]

    def narrow[A <: I]: Input[A] =
      contramap[A](a => a)
  }

  trait Fn[I, O] extends Input[I] with Ref[O] { self =>

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

  // TODO Create via React.createRef()
  final class Handle[A](val raw: Raw.React.RefHandle[A]) extends RawHandle[A] with Ref[A] {
    override val get = CallbackOption(CallbackTo(raw.current.toOption.flatMap(jsNullToOption)))

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